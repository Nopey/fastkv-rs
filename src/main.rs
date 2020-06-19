#![feature(const_transmute)]
use persist_o_vec::Persist;

// START fastkv.h
#[derive(Debug)]
enum Item {
    // DIFF: removed Error variant, using Option instead.
    String(*const u8),
    //DIFF: Length and size is stored as usize (uint64_t rather than uint32_t)
    // (makes Item take up 32 bytes :c)
    Object(Persist<Pair>)
}

#[derive(Debug)]
struct Pair {
    // DIFF: key cannot be an object or an error.
    key: *const u8,
    value: Item,

}

struct Vars (Vec<*const u8>);
// END fastkv.h

// START fastkv.c
use std::mem::transmute;
const SLASH_SLASH: u16 = unsafe{transmute([b'/', b'/'])};
const SLASH_STAR:  u16 = unsafe{transmute([b'/', b'*'])};
const STAR_SLASH:  u16 = unsafe{transmute([b'/', b'*'])};

//DIFF: I'm returning the new pointer rather than modifying the int64 i stuff, because that seems silly.

#[inline]
unsafe fn skipws(mut text: *mut u8) -> *mut u8 {
    loop{ // start
        // NOTE: There's definitely a prettier way to do these loops
        // but its nighttime, and I've got 18 hours to sleep and finish this (ridiculous) port.
        let sp = |c| c!=b'\0' && c<=b' ';
        while sp(text.read())
            {text = text.offset(1);}
        let read = (text as *const u16).read_unaligned();
        if read == SLASH_SLASH {
            // got a comment //
            text = text.offset(2);
            
            let nnl = |c| c!=b'\0' && c!=b'\n';
            while nnl(text.read())
                {text = text.offset(1);}
        }else if read == SLASH_STAR {
            // got a comment /*
            text = text.offset(2);
            loop{
                // Error handling? Who gives a crap about Error handling! -Scout, on being fast.
                let read = (text as *const u16).read_unaligned();
                if read == STAR_SLASH {
                    break;
                }
                text = text.offset(1);
            }
        }else{
            return text;
        }
    }
}

/// DIFF: Returns Type tagged enum rather than Item struct.
/// (with inlining, the unused field would get optimized away anyways, but whatever)
#[inline]
unsafe fn parsestring(mut text: *mut u8) -> (*mut u8, *const u8) {
    let inner = if text.read() == b'"' {
        text = text.offset(1);
        let inner= text;
        let good = |c| c!=b'\0' && c!=b'"';
        while good(text.read())
            {text = text.offset(1);}
        inner
    }else{
        let inner= text;
        let good = |read| (read > b'0' && read <= b'Z') ||
			   (read >= b'a' && read <= b'z') || (read == b'_');
        while good(text.read()){
            text = text.offset(1);
        }
        inner
    };

	// abuse the fact that strings cannot be followed directly by other strings
	text.write(b'\0');
	text = text.offset(1);

	// dbgf("parsed string: '%s'\n", str.string);

	(text, inner)
}

#[cfg(feature = "conditions")]
#[inline]
unsafe fn term(mut text: *mut u8, defs: &Vars) -> (*mut u8, bool) {
    let negate = if text.read() == b'!' {
        text = skipws(text);
        true
    }else{ false };

    if text.read() == b'$'{
        text = text.offset(1);
        let (rest, item) = parsestring(text);
        text = rest;
        for var in defs.0.iter(){
            // libc, why not
            if libc::strcmp(*var as *const i8, item as *const i8) == 0{
                return (text, !negate);
            }
        }
    }

    (text, negate)
}

#[cfg(feature = "conditions")]
#[inline]
unsafe fn expr(mut text: *mut u8, defs: &Vars) -> (*mut u8, bool) {
    text = skipws(text);

    let (rest, first) = term(text, defs);
    text = rest;
    text = skipws(text);
    let read = text.read();
    if read == b'&' {
        while text.read() == b'&' { text = text.offset(1); }

		text = skipws(text);
		let (rest, second) = expr(text, defs);
        text = rest;
		(text, first && second)
    }else if read == b'|' {
        while text.read() == b'|' { text = text.offset(1); }

		text = skipws(text);
		let (rest, second) = expr(text, defs);
        text = rest;
		(text, first || second)
    }else{
        (text, first)
    }
}

#[cfg(feature = "conditions")]
#[inline]
unsafe fn parsecond(mut text: *mut u8, defs: &Vars) -> (*mut u8, bool) {
    text = skipws(text);
	// dbgf("Parsing condition, skipped ws, at '%.2s'\n", text + *i);
    if text.read() == b'[' {
        let (rest, res) = expr(text, defs);
        text = rest;
        text = skipws(text);
        if text.read() != b']' {
            eprintln!("Expected a ]\n");
        }else{
            text = text.offset(1);
        }
        (text, res)
    }else{
        (text, true)
    }
}


#[cfg(not(feature = "conditions"))]
#[inline]
unsafe fn parsecond(text: *mut u8, _vars: &Vars) -> (*mut u8, bool) {
    (text, true)
}

// DIFF: I'm not gonna bother with the length arg and bounds checking
// just the nullptr check should be enough, at least for drag racing.
unsafe fn kv_parse(mut text: *mut u8, defs: &Vars) -> (*mut u8, Item) {
    // this is the biggest impact on optimization. 2-8 gives the best
	// results on most files. Anything over 64 is almost certain to slow
	// down the kv_parse
    const OBJ_GUESS: usize = 8;
    let mut object = Persist::with_capacity(OBJ_GUESS);
    
    text = skipws(text);

    while text.read() != 0 {
		if text.read() == b'}' {
			text = text.offset(1);
			// dbgf("returning at }, '%s', %ld\n", text + *i, *i);
			return (text, Item::Object(object));
		}
		// dbgf("continuing loop with %c (%d), i: %ld\n", text.read(), text.read(), *i);
		// dbgf("got key, at '%s'\n", text + *i);
		let (rest, key) = parsestring(text);
        text = rest;

		text = skipws(text);

		// dbgf("Peeking char %c at i: %ld\n", text.read(), *i);

		let value = if text.read() == b'{' { // sub object
			text = text.offset(1);
			// dbgf("got an object at %c, i(++) %ld\n", text.read(), *i);
			let (rest, value) = kv_parse(text, defs);
            text = rest;
            value
			// dbgf("the object returned\n");
		} else {
			let (rest, value) = parsestring(text);
            text = rest;
            // dbgf("it was '%s'\n", object.object[object.length].value.string);
            Item::String(value as *const u8)
		};

		// dbgf("after sub-object, at: '%s', %ld\n", text + *i, *i);

		let (rest, cond) = parsecond(text, defs);
        text = rest;

		if cond {
			object.push(Pair{key, value});
		}

		// dbgf("[] checking cond: %d\n", cond);
		// dbgf("Finished [] at %ld\n", *i);

		text = skipws(text);

		// dbgf("starting next loop with %c (%d), i: %ld\n", text.read(), text.read(), *i);
    }

    (text, Item::Object(object))
}

impl Item{
    unsafe fn printitem(&self, depth: u32) {
        //DIFF: I don't care how ugly this function is, i don't want to spend any more time on it.
        match self{
            Item::String(s) => {
                libc::printf("\t\"%s\"\n\0".as_ptr() as *const _, *s);
            },
            Item::Object(o) => {
                // dont print {} for top level objects
                if depth > 0 { libc::printf("{ // %d items\n\0".as_ptr() as *const _, o.len()); }
                
                for current in o.iter() {
                    for _ in 0..depth { libc::printf("\t\0".as_ptr() as *const _); }
                    libc::printf("\"%s\" \0".as_ptr() as *const _, current.key);
                    current.value.printitem(depth + 1);
                }
                
                if depth > 0 {
                    for _ in 0..depth { libc::printf(b"\t\0".as_ptr() as *const _); }
                    libc::printf("}\n\0".as_ptr() as *const _);
                }
            }
        }
    }
    unsafe fn get(&self, q: *const u8) -> Option<&Item> {
        if let Item::Object(v) = self{
            for current in v.iter() {
                if libc::strcmp(current.key as *const i8, q as *const i8) == 0 {
                    return Some(&current.value);
                }
            }
        }

        None
    }

    unsafe fn query(&self, qq: *const u8) -> Option<&Item> {
        let mut current = Some(self);

        let mut q = libc::strdup(qq as *const _) as *mut u8;
        let q_start = q;
        let mut start = q;

        if q.read() == b'.' {
            q.write(0);
            start = q.offset(1);
        }

        while q.read() != 0 {
            if q.read() == b'.' {
                q.write(0);
                current = current.and_then( |current| current.get(start));

                start = q.offset(1);
            }
            q = q.offset(1);
        }
        if q > start {
            current = current.and_then( |current| current.get(start));
        }

        libc::free(q_start as *mut _);

        current
    }
}

// END fastkv.c

// START main.c
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

const HELP: &'static str =
"FastKV-rs help
Originally Written by swissChili,
Butchered by Nopey

Usage:
\tfastkv-rs [file] [query]
\t<file>\tThe file to kv_parse
\t<query>\tThe query. Format key.subkey.value";

fn main() {
    let mut args = std::env::args().skip(1);
    let (file, query) = if let Some(file) = args.next(){
        if file=="-h" || file == "--help" {
            println!("{}", HELP);
            return;
        }
        (file, args.next())
    }else{
        println!("Expected at least one argument. Try --help?");
        return;
    };

    use std::fs::File;
    use std::io::Read;
    let mut file = File::open(file).expect("Couldn't open input file!");
    let mut text = vec![];
    file.read_to_end(&mut text).expect("Couldn't read from input file? odd.");
    text.push(0);
    
    // just wrap this entire program in an unsafe block :)
    unsafe {
        let q = query.map(|q| std::ffi::CString::new(q).unwrap());
        let defines = Vars(vec![b"X64\0".as_ptr(), b"X86_64\0".as_ptr(), b"LINUX\0".as_ptr()]);

        // rust nerds are crying at this display of uninitialized memory.
        let mut start   = libc::timeval{tv_sec: 0, tv_usec: 0};
        let mut end     = libc::timeval{tv_sec: 0, tv_usec: 0};
        use std::ptr::null_mut;
	    libc::gettimeofday(&mut start, null_mut());
        let (_, parsed) = kv_parse(text.as_mut_ptr(),&defines);
        libc::gettimeofday(&mut end, null_mut());
        if let Some(q) = q {
            let result = parsed.query(q.as_ptr() as *const _);
            match result {
                Some(item) => item.printitem(0),
                None => { libc::printf("The query failed\n\0".as_ptr() as *const _); }
            }
        }
        let microsecs =
            (end.tv_sec - start.tv_sec) * 1000000 + end.tv_usec - start.tv_usec;

        println!("Parsing took: {} microseconds", microsecs);
        println!("That means {} MBps", (text.len()-1) / microsecs as usize);
    }
}
// END main.c