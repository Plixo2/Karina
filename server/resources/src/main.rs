#![allow(nonstandard_style)]
#![allow(dead_code)]
#![allow(unused)]

static mut GC_INSTANCE: *mut GC = 0 as *mut GC;

pub fn main() -> () {
    unsafe {
        GC_INSTANCE = Box::into_raw(Box::new(GC { objects: Vec::new() }));
        start();
    }
    ;
}

unsafe fn start() -> () {
    {
        let a: i64 = (1i64) + (1i64);
        let obj: *mut test_SomeObject = {
            let ptr = Box::into_raw(Box::new(test_SomeObject { time: 99i64, m: 0.0f64 }));
            (*GC_INSTANCE).objects.push(ptr as *mut u8);
            ptr
        };
        let b: i64 = (*obj).time;
        return;
    };
}

unsafe fn call_me(this: *mut test_SomeObject) -> i64 {
    {
        return 0i64;
    };
}

struct GC {
    objects: Vec<*mut u8>,
}

struct test_SomeObject {
    time: i64,
    m: f64,
}