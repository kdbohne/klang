extern fn syscall5(num *i64, arg1 *i64, arg2 *i64, arg3 *i64, arg4 *i64, arg5 *i64) -> i64;

fn inc(x i64) -> i64
{
    x + 1
}

fn add(a i64, b i64) -> i64
{
    a + b
}

fn main()
{
    /*
    x := 3;
    y := inc(x);
    z := add(x, y);
    */

    f := 3.14159;

    x := 3;
    xp := &x;
    xpp := &xp;

    x = 8;

    y := (x = 4) + 3;

    str := "hello!\n";
    stri := cast(*i64) str;
    strp := &str;

    ptr := &f;
    ptri := cast(*i64) ptr;

    syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) str, cast(*i64) 7, cast(*i64) 0, cast(*i64) 0);

    st := "true\n";
    sf := "false\n";
    if y == 7
    {
        syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) st, cast(*i64) 5, cast(*i64) 0, cast(*i64) 0);
    }
    else
    {
        syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) sf, cast(*i64) 6, cast(*i64) 0, cast(*i64) 0);
    }
}
