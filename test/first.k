//extern fn syscall5(num *i64, arg1 *i64, arg2 *i64, arg3 *i64, arg4 *i64, arg5 *i64) -> i64;
//extern fn syscall5(num i64, arg1 i64, arg2 i64, arg3 i64, arg4 i64, arg5 i64) -> i64;
extern fn syscall5();

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
    x := 3;
    y := inc(x);
    z := add(x, y);

    f := 3.14159;
    str := "test string";

    syscall5();
//    syscall5(1, 1, 3, 8, 0, 0);
}
