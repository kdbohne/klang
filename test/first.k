fn inc(x i64) -> i64
{
    x + 1
}

fn add(a i64, b i64) -> i64
{
    a + b
}

fn err_func() -> i64
{
    3;
}

fn err_func2()
{
    3 + 4
}

fn main()
{
    x := 3;
    y := inc(x);
    z := add(x, y);

    f := 3.14159;
    str := "test string";

    err := add(f, str);
    err2 := add(3, 4, 5);
    err3 := add();
    err3 := 3;
}
