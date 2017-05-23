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

    str := "test string";
}
