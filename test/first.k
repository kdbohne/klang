fn inc(x i32) -> i32
{
    x + 1
}

fn add(a i32, b i32) -> i32
{
    a + b
}

fn main()
{
    x := 3;
    y := inc(x);
    z := add(x, y);
}
