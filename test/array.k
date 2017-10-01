fn main() {
    let x i64[4];
    x[0] = 0;
    x[1] = 1;
    x[2] = 2;
    x[3] = 3;

    let y i64[3][4];
    y[0] = x;
    y[0][0] = 0;
    y[0][1] = 1;
    y[0][2] = 2;
    y[1][0] = 3;
    y[1][1] = 4;
    y[1][2] = 5;
    y[2][0] = 6;
    y[2][1] = 7;
    y[2][2] = 8;

    /*
    let xs i64[3];
    let ys i64[3];
    let zs i64[5];
    let ws i64[3][4];

    xs = ys;

    let x i64;
    x = xs;

    ys = xs;
    zs = xs;
    ws = xs;
    */
}
