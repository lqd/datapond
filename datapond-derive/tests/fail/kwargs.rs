use datapond_derive::datapond;

fn test1() {
    let inp = vec![(1, 2, 0), (2, 3, 0)];
    let out;
    datapond! {
        input inp(x: u32, y: u32, z: u32)
        output out(x: u32, y: u32)
        out(x, y) :- inp(.y=y, .y=x).
    };
    assert_eq!(out.len(), 2);
}

fn test2() {
    let inp = vec![(1, 2, 0), (2, 3, 0)];
    let out;
    datapond! {
        input inp(x: u32, y: u32, z: u32)
        output out(x: u32, y: u32)
        out(x, y) :- inp(.a=y, .y=x).
    };
    assert_eq!(out.len(), 2);
}

fn main() {
    test1();
    test2();
}