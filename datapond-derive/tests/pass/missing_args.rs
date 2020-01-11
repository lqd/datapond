use datapond_derive::datapond;

fn test1() {
    let inp = vec![(1, 2), (2, 3)];
    let out;
    datapond! {
        input inp(x: u32, y: u32)
        output out(x: u32)
        out(x) :- inp(x, _).
    };
    assert!(out.len() == 2);
    assert!(out[0] == (1,));
    assert!(out[1] == (2,));
}

fn test2() {
    let inp = vec![(1, 2), (2, 3)];
    let out;
    datapond! {
        input inp(x: u32, y: u32)
        output out(x: u32)
        out(x) :- inp(x, _), inp(_, x).
    };
    assert!(out.len() == 1);
    assert!(out[0] == (2,));
}

fn main() {
    test1();
    test2();
}
