use datapond_derive::datapond;

fn main() {
    let inp = vec![(1, 2), (2, 3)];
    let out;
    datapond! {
        input inp(x: u32, y: u32)
        output out(x: u32, y: u32)
        out(x, y) :- inp(y, x).
    };
    assert!(out.len() == 2);
    assert!(out[0] == (2, 1));
    assert!(out[1] == (3, 2));
}
