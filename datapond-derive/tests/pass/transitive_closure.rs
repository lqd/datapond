use datapond_derive::datapond;

fn main() {
    let inp = vec![(1, 2), (2, 3)];
    let out;
    datapond! {
        input inp(x: u32, y: u32)
        output out(x: u32, y: u32)
        out(x, y) :- inp(x, y).
        out(x, y) :- out(x, z), out(z, y).
    };
    assert!(out.len() == 3);
    assert!(out[0] == (1, 2));
    assert!(out[1] == (1, 3));
    assert!(out[2] == (2, 3));
}
