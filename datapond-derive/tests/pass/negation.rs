use datapond_derive::datapond;

fn main() {
    let inp = vec![(1, 2), (2, 3)];
    let kill = vec![(3,), (4,), (5,)];
    let out;
    datapond! {
        input inp(x: u32, y: u32)
        input kill(y: u32)
        output out(x: u32, y: u32)
        out(x, y) :- inp(x, y), !kill(y).
    };
    assert!(out.len() == 1);
    assert!(out[0] == (1, 2));
}
