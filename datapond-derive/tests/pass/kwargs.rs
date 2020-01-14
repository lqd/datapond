use datapond_derive::datapond;

fn main() {
    let inp = vec![(1, 2, 0), (2, 3, 0)];
    let out;
    let out2;
    datapond! {
        input inp(x: u32, y: u32, z: u32)

        output out(x: u32, y: u32)
        out(x, y) :- inp(.y=y, .x=x).

        output out2(x: u32, y: u32)
        out2(a, b) :- inp(.y=a, .x=b).
    };
    assert_eq!(out.len(), 2);
    assert_eq!(out[0], (1, 2));
    assert_eq!(out[1], (2, 3));

    assert_eq!(out2.len(), 2);
    assert_eq!(out2[0], (2, 1));
    assert_eq!(out2[1], (3, 2));
}
