use datapond_derive::datapond;

fn main() {
    let inp = vec![(1, 2), (2, 3)];
    let out;
    datapond! {
        input inp(x: u32, y: u32, z: u32)
        output out(x: u32, y: u32)
        out(x, y) :- inp(y, x).
    };
}
