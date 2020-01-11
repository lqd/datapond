#[test]
fn tests() {
    let runner = trybuild::TestCases::new();
    runner.pass("tests/pass/*.rs");
}
