use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;

#[proc_macro_hack]
pub fn datapond(input: TokenStream) -> TokenStream {
    datapond::generate_datafrog(input.into()).into()
}

#[proc_macro]
pub fn datapond_skeleton(input: TokenStream) -> TokenStream {
    let input = input.to_string();
    let skeleton = datapond::generate_skeleton_datafrog(&input, false);
    skeleton.parse().unwrap()
}
