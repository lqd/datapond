use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;

#[proc_macro_hack]
pub fn datapond(input: TokenStream) -> TokenStream {
    datapond::generate_datafrog(input.into()).into()
}
