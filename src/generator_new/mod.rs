use proc_macro2::TokenStream;
use quote::{ToTokens, quote};

mod ast;
mod encode;
mod to_tokens;

pub fn generate_datafrog(input: TokenStream) -> TokenStream {
    let parsed_program = match syn::parse2(input) {
        Ok(program) => program,
        Err(err) => {
            let tokens = TokenStream::from(err.to_compile_error());
            return quote! { {#tokens }};
        },
    };
    let typechecked_program = match crate::typechecker::typecheck(parsed_program) {
        Ok(program) => program,
        Err(err) => {
            let tokens = TokenStream::from(err.to_syn_error().to_compile_error());
            return quote! { {#tokens }};
        },
    };
    let encoded_program = encode::encode(typechecked_program);
    encoded_program.to_token_stream()
}
