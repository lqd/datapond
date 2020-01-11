use proc_macro2::TokenStream;
use quote::ToTokens;

mod ast;
mod encode;
mod to_tokens;

pub fn generate_datafrog(input: TokenStream) -> TokenStream {
    let parsed_program = match syn::parse2(input) {
        Ok(program) => program,
        Err(err) => return TokenStream::from(err.to_compile_error()),
    };
    let typechecked_program = crate::typechecker::typecheck(parsed_program).unwrap();
    let encoded_program = encode::encode(typechecked_program);
    encoded_program.to_token_stream()
}