use quote::quote;
use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(SmartExpression)]
pub fn expression_impl_fn(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let name = &input.ident;

    let expanded = quote! {
        impl Expression for #name {
            fn as_any(&self) -> &dyn Any {
                self
            }

            fn to_string(&self) -> String {
                return format!("{:?}", self);
            }
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(SmartStatement)]
pub fn statement_as_any_fn(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let name = &input.ident;

    let expanded = quote! {
        impl Statement for #name {
            fn as_any(&self) -> &dyn Any {
                self
            }

            fn to_string(&self) -> String {
                return format!("{:?}", self);
            }
        }
    };

    TokenStream::from(expanded)
}
