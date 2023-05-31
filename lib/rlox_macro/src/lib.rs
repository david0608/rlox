use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input,
    Ident,
    Token,
    parse::{
        Parse,
        ParseStream,
        Result,
    },
};

// Example:
//  declare_unary_create_method! { #name, #variant }
struct DeclareUnaryCreateMethod {
    name: Ident,
    variant: Ident,
}

impl Parse for DeclareUnaryCreateMethod {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let variant: Ident = input.parse()?;
        Ok(
            DeclareUnaryCreateMethod {
                name,
                variant,
            }
        )
    }
}

#[proc_macro]
pub fn declare_unary_create_method(input: TokenStream) -> TokenStream {
    let DeclareUnaryCreateMethod {
        name,
        variant,
    } = parse_macro_input!(input as DeclareUnaryCreateMethod);

    let expanded = quote! {
        pub fn #name(r: Expression<'a>) -> UnaryExpression<'a> {
            UnaryExpression::#variant(r)
        }
    };

    TokenStream::from(expanded)
}

// Example:
//  declare_binary_create_method! { #name, #variant }
struct DeclareBinaryCreateMethod {
    name: Ident,
    variant: Ident,
}

impl Parse for DeclareBinaryCreateMethod {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let variant: Ident = input.parse()?;
        Ok(
            DeclareBinaryCreateMethod {
                name,
                variant,
            }
        )
    }
}

#[proc_macro]
pub fn declare_binary_create_method(input: TokenStream) -> TokenStream {
    let DeclareBinaryCreateMethod {
        name,
        variant,
    } = parse_macro_input!(input as DeclareBinaryCreateMethod);

    let expanded = quote! {
        pub fn #name(l: Expression<'a>, r: Expression<'b>) -> BinaryExpression<'a, 'b> {
            BinaryExpression::#variant(l, r)
        }
    };

    TokenStream::from(expanded)
}
