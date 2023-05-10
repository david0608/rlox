use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input,
    Ident,
    Expr,
    Token,
    parse::{
        Parse,
        ParseStream,
        Result,
    },
};

// Example:
//  impl_simple_token! { #name, #lexeme }
struct DeclareSimpleToken {
    name: Ident,
    lexeme: Expr,
}

impl Parse for DeclareSimpleToken {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let lexeme: Expr = input.parse()?;
        Ok(DeclareSimpleToken {
            name,
            lexeme,
        })
    }
}

#[proc_macro]
pub fn declare_simple_token(input: TokenStream) -> TokenStream {
    let DeclareSimpleToken {
        name,
        lexeme,
    } = parse_macro_input!(input as DeclareSimpleToken);

    let expanded = quote! {
        pub struct #name {
            line: usize,
        }

        impl SimpleToken for #name {
            fn new(line: usize) -> #name {
                #name {
                    line,
                }
            }

            fn new_enum(line: usize) -> TokenEnum<'static> {
                TokenEnum::#name(Self::new(line))
            }
        }

        impl Token for #name {
            fn lexeme(&self) -> &str {
                #lexeme
            }

            fn line(&self) -> usize {
                self.line
            }
        }
    };

    TokenStream::from(expanded)
}
