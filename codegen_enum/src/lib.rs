use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(Codegen)]
pub fn codegen_enum(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let Data::Enum(data_enum) = input.data else {
        panic!("cannot use the Codegen derive macro on non-enum inputs");
    };

    let variants = data_enum.variants.iter().filter_map(|variant| {
        if let Fields::Unnamed(_) = variant.fields {
            let ident = &variant.ident;
            Some(quote! { Self::#ident(this) => this.compile(gen, ctx) })
        } else {
            None
        }
    });

    let expanded = quote! {
        impl #name {
            pub fn compile(self, gen: &mut Compiler, ctx: &CodegenContext) -> Option<(Type, Value)> {
                match self {
                    #(#variants,)*
                    _ => panic!("statement: {:?}", self),
                }
            }
        }
    };

    TokenStream::from(expanded)
}
