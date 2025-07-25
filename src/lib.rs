pub mod parser;

// use proc_macro::TokenStream;
// use quote::quote;
// use syn::{parse_macro_input, DeriveInput};

// #[proc_macro_derive(ParserMethods)]
// pub fn derive_parser_methods(input: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(input as DeriveInput);
//     let name = input.ident;

//     // The generated impl block
//     let expanded = quote! {
//         impl<'a, K, O> Parser<'a, K, O> for #name
//         where
//             K: PartialEq + Copy + Clone,
//         {
//             fn then<O2, T>(self, p2: T) -> impl Parser<'a, K, (O, O2)>
//             where
//                 T: Parser<'a, K, O2> {
//                 pseq(self, p2)
//             }

//             fn or<T>(self, p2: T) -> impl Parser<'a, K, O>
//             where
//                 T: Parser<'a, K, O> {
//                 por(self, p2)
//             }

//             fn map<O2, F>(self, f: F) -> impl Parser<'a, K, O2>
//             where
//                 F: Fn(O) -> O2 + 'static {
//                 pbind(self, f)
//             }

//             fn between<A, P1, P2>(self, l: P1, r: P2) -> impl Parser<'a, K, O>
//             where
//                 P1: Parser<'a, K, A>,
//                 P2: Parser<'a, K, A> {
//                 pbetween(l, self, r)
//             }

//             fn many(self) -> impl Parser<'a, K, Vec<O>> {
//                 pmany(self)
//             }
//         }
//     };

//     TokenStream::from(expanded)
// }
