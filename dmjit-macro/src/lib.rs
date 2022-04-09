use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use syn::Lit;

#[proc_macro]
pub fn ex_signature(args: TokenStream) -> TokenStream {
    let input = parse_macro_input!(args);
    let input_str = match input {
        Lit::Str(lit) => lit.value(),
        _ => panic!("not string input"),
    };

    let mut marker_pos: u32 = 0;
    let mut signature = Vec::new();
    for (idx, mask) in input_str.trim().split(' ').enumerate() {
        if mask.starts_with(">") {
            marker_pos = idx as u32;
        }
        let value = mask.strip_prefix(">").unwrap_or(mask);
        let element = if value == "??" {
            quote! {
                None
            }
        } else {
            let byte = hex::decode(value).map(|decoded_byte| decoded_byte[0]).unwrap();
            quote! {
                Some(#byte)
            }
        };
        signature.push(element);
    }

    let result = quote! {
        ExSignature {
            data: &[ #( #signature, )* ],
            marker_pos: #marker_pos
        }
	};
    result.into()
}