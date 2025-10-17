use std::env;
use std::path::PathBuf;

use bindgen::callbacks::{DeriveInfo, ParseCallbacks, TypeKind};

fn main() {
    // Tell cargo to invalidate the built crate whenever the header changes
    println!("cargo:rerun-if-changed=src/libretro.h");
    println!("cargo:rerun-if-changed=src/wrapper.h");

    // The bindgen::Builder is the main entry point
    // to bindgen, and lets you build up options for
    // the resulting bindings.
    let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .header("src/wrapper.h")
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .parse_callbacks(Box::new(DeriveZerocopy))
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}

#[derive(Debug)]
struct DeriveZerocopy;
impl ParseCallbacks for DeriveZerocopy {
    fn add_derives(&self, info: &bindgen::callbacks::DeriveInfo<'_>) -> Vec<String> {
        if matches!(
            info,
            DeriveInfo {
                name: "retro_trace_elem_header_t",
                kind: TypeKind::Struct,
                ..
            }
        ) {
            vec!["zerocopy::FromBytes".to_string()]
        } else {
            vec![]
        }
    }
}
