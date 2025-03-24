use std::{env, process::Command};
use chrono::Utc;

macro_rules! var_from_cmd {
    ($cmd:expr, $out:literal) => {
        var_from_cmd!($cmd, $out, |x: String| x);
    };

    ($cmd:expr, $out:literal, $func:expr) => {{
        let output = $cmd.output().expect(concat!("Failed to get ", $out));
        let res = $func(String::from_utf8_lossy(&output.stdout).trim().to_string());
        println!(concat!("cargo:rustc-env=", $out, "={}"), res);
    }};
}

fn main() {
    var_from_cmd!(Command::new("git").args(["rev-parse", "--short", "HEAD"]), "GIT_HASH");

    var_from_cmd!(
        Command::new(env::var("RUSTC").unwrap_or("rustc".into())).args(["--version"]),
        "RUSTC_VERSION",
        |s: String| s.split_whitespace().nth(1).unwrap_or("unknown").to_string()
    );

    println!("cargo:rustc-env=BUILD_DATE={}", Utc::now().format("%Y-%m-%d").to_string());
}
