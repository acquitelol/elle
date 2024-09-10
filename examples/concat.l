use std/io;

fn main() {
    // This uses string::concat under the hood
    // You may also use "a" <> "b" instead of <>=
    string res = "‎ ‎╱|、\n";
    res <>= "(˚ˎ 。7\n";
    res <>= "|、˜〵\n";
    res <>= "じしˍ,)ノ";

    io::println(res);
}
