use std/io;
global pub;

fn assert(ElleMeta meta, bool condition) {
    if !condition {
        fprintf(
            stderr,
            "%s: Assertion '%s' failed.\n",
            meta.caller,
            meta.exprs[0]
        );

        abort();
    }
}
