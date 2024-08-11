use std/io;
use std/math;
use std/vectors;

fn main() {
    Vector3 v = Vector3 { x = 1.0, y = 0.0, z = 0.0 };
    Vector3 axis = Vector3 { x = 0.0, y = 0.0, z = 1.0 };
    f32 angle = PI / 2.0;

    Vector3 rotated = v3_rotate(v, axis, angle);

    // Expected: (0, 1, 0)
    // Result: (-0, 1, 0)
    //
    // Great!
    dbg(rotated.x, rotated.y, rotated.z);

    Vector3 v1 = Vector3 { x = 1, y = 0, z = 0 };
    Vector3 v2 = Vector3 { x = 0, y = 1, z = 0 };

    // Expected: PI / 2 or 1.57079632679
    // Result: 1.5707963705
    //
    // Close enough!
    dbg(v3_angle(v1, v2));
}
