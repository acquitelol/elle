use std/vectors;
use std/math;
use std/io;

fn Vector3::test() {
    Vector3 v = Vector3 { x = 1.0, y = 0.0, z = 0.0 };
    Vector3 axis = Vector3 { x = 0.0, y = 0.0, z = 1.0 };
    f32 angle = PI / 2.0;

    Vector3 rotated = v.rotate(axis, angle);
    io::dbg(rotated.x, rotated.y, rotated.z);

    Vector3 v1 = Vector3 { x = 1, y = 0, z = 0 };
    Vector3 v2 = Vector3 { x = 0, y = 1, z = 0 };
    io::dbg(v1.angle(v2));

    Vector3 zero = Vector3::zero();
    io::dbg(zero.x, zero.y, zero.z);
}

fn Vector2::test() {
    Vector2 v1 = Vector2 { x = 1.0, y = 0.0 };
    Vector2 rotated = v1.rotate(PI);
    io::dbg(rotated.x, rotated.y);

    Vector2 v1 = Vector2 { x = 1, y = 0 };
    Vector2 v2 = Vector2 { x = 0, y = 1 };
    io::dbg(v1.angle(v2));

    Vector2 zero = Vector2::zero();
    io::dbg(zero.x, zero.y);
}

fn main() {
    Vector3::test();
    io::print('\n');
    Vector2::test();
}
