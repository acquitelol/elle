use std/math;
global pub;

def Vector3 {
    f32 x;
    f32 y;
    f32 z;
}

def Vector2 {
    f32 x;
    f32 y;
}

const Vector2 v2_zero = Vector2 { x = 0, y = 0 };
const Vector3 v3_zero = Vector3 { x = 0, y = 0, z = 0 };

// ----- Vector2 methods -----

fn v2_dot(Vector2 a, Vector2 b) {
    return a.x * b.x + a.y * b.y;
}

fn v2_cross(Vector2 a, Vector2 b) {
    return a.x * b.y - a.y * b.x;
}

fn v2_length_sq(Vector2 v) {
    return v2_dot(v, v);
}

fn v2_length(Vector2 v) {
    return sqrt(v2_length_sq(v));
}

fn v2_add(Vector2 a, Vector2 b) {
    return Vector2 {
        x = a.x + b.x,
        y = a.y + b.y,
    };
}

fn v2_sub(Vector2 a, Vector2 b) {
    return Vector2 {
        x = a.x - b.x,
        y = a.y - b.y,
    };
}

fn v2_scale(Vector2 v, f32 scalar) {
    return Vector2 {
        x = v.x * scalar,
        y = v.y * scalar,
    };
}

fn v2_normalize(Vector2 v) {
    f32 length = v2_length(v);
    return v2_scale(v, 1.0 / length);
}

fn v2_rotate(Vector2 v, f32 angle) {
    f32 c = cos(angle);
    f32 s = sin(angle);

    return Vector2 {
        x = v.x * c - v.y * s,
        y = v.x * s + v.y * c,
    };
}

fn v2_angle(Vector2 v1, Vector2 v2) {
    return atan2(v2.y, v2.x) - atan2(v1.y, v1.x);
}

// ----- Vector3 methods -----

fn v3_dot(Vector3 a, Vector3 b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

fn v3_cross(Vector3 a, Vector3 b) {
    return Vector3 {
        x = a.y * b.z - a.z * b.y,
        y = a.z * b.x - a.x * b.z,
        z = a.x * b.y - a.y * b.x,
    };
}

fn v3_length_sq(Vector3 v) {
    return v3_dot(v, v);
}

fn v3_length(Vector3 v) {
    return sqrt(v3_length_sq(v));
}

fn v3_add(Vector3 a, Vector3 b) {
    return Vector3 {
        x = a.x + b.x,
        y = a.y + b.y,
        z = a.z + b.z,
    };
}

fn v3_sub(Vector3 a, Vector3 b) {
    return Vector3 {
        x = a.x - b.x,
        y = a.y - b.y,
        z = a.z - b.z,
    };
}

fn v3_scale(Vector3 v, f32 scalar) {
    return Vector3 {
        x = v.x * scalar,
        y = v.y * scalar,
        z = v.z * scalar,
    };
}

fn v3_normalize(Vector3 v) {
    f32 length = v3_length(v);
    return v3_scale(v, 1.0 / length);
}

// Vector3 rotation follows Rodrigues' Rotation formula:
// as defined by:

// v_rot = v ⋅ cos(θ) + (u × v) ⋅ sin(θ) + u ⋅ (u ⋅ v) ⋅ (1 - cos(θ))

// --- Where:
// v is the original vector
// u is the normalized axis of rotation
// θ is the angle of rotation
// × denotes the cross product
// ⋅ denotes the dot product
// ----------
fn v3_rotate(Vector3 v, Vector3 axis, f32 angle) {
    Vector3 u = v3_normalize(axis);

    f32 cos_theta = cos(angle);
    f32 sin_theta = sin(angle);

    Vector3 term1 = v3_scale(v, cos_theta);
    Vector3 term2 = v3_scale(v3_cross(u, v), sin_theta);
    Vector3 term3 = v3_scale(u, v3_dot(u, v) * (1.0 - cos_theta));

    return v3_add(v3_add(term1, term2), term3);
}

// Vector3 angle follows the following formula:
//
// cos(θ) = (v1 ⋅ v2) / (||v1|| × ||v2||)
//
// --- Where:
// v1 is the first vector
// v2 is the second vector
// θ is the angle of rotation
// × denotes the cross product
// ⋅ denotes the dot product
// ||x|| denotes the magnitude
// ----------
fn v3_angle(Vector3 v1, Vector3 v2) {
    f32 dot = v3_dot(v1, v2);
    f32 lengths = v3_length(v1) * v3_length(v2);

    if lengths == 0.0 {
        return 0.0;
    }

    f32 cos_theta = dot / lengths;
    cos_theta = fclamp(cos_theta, -1.0, 1.0);

    // Unwrap the cos(θ) into just θ
    return (f32)acos(cos_theta);
}
