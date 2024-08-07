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

fn v2_dot(Vector2 a, Vector2 b) -> f32 {
    return a.x * b.x + a.y * b.y;
}

fn v2_length_sq(Vector2 v) {
    return v2_dot(v, v);
}

fn v2_add(Vector2 a, Vector2 b) {
    return Vector2 {
        x = a.x + b.x,
        y = a.y + b.y
    };
}

fn v2_sub(Vector2 a, Vector2 b) {
    return Vector2 {
        x = a.x - b.x,
        y = a.y - b.y
    };
}

fn v2_normalize(Vector2 v) {
    f32 length = sqrt(v.x * v.x + v.y * v.y);

    return Vector2 {
        x = v.x / length,
        y = v.y / length
    };
}

fn v2_scale(Vector2 v, f32 scalar) {
    return Vector2 {
        x = v.x * scalar,
        y = v.y * scalar
    };
}

fn v2_rotate(Vector2 v, f32 angle) {
    f32 c = cos(angle);
    f32 s = sin(angle);

    return Vector2 {
        x = v.x * c - v.y * s,
        y = v.x * s + v.y * c,
    }
}

fn v2_angle(Vector2 v1, Vector2 v2) {
    return atan2(v2.y, v2.x) - atan2(v1.y, v1.x);
}

fn v3_dot(Vector3 a, Vector3 b) -> f32 {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

fn v3_length_sq(Vector3 v) {
    return v3_dot(v, v);
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

fn v3_normalize(Vector3 v) {
    f32 length = sqrt(v.x * v.x + v.y * v.y + v.z * v.z);

    return Vector3 {
        x = v.x / length,
        y = v.y / length,
        z = v.z / length,
    };
}

fn v3_scale(Vector3 v, f32 scalar) {
    return Vector3 {
        x = v.x * scalar,
        y = v.y * scalar,
        z = v.z * scalar
    };
}
