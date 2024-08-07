global pub;

def Vector3 {
    f32 x;
    f32 y;
    f32 z;
}

fn lengthSquared(Vector3 v) -> f32 {
    return v.x * v.x + v.y * v.y + v.z * v.z;
}

fn dot(Vector3 a, Vector3 b) -> f32 {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

fn normalize(Vector3 v) -> Vector3 {
    f32 length = sqrt(v.x * v.x + v.y * v.y + v.z * v.z);

    return Vector3 {
        x = v.x / length,
        y = v.y / length,
        z = v.z / length,
    };
}

fn scale(Vector3 v, f32 scalar) {
    return Vector3 {
        x = v.x * scalar,
        y = v.y * scalar,
        z = v.z * scalar
    };
}
