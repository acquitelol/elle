use std/math;
global pub;

struct Vector3 {
    f32 x;
    f32 y;
    f32 z;
};

struct Vector2 {
    f32 x;
    f32 y;
};

// ----- Vector2 methods -----

fn Vector2::zero() {
    return Vector2 {
        x = 0,
        y = 0
    };
}

fn Vector2::dot(Vector2 self, Vector2 v2) {
    return self.x * v2.x + self.y * v2.y;
}

fn Vector2::cross(Vector2 self, Vector2 v2) {
    return self.x * v2.y - self.y * v2.x;
}

fn Vector2::length_sq(Vector2 self) {
    return self.dot(self);
}

fn Vector2::length(Vector2 self) {
    return sqrt(self.length_sq());
}

fn Vector2::add(Vector2 self, Vector2 v2) {
    return Vector2 {
        x = self.x + v2.x,
        y = self.y + v2.y,
    };
}

fn Vector2::sub(Vector2 self, Vector2 v2) {
    return Vector2 {
        x = self.x - v2.x,
        y = self.y - v2.y,
    };
}

fn Vector2::scale(Vector2 self, f32 scalar) {
    return Vector2 {
        x = self.x * scalar,
        y = self.y * scalar,
    };
}

fn Vector2::normalize(Vector2 self) {
    f32 length = self.length();
    return self.scale(1.0 / length);
}

fn Vector2::rotate(Vector2 self, f32 angle) {
    f32 c = cos(angle);
    f32 s = sin(angle);

    return Vector2 {
        x = self.x * c - self.y * s,
        y = self.x * s + self.y * c,
    };
}

fn Vector2::angle(Vector2 self, Vector2 v2) {
    return atan2(v2.y, v2.x) - atan2(self.y, self.x);
}

// ----- Vector3 methods -----

fn Vector3::zero() {
    return Vector3 {
        x = 0,
        y = 0,
        z = 0
    };
}

fn Vector3::dot(Vector3 self, Vector3 v2) {
    return self.x * v2.x + self.y * v2.y + self.z * v2.z;
}

fn Vector3::cross(Vector3 self, Vector3 v2) {
    return Vector3 {
        x = self.y * v2.z - self.z * v2.y,
        y = self.z * v2.x - self.x * v2.z,
        z = self.x * v2.y - self.y * v2.x,
    };
}

fn Vector3::length_sq(Vector3 self) {
    return self.dot(self);
}

fn Vector3::length(Vector3 self) {
    return sqrt(self.length_sq());
}

fn Vector3::add(Vector3 self, Vector3 v2) {
    return Vector3 {
        x = self.x + v2.x,
        y = self.y + v2.y,
        z = self.z + v2.z,
    };
}

fn Vector3::sub(Vector3 self, Vector3 v2) {
    return Vector3 {
        x = self.x - v2.x,
        y = self.y - v2.y,
        z = self.z - v2.z,
    };
}

fn Vector3::scale(Vector3 self, f32 scalar) {
    return Vector3 {
        x = self.x * scalar,
        y = self.y * scalar,
        z = self.z * scalar,
    };
}

fn Vector3::normalize(Vector3 self) {
    f32 length = self.length();
    return self.scale(1.0 / length);
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
fn Vector3::rotate(Vector3 self, Vector3 axis, f32 angle) {
    Vector3 u = axis.normalize();

    f32 cos_theta = cos(angle);
    f32 sin_theta = sin(angle);

    Vector3 term1 = self.scale(cos_theta);
    Vector3 term2 = u.cross(self).scale(sin_theta);
    Vector3 term3 = u.scale(u.dot(self) * (1.0 - cos_theta));

    return term1.add(term2).add(term3);
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
fn Vector3::angle(Vector3 self, Vector3 v2) {
    f32 dot = self.dot(v2);
    f32 lengths = self.length() * v2.length();

    if lengths == 0.0 {
        return 0.0;
    }

    f32 cos_theta = dot / lengths;
    cos_theta = fclamp(cos_theta, -1.0, 1.0);

    // Unwrap the cos(θ) into just θ
    return (f32)acos(cos_theta);
}
