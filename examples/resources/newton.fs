#version 330

in vec2 fragTexCoord;
out vec4 finalColor;

uniform vec2 roots[5];
uniform vec3 colors[5];
uniform float x_min, x_max, y_min, y_max;
uniform int max_iter;
uniform float tol;

vec2 cplx_mul(vec2 a, vec2 b) {
    return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

vec2 cplx_div(vec2 a, vec2 b) {
    float d = b.x * b.x + b.y * b.y;
    return vec2((a.x * b.x + a.y * b.y) / d, (a.y * b.x - a.x * b.y) / d);
}

float cplx_abs(vec2 a) {
    return sqrt(a.x * a.x + a.y * a.y);
}

// z^5 + z^2 - z + 1 = 0
vec2 poly(vec2 z) {
    vec2 result = vec2(1);

    for (int i = 0; i < 5; ++i) {
        result = cplx_mul(result, z - roots[i]);
    }

    return result;
}

vec2 poly_prime(vec2 z) {
    vec2 result = vec2(0);

    for (int i = 0; i < 5; ++i) {
        vec2 term = vec2(1);

        for (int j = 0; j < 5; ++j) {
            if (j == i) continue;
            term = cplx_mul(term, z - roots[j]);
        }

        result += term;
    }

    return result;
}

void main() {
    float x = mix(x_min, x_max, fragTexCoord.x);
    float y = mix(y_min, y_max, fragTexCoord.y);
    vec2 z = vec2(x, y);

    for (int i = 0; i < max_iter; ++i) {
        vec2 f = poly(z);
        if (cplx_abs(f) < tol) break;

        vec2 f_prime = poly_prime(z);
        if (cplx_abs(f_prime) < tol) break;

        z -= cplx_div(f, f_prime);
    }

    int ri = 0;
    float dmin = cplx_abs(z - roots[0]);

    for (int i = 1; i < 5; ++i) {
        float d = cplx_abs(z - roots[i]);
        if (d < dmin) dmin = d, ri = i;
    }

    finalColor = vec4(colors[ri], 1.0);
}
