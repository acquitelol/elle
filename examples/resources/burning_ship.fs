#version 330

in vec2 fragTexCoord;
out vec4 finalColor;

uniform vec3 colors[5];
uniform float x_min, x_max, y_min, y_max;
uniform int mandelbrot;
uniform int max_iter;
uniform vec2 z0;

vec2 cplx_mul(vec2 a, vec2 b) {
    return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

float cplx_abs(vec2 a) {
    return sqrt(a.x * a.x + a.y * a.y);
}

void main() {
    float x = mix(x_min, x_max, fragTexCoord.x);
    float y = mix(y_min, y_max, fragTexCoord.y);
    vec2 c = vec2(x, y);
    vec2 z = z0;

    int i = 0;
    for (; i < max_iter; ++i) {
        // z = (|Re(z)| + |Im(z)|i)^2 + c
        if (mandelbrot == 0) z = vec2(abs(z.x), abs(z.y));
        z = cplx_mul(z, z) + c;
        if (cplx_abs(z) > 2.0) break;
    }

    float t = float(i) / float(max_iter);
    t = 1 - smoothstep(0.0, 1.0, sqrt(t));

    float k = t * 4.0;
    int l = int(floor(k));
    int m = min(l + 1, 4);
    float f = fract(k);

    vec3 col = mix(colors[l], colors[m], f);
    finalColor = vec4(col, 1.0);
}
