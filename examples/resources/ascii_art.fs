#version 330

in vec2 fragTexCoord;
out vec4 finalColor;

uniform sampler2D texture0;
uniform float time;

float character(int n, vec2 p) {
    p = floor(p * 4 * vec2(-1.0, 1.0) + 5.0 / 2);

    if (clamp(p.x, 0.0, 4.0) == p.x && clamp(p.y, 0.0, 4.0) == p.y) {
        int a = int(round(p.x) + 5.0 * round(p.y));
        if (((n >> a) & 1) == 1) return 1.0;
    }

    return 0.0;
}

float minCell = 4.0;
float maxCell = 16.0;

void main() {
    float t = (sin((time + 8) / 4) + 1.0) / 2.0;
    float cell = mix(minCell, maxCell, t);

    ivec2 size = textureSize(texture0, 0);
    vec2 pix = fragTexCoord * vec2(size);
    vec2 block = floor(pix / cell) * cell;
    vec3 base = texture(texture0, block / vec2(size)).rgb;

    float gray = 0.3 * base.r + 0.59 * base.g + 0.11 * base.b;

    int n = 4096; // 0b00000_00000_00100_00000_00000
    if (gray > 0.2) n = 65600; // 0b00000_00010_00000_00010_00000
    if (gray > 0.3) n = 163153; // 0b00000_00100_11111_01010_10001
    if (gray > 0.4) n = 15255086; // 0b01110_10001_10001_10001_01110
    if (gray > 0.5) n = 13121101; // 0b01100_10000_01101_10010_01101
    if (gray > 0.6) n = 15252014; // 0b01110_10001_01110_10001_01110
    if (gray > 0.7) n = 13195790; // 0b01100_10010_10110_10000_01110
    if (gray > 0.8) n = 11512810; // 0b01010_11111_01010_11111_01010

    vec2 local = (pix - block) / float(cell);
    vec2 p = local * 2.0 - 1.0;
    float bit = character(n, p);
    finalColor = vec4(base * bit, 1.0);
}
