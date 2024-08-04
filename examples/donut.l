use std/io@{printf, malloc};
use std/string@{memset};
use std/math@{sin, cos, PI};

const f32 THETA_SPACING = 0.07;
const f32 PHI_SPACING = 0.02;
const string LIGHTING = ".,-~:;=!*#$@";

const i32 WIDTH = 100;
const i32 HEIGHT = 24;
const i32 SIZE = WIDTH * HEIGHT;

fn update(f32 rotX, f32 rotZ, f32 *zBuffer, string output) {
    memset(zBuffer, 0, SIZE * #size(f32));
    memset(output, 32, SIZE * #size(char));

    f32 sinRotX = sin(rotX);
    f32 cosRotX = cos(rotX);

    f32 sinRotZ = sin(rotZ);
    f32 cosRotZ = cos(rotZ);

    for f32 theta = 0; theta < PI * 2; theta += THETA_SPACING {
        for f32 phi = 0; phi < PI * 2; phi += PHI_SPACING {
            f32 sinPhi = sin(phi);
            f32 cosPhi = cos(phi);

            f32 sinTheta = sin(theta);
            f32 cosTheta = cos(theta);

            f32 thickness = cosTheta + 2;
            f32 depth = 1 / (sinPhi * thickness * sinRotX + sinTheta * cosRotX + 5);
            f32 projectionOffset = sinPhi * thickness * cosRotX - sinTheta * sinRotX;

            i32 x = 30 + 36 * depth * (cosPhi * thickness * cosRotZ - projectionOffset * sinRotZ);
            i32 y = 12 + 12 * depth * (cosPhi * thickness * sinRotZ + projectionOffset * cosRotZ);

            i32 offset = x + WIDTH * y;
            i32 illumination = 8 * ((sinTheta * sinRotX - sinPhi * cosTheta * cosRotX) * cosRotZ - sinPhi * cosTheta * sinRotX - sinTheta * cosRotX - cosPhi * cosTheta * sinRotZ);

            if HEIGHT > y && y > 0 && x > 0 && WIDTH > x && depth > zBuffer[offset] {
                zBuffer[offset] = depth;

                if illumination > 0 {
                    output[offset] = LIGHTING[illumination];
                } else {
                    output[offset] = LIGHTING[0];
                }
            }
        }
    }

    printf("\x1b[H");

    for i32 character = 0; character < SIZE; character++ {
        if character % WIDTH {
            printf("%c", output[character]);
        } else {
            printf("\n");
        }
    }
}

fn main() {
    f32 rotX = 0;
    f32 rotZ = 0;

    f32 *zBuffer = malloc(SIZE * #size(f32));
    string output = malloc(SIZE * #size(char));
    printf("\x1b[2J");

    while true {
        update(rotX, rotZ, zBuffer, output);

        rotX += 0.004;
        rotZ += 0.002;
    }
}
