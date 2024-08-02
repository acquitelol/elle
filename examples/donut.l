use std/io@{printf, malloc};
use std/string@{memset};
use std/math@{sin, cos, PI};

const float THETA_SPACING = 0.07;
const float PHI_SPACING = 0.02;
const string LIGHTING = ".,-~:;=!*#$@";

const int WIDTH = 100;
const int HEIGHT = 24;
const int SIZE = WIDTH * HEIGHT;

fn update(float rotX, float rotZ, float *zBuffer, string output) {
    memset(zBuffer, 0, SIZE * #size(float));
    memset(output, 32, SIZE * #size(char));

    float sinRotX = sin(rotX);
    float cosRotX = cos(rotX);

    float sinRotZ = sin(rotZ);
    float cosRotZ = cos(rotZ);

    for float theta = 0; theta < PI * 2; theta += THETA_SPACING {
        for float phi = 0; phi < PI * 2; phi += PHI_SPACING {
            float sinPhi = sin(phi);
            float cosPhi = cos(phi);

            float sinTheta = sin(theta);
            float cosTheta = cos(theta);

            float thickness = cosTheta + 2;
            float depth = 1 / (sinPhi * thickness * sinRotX + sinTheta * cosRotX + 5);
            float projectionOffset = sinPhi * thickness * cosRotX - sinTheta * sinRotX;

            int x = 30 + 36 * depth * (cosPhi * thickness * cosRotZ - projectionOffset * sinRotZ);
            int y = 12 + 12 * depth * (cosPhi * thickness * sinRotZ + projectionOffset * cosRotZ);

            int offset = x + WIDTH * y;
            int illumination = 8 * ((sinTheta * sinRotX - sinPhi * cosTheta * cosRotX) * cosRotZ - sinPhi * cosTheta * sinRotX - sinTheta * cosRotX - cosPhi * cosTheta * sinRotZ);

            if HEIGHT > y && y > 0 && x > 0 && WIDTH > x && depth > zBuffer[(long)offset] {
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

    for long character = 0; character < SIZE; character++ {
        if character % WIDTH {
            printf("%c", output[character]);
        } else {
            printf("\n");
        }
    }
}

fn main() {
    float rotX = 0;
    float rotZ = 0;

    float *zBuffer = malloc(SIZE * #size(float));
    string output = malloc(SIZE * #size(char));
    printf("\x1b[2J");

    while true {
        update(rotX, rotZ, zBuffer, output);

        rotX += 0.004;
        rotZ += 0.002;
    }
}
