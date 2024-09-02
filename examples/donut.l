use std/vectors;
use std/string;
use std/math;
use std/io;

const f32 THETA_SPACING = 0.07;
const f32 PHI_SPACING = 0.02;
const string LIGHTING = ".,-~:;=!*#$@";

const i32 WIDTH = 60;
const i32 HEIGHT = 24;
const i32 SIZE = WIDTH * HEIGHT;

fn update(f32 rotX, f32 rotZ, f32 *zBuffer, string output, Vector3 *lightDirection) {
    string lighting = LIGHTING;
    u32 lightingLength = lighting.len();

    memset(zBuffer, 0, SIZE * #size(f32));
    memset(output, 32, SIZE * #size(char));

    f32 sinRotX = sin(rotX);
    f32 cosRotX = cos(rotX);

    f32 sinRotZ = sin(rotZ);
    f32 cosRotZ = cos(rotZ);

    for f32 theta = 0; theta < 2 * PI; theta += THETA_SPACING {
        f32 sinTheta = sin(theta);
        f32 cosTheta = cos(theta);

        for f32 phi = 0; phi < 2 * PI; phi += PHI_SPACING {
            f32 sinPhi = sin(phi);
            f32 cosPhi = cos(phi);

            Vector3 surfacePos = Vector3 {
                x = cosPhi * (cosTheta + 2),
                y = sinPhi * (cosTheta + 2),
                z = sinTheta,
            };

            f32 depth = 1 / (surfacePos.y * sinRotX + surfacePos.z * cosRotX + 5);
            f32 projectionOffset = surfacePos.y * cosRotX - surfacePos.z * sinRotX;

            i32 x = 30 + 36 * depth * (surfacePos.x * cosRotZ - projectionOffset * sinRotZ);
            i32 y = 12 + 12 * depth * (surfacePos.x * sinRotZ + projectionOffset * cosRotZ);

            i32 offset = x + WIDTH * y;

            Vector3 normal = Vector3 {
                x = surfacePos.x,
                y = surfacePos.y,
                z = surfacePos.z,
            };

            f32 normalLengthSquared = normal.length_sq();

            if normalLengthSquared > 0 {
                normal = normal.scale(1.0 / normalLengthSquared);
            }

            f32 illumination = normal.dot(*lightDirection);
            i32 lightingIndex = (i32)(fabs(illumination) * (lightingLength - 1));

            if HEIGHT > y && y > 0 && x > 0 && WIDTH > x && depth > zBuffer[offset] {
                zBuffer[offset] = depth;
                i32 lightingMapping = (i32)lightingIndex % (lightingLength - 1);

                if lightingMapping < 0 {
                    output[offset] = lighting[0];
                    continue;
                }

                if lightingMapping > lightingLength - 4 {
                    output[offset] = lighting[lightingMapping - 4];
                    continue;
                }

                output[offset] = lighting[lightingMapping];
            }
        }
    }

    io::print("\x1b[H");

    for i32 character = 0; character < SIZE; character += 1 {
        if character % WIDTH {
            io::putchar(output[character]);
        } else {
            io::putchar('\n');
        }
    }
}

fn main() {
    f32 rotX = 0;
    f32 rotZ = 0;

    f32 *zBuffer = malloc(SIZE * #size(f32));
    string output = malloc(SIZE * #size(char));
    Vector3 lightDirection = Vector3 { x = 0, y = 1, z = -1 };
    io::print("\x1b[2J");

    while true {
        update(rotX, rotZ, zBuffer, output, &lightDirection);

        rotX += 0.004;
        rotZ += 0.002;
    }
}
