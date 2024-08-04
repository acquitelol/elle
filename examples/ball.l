use std/io@{printf, sprintf};
use std/string@{concat, strlen};
use std/math@{fabs};
use examples/resources/raylib;

const i32 screenWidth = 600;
const i32 screenHeight = 600;

const Color BLACK = GetColor(0x00000000);
const Color PINK = GetColor(0xFF7DFBFF);
const Color BERRY = GetColor(0x2E172DFF);
const Color PURPLE = GetColor(0xE785FFFF);
const Color YELLOW = GetColor(0xFFE2B8FF);
const Color BLUE = GetColor(0xC6B5FFFF);

const f32 circleRadius = 50.0;
const f32 gravity = 1000.0;
const f32 damping = -0.8;
const f32 epsilon = 5;

fn main() {
    string title = "Using raylib in Elle! <3";

    InitWindow(screenWidth, screenHeight, title);
    defer CloseWindow();

    InitAudioDevice();
    defer CloseAudioDevice();

    Sound sound = LoadSound("./examples/resources/hit.wav");
    defer UnloadSound(sound);

    SetTargetFPS(60);

    Vector2 position = Vector2 {
        x = screenWidth - circleRadius,
        y = 100
    };

    i32 fontSize = 40;
    i32 velocityUpdateFactor = 4;
    f32 velocityFactor = 2;

    Vector2 velocity = Vector2 {
        x = 120 * velocityFactor,
        y = 100 * velocityFactor
    };

    bool finished = false;
    i32 bounces = 0;

    // 10 = number of digits in INT_MAX
    string bouncesString[strlen("Bounces: ") + 10];

    while !WindowShouldClose() {
        f32 deltaTime = GetFrameTime();

        if finished {
            finished = false;
            velocity.x *= velocityUpdateFactor * -1;
            velocity.y = gravity * velocityUpdateFactor * 0.2;
        }

        BeginDrawing();
            velocity.y += gravity * deltaTime;
            ClearBackground(BERRY);

            f32 newX = position.x + velocity.x * deltaTime;

            if newX < circleRadius || newX + circleRadius > GetScreenWidth() {
                bounces++;
                velocity.x *= damping;
                PlaySound(sound);
            } else {
                position.x = newX;
            }

            f32 newY = position.y + velocity.y * deltaTime;

            if newY < circleRadius || newY + circleRadius > GetScreenHeight() {
                bounces++;
                velocity.y *= damping;
                PlaySound(sound);
            } else {
                position.y = newY;
            }

            position.x += velocity.x * deltaTime;
            position.y += velocity.y * deltaTime;

            if (fabs(velocity.y) < epsilon) && screenHeight - circleRadius - position.y < epsilon && !finished {
                finished = true;
            }

            DrawCircle((i32)position.x, (i32)position.y, circleRadius, PINK);
            DrawText(title, (screenWidth - MeasureText(title, fontSize)) / 2, 40, fontSize, BLUE);

            sprintf(bouncesString, "Bounces: %d", bounces);
            DrawText(bouncesString, (screenWidth - MeasureText(bouncesString, fontSize)) / 2, 100, fontSize, YELLOW);

            printf(
                concat.(
                    "position.x = %f",
                    "position.y = %f",
                    "velocity.x = %f",
                    "velocity.y = %f",
                    "deltaTime = %f",
                    "-----------------------"
                ),
                (f64)position.x,
                (f64)position.y,
                (f64)velocity.x,
                (f64)velocity.y,
                (f64)deltaTime
            );
        EndDrawing();
    }
}
