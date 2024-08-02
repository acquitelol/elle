use std/io@{printf, sprintf};
use std/string@{concat, strlen};
use std/math@{fabs};
use examples/resources/raylib;

const i32 screenWidth = 600;
const i32 screenHeight = 600;

const i64 BLACK = GetColor(0x00000000);
const i64 PINK = GetColor(0xFF7DFBFF);
const i64 PURPLE = GetColor(0xE785FFFF);
const i64 YELLOW = GetColor(0xFFE2B8FF);
const i64 BLUE = GetColor(0xC6B5FFFF);

const f32 circleRadius = 50.0;
const f32 gravity = 1000.0;
const f32 damping = -0.8;
const f32 epsilon = 5;

fn main() {
    string title = "Using raylib in Elle! <3";

    InitWindow(screenWidth, screenHeight, title);
    defer CloseWindow();

    SetTargetFPS(60);

    f32 positionX = screenWidth - circleRadius;
    f32 positionY = 100;

    i32 fontSize = 40;
    i32 velocityUpdateFactor = 4;
    f32 velocityFactor = 2;

    f32 velocityX = 120 * velocityFactor;
    f32 velocityY = 100 * velocityFactor;

    bool finished = false;
    i32 bounces = 0;

    // 10 = number of digits in INT_MAX
    string bouncesString[strlen("Bounces: ") + 10];

    while !WindowShouldClose() {
        f32 deltaTime = GetFrameTime();

        if finished {
            finished = false;
            velocityX *= -1;
            velocityX *= velocityUpdateFactor;
            velocityY = gravity * velocityUpdateFactor * 0.2;
        }

        BeginDrawing();
            velocityY += gravity * deltaTime;
            ClearBackground(BLACK);

            f32 newX = positionX + velocityX * deltaTime;

            if newX < circleRadius || newX + circleRadius > GetScreenWidth() {
                bounces++;
                velocityX *= damping;
            } else {
                positionX = newX;
            }

            f32 newY = positionY + velocityY * deltaTime;

            if newY < circleRadius || newY + circleRadius > GetScreenHeight() {
                bounces++;
                velocityY *= damping;
            } else {
                positionY = newY;
            }

            positionX += velocityX * deltaTime;
            positionY += velocityY * deltaTime;

            if (fabs(velocityY) < epsilon) && screenHeight - circleRadius - positionY < epsilon && !finished {
                finished = true;
            }

            DrawCircleGradient((i32)positionX, (i32)positionY, circleRadius, PURPLE, PINK);
            DrawText(title, (screenWidth - MeasureText(title, fontSize)) / 2, 40, fontSize, BLUE);

            sprintf(bouncesString, "Bounces: %d", bounces);
            DrawText(bouncesString, (screenWidth - MeasureText(bouncesString, fontSize)) / 2, 100, fontSize, YELLOW);

            printf(
                concat.(
                    "positionX = %f",
                    "positionY = %f",
                    "velocityX = %f",
                    "velocityY = %f",
                    "deltaTime = %f",
                    "-----------------------"
                ),
                (f64)positionX,
                (f64)positionY,
                (f64)velocityX,
                (f64)velocityY,
                (f64)deltaTime
            );
        EndDrawing();
    }
}
