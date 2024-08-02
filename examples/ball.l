use std/io@{printf, sprintf};
use std/string@{concat, strlen};
use std/math@{fabs};
use examples/resources/raylib;

const int screenWidth = 600;
const int screenHeight = 600;

const long BLACK = GetColor(0x00000000);
const long PINK = GetColor(0xFF7DFBFF);
const long PURPLE = GetColor(0xE785FFFF);
const long YELLOW = GetColor(0xFFE2B8FF);
const long BLUE = GetColor(0xC6B5FFFF);

const float circleRadius = 50.0;
const float gravity = 1000.0;
const float damping = -0.8;
const float epsilon = 5;

fn main() {
    string title = "Using raylib in Elle! <3";

    InitWindow(screenWidth, screenHeight, title);
    defer CloseWindow();

    SetTargetFPS(60);

    float positionX = screenWidth - circleRadius;
    float positionY = 100;

    int fontSize = 40;
    int velocityUpdateFactor = 4;
    float velocityFactor = 2;

    float velocityX = 120 * velocityFactor;
    float velocityY = 100 * velocityFactor;

    bool finished = false;
    int bounces = 0;

    // 10 = number of digits in INT_MAX
    string bouncesString[strlen("Bounces: ") + 10];

    while !WindowShouldClose() {
        float deltaTime = GetFrameTime();

        if finished {
            finished = false;
            velocityX *= -1;
            velocityX *= velocityUpdateFactor;
            velocityY = gravity * velocityUpdateFactor * 0.2;
        }

        BeginDrawing();
            velocityY += gravity * deltaTime;
            ClearBackground(BLACK);

            float newX = positionX + velocityX * deltaTime;

            if newX < circleRadius || newX + circleRadius > GetScreenWidth() {
                bounces++;
                velocityX *= damping;
            } else {
                positionX = newX;
            }

            float newY = positionY + velocityY * deltaTime;

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

            DrawCircleGradient((int)positionX, (int)positionY, circleRadius, PURPLE, PINK);
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
                (double)positionX,
                (double)positionY,
                (double)velocityX,
                (double)velocityY,
                (double)deltaTime
            );
        EndDrawing();
    }
}
