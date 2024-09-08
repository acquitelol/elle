use examples/resources/raylib;
use std/string;
use std/math;
use std/io;

const i32 screenWidth = 600;
const i32 screenHeight = 600;

const Color BLACK = raylib::get_color(0x00000000);
const Color PINK = raylib::get_color(0xFF7DFBFF);
const Color BERRY = raylib::get_color(0x2E172DFF);
const Color PURPLE = raylib::get_color(0xE785FFFF);
const Color YELLOW = raylib::get_color(0xFFE2B8FF);
const Color BLUE = raylib::get_color(0xC6B5FFFF);

const f32 circleRadius = 50.0;
const f32 gravity = 1000.0;
const f32 damping = -0.8;
const f32 epsilon = 5;

fn main() {
    string title = "Using raylib in Elle! <3";
    string subtitle = "Try controlling the ball with W, A, S, D!";

    raylib::init_window(screenWidth, screenHeight, title);
    defer raylib::close_window();

    raylib::init_audio_device();
    defer raylib::close_audio_device();

    Sound sound = raylib::load_sound("./examples/resources/hit.wav");
    defer raylib::unload_sound(sound);

    raylib::set_target_fps(60);

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
    string bouncesString["Bounces: ".len() + 10];
    string fpsString["FPS: ".len() + 10];

    while !raylib::window_should_close() {
        f32 deltaTime = raylib::get_frame_time();
        i32 fps = math::round(1 / deltaTime);

        if finished {
            finished = false;
            velocity.x *= velocityUpdateFactor * -1;
            velocity.y = gravity * velocityUpdateFactor * 0.2;
        }

        raylib::begin_drawing();
        velocity.y += gravity * deltaTime;
        raylib::clear_background(BERRY);

        if raylib::is_key_down('W') {
            velocity.y = -200;
        }

        if raylib::is_key_down('S') {
            velocity.y = 200;
        }

        if raylib::is_key_down('A') {
            velocity.x = -200;
        }

        if raylib::is_key_down('D') {
            velocity.x = 200;
        }

        f32 newX = position.x + velocity.x * deltaTime;

        if newX < circleRadius || newX + circleRadius > raylib::get_screen_width() {
            bounces += 1;
            velocity.x *= damping;
            raylib::play_sound(sound);
        } else {
            position.x = newX;
        }

        f32 newY = position.y + velocity.y * deltaTime;

        if newY < circleRadius || newY + circleRadius > raylib::get_screen_height() {
            bounces += 1;
            velocity.y *= damping;
            raylib::play_sound(sound);
        } else {
            position.y = newY;
        }

        position.x += velocity.x * deltaTime;
        position.y += velocity.y * deltaTime;

        if (math::abs(velocity.y) < epsilon) && screenHeight - circleRadius - position.y < epsilon && !finished {
            finished = true;
        }

        raylib::draw_circle((i32)position.x, (i32)position.y, circleRadius, PINK);

        // Title
        raylib::draw_text(title, (screenWidth - raylib::measure_text(title, fontSize)) / 2, 40, fontSize, BLUE);

        // Bounces
        io::sprintf(bouncesString, "Bounces: %d", bounces);
        raylib::draw_text(bouncesString, (screenWidth - raylib::measure_text(bouncesString, fontSize)) / 2, 100, fontSize, YELLOW);

        // Subtitle
        raylib::draw_text(subtitle, (screenWidth - raylib::measure_text(subtitle, fontSize / 2)) / 2, 160, fontSize / 2, PURPLE);

        // FPS
        io::sprintf(fpsString, "FPS: %d", fps);
        raylib::draw_text(fpsString, screenWidth - raylib::measure_text(fpsString, fontSize / 2) - 30, screenHeight - 50, fontSize / 2, YELLOW);


        io::dbg(position.x, position.y, velocity.x, velocity.y, deltaTime);
        io::println("-----------------------");

        raylib::end_drawing();
    }
}
