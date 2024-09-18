// This code was intended to draw snowflakes however
// it was repurposed to draw the Sierpinski Triangle
use examples/resources/raylib;
use std/math;

const i32 SCREEN_FACTOR = 80;
const i32 SCREEN_WIDTH = 8 * SCREEN_FACTOR;
const i32 SCREEN_HEIGHT = 8 * SCREEN_FACTOR;

const Color PINK = raylib::get_color(0xFFB5DAFF);
const Color BLACK = raylib::get_color(0x00000000);

const i32 BRANCHES = 3;
const i32 DEPTH = 12;
const f32 ANGLE = 2 * PI / BRANCHES;
const f32 ROTATION_OFFSET = PI / 6.0;
const f32 LENGTH = SCREEN_FACTOR * 2;
const f32 THICKNESS = 8.0;

fn render_sierpinski(Vector2 center, f32 length, f32 thickness, i32 depth) {
    if depth <= 0 {
        return;
    }

    for i32 i = 0; i < BRANCHES; i += 1 {
        f32 angle = ROTATION_OFFSET + ANGLE * i;

        Vector2 branch = Vector2 {
            x = center.x + math::cos(angle) * length,
            y = center.y + math::sin(angle) * length
        };

        raylib::draw_line_ex(center, branch, thickness, PINK);
        render_sierpinski(branch, length / 2, thickness / 2, depth - 1);
    }
}

fn main() {
    raylib::init_window(SCREEN_WIDTH, SCREEN_HEIGHT, "Sierpinski Triangle");
    defer raylib::close_window();

    Vector2 center = Vector2 {
        x = SCREEN_WIDTH / 2,
        y = SCREEN_HEIGHT / 2,
    };

    while !raylib::window_should_close() {
        raylib::begin_drawing();
            raylib::clear_background(BLACK);
            render_sierpinski(center, LENGTH, THICKNESS, DEPTH);
        raylib::end_drawing();
    }
}
