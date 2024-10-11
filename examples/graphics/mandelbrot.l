use examples/resources/raylib;
use std/string;
use std/math;

const i32 MAX_ITERATIONS = 100;
const Color BLACK = raylib::get_color(0x00000000);
const Color WHITE = raylib::get_color(0xFFFFFFFF);

fn main() {
    i32 screenWidth = 300;
    i32 screenHeight = 300;

    raylib::init_window(screenWidth, screenHeight, "Mandelbrot Set");
    defer raylib::close_window();

    raylib::set_target_fps(60);

    f32 zoom = 0.75;
    f32 offsetX = -0.75;
    f32 offsetY = 0.0;
    i32 fontSize = 15;
    f32 baseZoomScale = 0.02;

    // These must be declared out of the loop otherwise the program
    // will stack overflow due to stack allocating things over
    // and over again and never freeing them because its on the stack
    Vector2 zoomFactor;
    f32 deltaTime;
    i32 iteration;
    f32 temp_x;
    Vector2 z;
    Vector2 c;
    Color out;
    i32 fps;
    i32 x;
    i32 y;

    while !raylib::window_should_close() {
        deltaTime = raylib::get_frame_time();
        fps = math::round(1 / deltaTime);

        if raylib::is_key_down('1') {
            zoom *= 1.02;
        }

        if raylib::is_key_down('2') {
            zoom /= 1.02;
        }

        if raylib::is_key_down('A') {
            offsetX -= baseZoomScale / zoom;
        }

        if raylib::is_key_down('D') {
            offsetX += baseZoomScale / zoom;
        }

        if raylib::is_key_down('W') {
            offsetY -= baseZoomScale / zoom;
        }

        if raylib::is_key_down('S') {
            offsetY += baseZoomScale / zoom;
        }

        raylib::begin_drawing();
        defer raylib::end_drawing();

        raylib::clear_background(BLACK);

        zoomFactor.x = 0.5 * zoom * screenWidth;
        zoomFactor.y = 0.5 * zoom * screenHeight;

        for x = 0; x < screenWidth; x += 1 {
            for y = 0; y < screenHeight; y += 1 {
                z.x = (x - screenWidth / 2.0) / zoomFactor.x + offsetX;
                z.y = (y - screenHeight / 2.0) / zoomFactor.y + offsetY;

                c.x = z.x;
                c.y = z.y;
                iteration = 0;

                while (z.x * z.x + z.y * z.y) < 4 && iteration < MAX_ITERATIONS {
                    temp_x = z.x * z.x - z.y * z.y + c.x;
                    z.y = 2.0 * z.x * z.y + c.y;
                    z.x = temp_x;
                    iteration += 1;
                }

                if iteration == MAX_ITERATIONS {
                    raylib::draw_pixel(x, y, BLACK);
                } else {
                    out.r = (iteration * 8) % 255;
                    out.g = (iteration * 4) % 255;
                    out.b = (iteration * 2) % 255;
                    out.a = 255;

                    raylib::draw_pixel(x, y, raylib::get_color((out.r << 24) | (out.g << 16) | (out.b << 8) | out.a));
                }
            }
        }

        string fpsString = "FPS: {}".format(fps);
        raylib::draw_text(
            fpsString,
            screenWidth - raylib::measure_text(fpsString, fontSize) - (fontSize / 3 * 2),
            screenHeight - (fontSize * 3 / 2), fontSize,
            WHITE
        );
    }
}
