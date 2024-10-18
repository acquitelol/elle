use examples/resources/raylib;
use std/string;
use std/math;

const i32 MAX_ITERATIONS = 100;
const Color BLACK = raylib::get_color(0x00000000);
const Color WHITE = raylib::get_color(0xFFFFFFFF);

fn main() {
    i32 screenWidth = 900;
    i32 screenHeight = 900;

    f32 resolution = 1 / 3;
    i32 textureWidth = screenWidth * resolution;
    i32 textureHeight = screenHeight * resolution;

    raylib::init_window(screenWidth, screenHeight, "Mandelbrot Set");
    defer raylib::close_window();

    RenderTexture target = raylib::load_render_texture(textureWidth, textureHeight);
    defer raylib::unload_render_texture(target);

    raylib::set_target_fps(60);

    f32 zoom = 0.75;
    f32 offsetX = -0.75;
    f32 offsetY = 0.0;
    f32 fontSize = 15 * resolution;
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

        raylib::begin_texture_mode(target);
        raylib::clear_background(BLACK);

        zoomFactor.x = 0.5 * zoom * textureWidth;
        zoomFactor.y = 0.5 * zoom * textureHeight;

        for x = 0; x < textureWidth; x += 1 {
            for y = 0; y < textureHeight; y += 1 {
                z.x = (x - textureWidth / 2.0) / zoomFactor.x + offsetX;
                z.y = (y - textureHeight / 2.0) / zoomFactor.y + offsetY;

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
            textureWidth - raylib::measure_text(fpsString, fontSize) - (fontSize / 3 * 2),
            textureHeight - (fontSize * 3 / 2), fontSize,
            WHITE
        );

        raylib::end_texture_mode();

        raylib::begin_drawing();
        raylib::clear_background(BLACK);

        raylib::draw_texture_pro(target.texture,
            Rectangle {
                x = 0, y = 0,
                width = target.texture.width,
                height = -target.texture.height
            },
            Rectangle {
                x = 0, y = 0,
                width = screenWidth,
                height = screenHeight
            },
            Vector2 { x = 0, y = 0 },
            0, WHITE
        );

        raylib::end_drawing();
    }
}
