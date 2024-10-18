global pub;

struct AudioStream {
    void *buffer;
    void *processor;

    i32 sampleRate;
    i32 sampleSize;
    i32 channels;
};

struct Sound {
    AudioStream stream;
    i32 frameCount;
};

struct Rectangle {
    f32 x;                // Rectangle top-left corner position x
    f32 y;                // Rectangle top-left corner position y
    f32 width;            // Rectangle width
    f32 height;           // Rectangle height
};

struct Vector2 {
    f32 x;
    f32 y;
};

struct Color {
    u8 r;
    u8 g;
    u8 b;
    u8 a;
};

struct Texture {
    u32 id;                 // OpenGL texture id
    i32 width;              // Texture base width
    i32 height;             // Texture base height
    i32 mipmaps;            // Mipmap levels, 1 by default
    i32 format;             // Data format (PixelFormat type)
};

struct RenderTexture {
    i32 id;                 // OpenGL framebuffer object id
    Texture texture;        // Color buffer attachment texture
    Texture depth;          // Depth buffer attachment texture
};

// Pseudo-namespace for Raylib functions
// These bindings are VERY unfinished
external fn InitWindow(i32 width, i32 height, string title) @alias("raylib::init_window");
external fn CloseWindow() @alias("raylib::close_window");
external fn WindowShouldClose() @alias("raylib::window_should_close") -> bool;
external fn BeginDrawing() @alias("raylib::begin_drawing");
external fn EndDrawing() @alias("raylib::end_drawing");
external fn SetTargetFPS(i32 fps) @alias("raylib::set_target_fps");
external fn GetColor(i32 hexValue) @alias("raylib::get_color") -> Color;
external fn ColorFromHSV(f32 hue, f32 saturation, f32 value) @alias("raylib::color_from_hsv") -> Color;
external fn ClearBackground(Color color) @alias("raylib::clear_background");
external fn DrawCircle(i32 centerX, i32 centerY, f32 radius, Color color) @alias("raylib::draw_circle");
external fn DrawRectangleV(Vector2 position, Vector2 size, Color color) @alias("raylib::draw_rectangle_v");
external fn DrawText(string text, i32 x, i32 y, i32 fontSize, Color color) @alias("raylib::draw_text");
external fn GetFrameTime() @alias("raylib::get_frame_time") -> f32;
external fn GetScreenWidth() @alias("raylib::get_screen_width") -> i32;
external fn GetScreenHeight() @alias("raylib::get_screen_height") -> i32;
external fn MeasureText(string text, i32 fontSize) @alias("raylib::measure_text") -> i32;
external fn LoadSound(string filename) @alias("raylib::load_sound") -> Sound;
external fn UnloadSound(Sound sound) @alias("raylib::unload_sound");
external fn PlaySound(Sound sound) @alias("raylib::play_sound");
external fn InitAudioDevice() @alias("raylib::init_audio_device");
external fn CloseAudioDevice() @alias("raylib::close_audio_device");
external fn IsKeyDown(i32 key) @alias("raylib::is_key_down") -> bool;
external fn DrawLineEx(Vector2 start_pos, Vector2 end_pos, f32 thickness, Color color) @alias("raylib::draw_line_ex");
external fn DrawPixel(i32 posX, i32 poxY, Color color) @alias("raylib::draw_pixel");
external fn LoadRenderTexture(i32 width, i32 height) @alias("raylib::load_render_texture") -> RenderTexture;
external fn UnloadRenderTexture(RenderTexture texture) @alias("raylib::unload_render_texture");
external fn BeginTextureMode(RenderTexture texture) @alias("raylib::begin_texture_mode");
external fn EndTextureMode() @alias("raylib::end_texture_mode");
external fn DrawTexturePro(Texture texture, Rectangle source, Rectangle dest, Vector2 origin, f32 rotation, Color tint) @alias("raylib::draw_texture_pro");
external fn DrawTextureRec(Texture texture, Rectangle source, Vector2 position, Color tint) @alias("raylib::draw_texture_rec");

namespace raylib;
