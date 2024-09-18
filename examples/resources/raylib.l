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

struct Vector2 {
    f32 x;
    f32 y;
};

struct Color {
    u32 r;
    u32 g;
    u32 b;
    u32 a;
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

struct raylib @namespace {};
