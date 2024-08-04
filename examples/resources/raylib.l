global pub;

def AudioStream {
    void *buffer;
    void *processor;

    i32 sampleRate;
    i32 sampleSize;
    i32 channels;
}

def Sound {
    AudioStream stream;
    i32 frameCount;
}

def Vector2 {
    f32 x;
    f32 y;
}

def Color {
    u32 r;
    u32 g;
    u32 b;
    u32 a;
}

external fn InitWindow(i32 width, i32 height, string title);
external fn CloseWindow();
external fn WindowShouldClose() -> bool;
external fn BeginDrawing();
external fn EndDrawing();
external fn SetTargetFPS(i32 fps);
external fn GetColor(i32 hexValue) -> Color;
external fn ColorFromHSV(f32 hue, f32 saturation, f32 value) -> Color;
external fn ClearBackground(Color color);
external fn DrawCircle(i32 centerX, i32 centerY, f32 radius, Color color);
external fn DrawRectangleV(Vector2 position, Vector2 size, Color color);
external fn DrawText(string text, i32 x, i32 y, i32 fontSize, Color color);
external fn GetFrameTime() -> f32;
external fn GetScreenWidth() -> i32;
external fn GetScreenHeight() -> i32;
external fn MeasureText(string text, i32 fontSize) -> i32;
external fn LoadSound(string filename) -> Sound;
external fn UnloadSound(Sound sound);
external fn PlaySound(Sound sound);
external fn InitAudioDevice();
external fn CloseAudioDevice();
