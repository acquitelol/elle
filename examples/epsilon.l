use std/io;
use std/rand;
use std/time;

const i32 ITERATIONS = 1e5;
const i32 ACCURACY = 1e6;
const f64 EPSILON = 0.1305;

fn roll(i32* results, i32 formattedEpsilon) {
    i32 attempts = 0;
    i32 roll = ACCURACY;

    while roll > formattedEpsilon {
        attempts += 1;
        roll = random(0, ACCURACY);
    }

    return attempts;
}

fn calculateAverage(i32* results) {
    i32 sum = 0;

    for i32 i = 0; i < ITERATIONS; i += 1 {
        sum += results[i];
    }

    return sum / ITERATIONS;
}

fn main() {
    i32 results[ITERATIONS];
    i32 formattedEpsilon = EPSILON * (ACCURACY / 100);

    srand(time(nil));

    for i32 i = 0; i < ITERATIONS; i += 1 {
        results[i] = roll(results, formattedEpsilon);
    }

    io::printf(
        "Rolled a {}% chance in an average of {} attempts\n",
        EPSILON,
        calculateAverage(results)
    );
}
