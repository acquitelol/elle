use std/io;
use std/rand;
use std/time;

const i32 ITERATIONS = 100_000;
const i32 ACCURACY = 1_000_000;
const f64 EPSILON = 0.1305;

fn roll(i32* results, i32 formattedEpsilon) {
    i32 attempts = 0;
    i32 roll = ACCURACY;

    while roll > formattedEpsilon {
        attempts++;
        roll = random(0, ACCURACY);
    }

    return attempts;
}

fn calculateAverage(i32* results) {
    i32 sum = 0;

    for i32 i = 0; i < ITERATIONS; i++ {
        sum += results[i];
    }

    return sum / ITERATIONS;
}

fn main() {
    i32 results[ITERATIONS];
    i32 formattedEpsilon = EPSILON * (ACCURACY / 100);

    srand(time(NULL));

    for i32 i = 0; i < ITERATIONS; i++ {
        results[i] = roll(results, formattedEpsilon);
    }

    printf(
        "Rolled a %.4f%% chance in an average of %d attempts\n",
        EPSILON,
        calculateAverage(results)
    );
}
