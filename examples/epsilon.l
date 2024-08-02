use std/io;
use std/rand;
use std/time;

const int ITERATIONS = 100_000;
const int ACCURACY = 1_000_000;
const double EPSILON = 0.1305;

fn roll(int* results, int formattedEpsilon) {
    int attempts = 0;
    int roll = ACCURACY;

    while roll > formattedEpsilon {
        attempts++;
        roll = random(0, ACCURACY);
    }

    return attempts;
}

fn calculateAverage(int* results) {
    int sum = 0;

    for int i = 0; i < ITERATIONS; i++ {
        sum += results[i];
    }

    return sum / ITERATIONS;
}

fn main() {
    int results[ITERATIONS];
    int formattedEpsilon = EPSILON * (ACCURACY / 100);

    srand(time(NULL));

    for int i = 0; i < ITERATIONS; i++ {
        results[i] = roll(results, formattedEpsilon);
    }

    printf(
        "Rolled a %.4f%% chance in an average of %d attempts\n",
        EPSILON,
        calculateAverage(results)
    );
}
