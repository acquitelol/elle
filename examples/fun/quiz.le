use std/collections/array;
use std/io;
use std/rand;
use std/time;
use std/cast;

const i32 QUESTION_COUNT = 10;
const i32 LARGEST_INT = 12;

struct Question {
    string display;
    i32 answer;
    i32 input;
    i32 index;
};

fn Question::new(Array<Operation *> *operators, i32 index) {
    i32 lhs = rand::random(1, LARGEST_INT);
    i32 rhs = rand::random(1, LARGEST_INT);
    i32 operator = rand::random(0, operators.len() - 1);

    Operation *operation = operators.elements[operator];
    fn *func = operation.func;

    Question *question = malloc(#size(Question));
    *question = malloc(#size(Question));
    question.display = "What is {} {} {}? -> ".format(lhs, operation.display, rhs);
    question.answer = func(lhs, rhs);
    question.index = index;

    return question;
}

struct Operation {
    string display;
    void *func;
};

fn Operation::new(string display, fn *func) {
    Operation *op = malloc(#size(Operation));
    *op = malloc(#size(Operation));
    op.display = display;
    op.func = func;

    return op;
}

fn main() {
    rand::seed(time(nil));

    Array<Question *> *incorrect = Array::new();
    defer incorrect.free();

    Array<Operation *> *operations = Array::new(
        Operation::new("+", fn(i32 a, i32 b) -> a + b),
        Operation::new("-", fn(i32 a, i32 b) -> a - b),
        Operation::new("*", fn(i32 a, i32 b) -> a * b),
        Operation::new("/", fn(i32 a, i32 b) -> a / b) // Random numbers start at 1
    );
    defer operations.free();

    for i32 i = 0; i < QUESTION_COUNT; i += 1 {
        Question *question = Question::new(operations, i);
        string input = io::input("({}/{}) {}".format(i + 1, QUESTION_COUNT, question.display));
        i32 answer = 0;

        if !i32::try_parse(input, &answer) || answer != question.answer {
            question.input = answer;
            incorrect.push(question);
        }
    }

    i32 correct = QUESTION_COUNT - incorrect.len();
    io::printf("\nYou answered {}/{} questions correctly!", correct, QUESTION_COUNT);
    io::printf(
        "That's {}%. Congrats!",
        (i32)((f32)correct * 100 / QUESTION_COUNT)
    );

    if !incorrect.is_empty() {
        io::println("\nIncorrect questions:");
    }

    for i32 i = 0; i < incorrect.len(); i += 1 {
        Question *question = incorrect.elements[i];

        io::printf(
            "{}) The correct answer was {}, you wrote {}.",
            question.index + 1, question.answer, question.input
        );
    }
}
