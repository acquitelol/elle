use std/collections/tuple;
use std/io;

fn Tuple::capitalize(Tuple<string, string> self) -> Tuple<string, string> {
    return Tuple {
        first = self.first.capitalize(),
        second = self.second.capitalize(),
    };
}

struct Info {
    Tuple<string, string> name;
    Tuple<string, string> data;
};

fn Info::to_space_name(Info self) {
    Tuple<string, string> name = Tuple {
        first = self.name.second.slice(0, 3) <> self.name.first.slice(0, 2),
        second = self.data.second.slice(0, 3) <> self.data.first.slice(0, 2)
    };

    return name.capitalize();
}

fn main() {
    Info info = Info {
        name = Tuple {
            first = io::input("What is your first name? -> "),
            second = io::input("What is your last name? -> ")
        },
        data = Tuple {
            first = io::input("What is your birth month? -> "),
            second = io::input("What is your favourite animal? -> ")
        }
    };

    Tuple<string, string> space_name = info.to_space_name();
    io::printf("Your space name is {} {}", space_name.first, space_name.second);
}
