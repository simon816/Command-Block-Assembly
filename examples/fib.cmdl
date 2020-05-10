include "Text"
include "Game"

int x;
int y;
int counter;
int old_x;

async void main() {
    x = 0;
    y = 1;
    counter = 1;
    do {
        Text t;
        t << "fib(";
        t.append_ref(counter);
        t << ") = ";
        t.append_ref(x);
        t.send_to_all();
        counter++;
        await Game.tick();
        old_x = x;
        x = y;
        y += old_x;
    } while(x >= 0);
}
