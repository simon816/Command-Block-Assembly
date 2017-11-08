from compiler.compiler import Compiler
from compiler.lexer import Lexer
from compiler.parser_ import Parser


if __name__ == '__main__':
    c = Compiler()
    code="""
void main() {
    int x = 0;
    int y = 1;
    int old_x;
    int counter = 1;
    do {
        printf("fib(%d) = %d", counter++, x);
        sync;
        old_x = x;
        x = y;
        y += old_x;
    } while(x >= 0);
}
"""
    parser = Parser(Lexer(code))
    program = parser.parse_program()

    #for stmt in program:
    #    print(stmt.print_node())

    compiled = c.compile_program(program)
    print(compiled)
    from assembler import Assembler
    from session import Session, DummyWriter
    assembler = Assembler()
    assembler.enable_sync = True
    assembler.parse(compiled)
    writer = DummyWriter()
    session = Session((0,0,0), writer, 'c_generated', debug=True, stack_size=1)
    assembler.write_to_session(session)
