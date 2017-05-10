import argparse
from assembler import Assembler
from generator import Session, Rel

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('file', help="ASM File", type=argparse.FileType('r'))
    parser.add_argument('--debug', action='store_true', help="Enable debug output")
    parser.add_argument('--stack', help="Stack size", type=int, default=8)
    parser.add_argument('--arg', help="ASM file arguments", nargs='*')
    parser.add_argument('--jump', help='Output subroutine jump instruction')
    parser.add_argument('--no-placement', help="Don't output placement",
                        action='store_true')
    parser.add_argument('--place-location', help="Location to place the blocks")
    parser.add_argument('--placer-location', default='~,~1,~')

    args = parser.parse_args()

    assembler = Assembler()
    with args.file as f:
        assembler.parse(f.read())

    sargs = {}
    if args.arg:
        for arg in args.arg:
            k, v = arg.split('=', 2)
            sargs[k] = v

    parse_pos = lambda p: Rel(int(p[1:]) if p[1:] else 0) if p[0] == '~' else int(p)

    if args.place_location:
        x, y, z = map(parse_pos, args.place_location.split(',', 3))
    else:
        x, y, z = Rel(1), Rel(), Rel()
    session = Session((x, y, z), stack_size=args.stack, args=sargs)
    session.dump = args.debug
    assembler.write_to_session(session)

    x, y, z = map(parse_pos, args.placer_location.split(',', 3))
    placement = session.output((x, y, z))

    if not args.no_placement:
        print(placement)

    if args.jump:
        from commands import SetConst, Var
        print('/' + SetConst(Var('func_pointer'),
                         assembler.subroutines[args.jump]).resolve(session.scope))
