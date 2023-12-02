:-use_module(library(socket)).

client(Host,Port):-
    tcp_socket(Socket),
    tcp_connect(Socket, Host:Port, Pair),
    stream_pair(Pair,In,Out),
    write('Input:'),nl,
    read(Atom),
    atom_codes(Atom,String),
    format(Out,'~s~n',Atom),
    write(Out),
    read_line_to_codes(In, Command),
    atom_codes(Result,Command),
    write(Result).

