%#!/usr/bin/gprolog
% -----------------------------------------------
% Library for Interface to F.S and system
% -----------------------------------------------

read_line(Codes) :-
    get0(Code),
    (   Code < 0 /* end of file */ -> Codes = "sexit"
    ;   Code =:= 10 /* end of line */ -> Codes = []
    ;   Codes = [Code|Codes1],
        read_line(Codes1)
    ).

write_str(X):-
    write('\''),
    write(X),
    write('\'').

c :-
    write(',').

write_db(db(A,B,C,D,E)):-
    write('db('),
    write(A),c,
    write(B),c,
    write(C),c,
    write(D),c,
    write_str(E),
    write(').'),nl.

print_db([]).
print_db([X|Xs]):-
    write_db(X),
    print_db(Xs).

print_db :-
    findall((db(Stat, Prio, Root, Name, Desc)), (db(Stat, Prio,Root, Name, Desc)), Ds),
    print_db(Ds).


save :-
    telling(Old),
    tell('.todo.db'),
    %write(':- dynamic(db/5).'),nl,
    !,
    print_db,
    told,
    tell(Old).

create :-
    telling(Old),
    tell('.todo.db'),
    %write(':- dynamic db/5.'),nl,
    told,
    tell(Old).

       
string_tokens(Cs, Ts) :-
    phrase(tokens(Cs, []), Ts).
tokens([], Ts) --> token(Ts).
tokens([C|Cs], Ts) --> ( { C == 0' } -> token(Ts), tokens(Cs, []) ; tokens(Cs, [C|Ts])).

token([]) --> [].
token([T|Ts]) --> { reverse([T|Ts], Token), atom_codes(StrToken,Token) },  [StrToken]. 
%token([T|Ts]) --> { reverse([T|Ts], Token), string_to_list(StrToken,Token) },  [StrToken]. 

write_tab(0).
write_tab(N) :-
    M is N-1,
    write('\t'), write_tab(M).

% -----------------------------------------------

add(Parent, Name, Desc) :-
    assertz(db(todo,normal,Parent, Name, Desc)).

:- dynamic(db/5).
:- public(db/5).

load_db(File):-
    see(File),
    repeat,
    read(B),
    assertz(B),
    (  B = end_of_file ->
       true, !
    ; fail
    ),
    seen.

% -----------------------------------------------

process([]).

process([W,X,Y,Z|[]]) :-
    write_to_atom(Cmd, W),
    write_to_atom(SubCmd, X),
    write_to_atom(Arg, Y),
    write_to_atom(Rest, Z),
    command(Cmd, SubCmd, Arg, Rest).

process([X,Y,Z|[]]) :-
    write_to_atom(Cmd, X),
    write_to_atom(SubCmd, Y),
    write_to_atom(Arg, Z),
    command(Cmd, SubCmd, Arg).

process([X,Y|[]]) :-
    write_to_atom(Cmd, X),
    write_to_atom(SubCmd, Y),
    command(Cmd, SubCmd).

process([X|[]]) :-
    write_to_atom(Cmd, X),
    command(Cmd).

command(load, File) :-
    load_db(File).

command(list, Root) :-
    print_tree(Root, 0, print_todo).

command(list) :-
    print_tree(osu, 0, print_todo).

command(save) :-
    save.

command(create) :-
    create.

command(exit) :-
    halt.

command(sexit) :-
    save,
    halt.

command(help) :-
    write('add <parent> <name> -> Desc'),nl,
    write('rm <parent> <name> (even done)'),nl,
    write('delete <parent> <name> (todo)'),nl,
    write('done <parent> <name>'),nl,
    write('show <name>'),nl,
    write('list <name>'),nl,
    write('tag <parent> <name> <priority>'),nl,
    write('load <file>'),nl.

command(show) :-
    print_tree(osu, 0, print_leaf).

command(dump) :-
    listing(db/5).

command(List) :-
    write('unrec:'), write(List),nl.


command(add, Parent, Name) :- 
    write(Name),write('> '),
    read_line(List),
    atom_codes(SList, List),
    write_to_atom(AList, SList),
    add(Parent, Name, AList).

command(done, Parent, Name) :- 
    db(todo, Priority, Parent, Name,Desc),
    retractall(db(todo, Priority, Parent, Name, Desc)),
    assertz(db(done, Priority, Parent, Name,Desc)).

command(rm, Parent, Name) :- 
    retractall(db(_R, _Priority, Parent, Name,_)).

command(delete, Parent, Name) :- 
    retractall(db(todo, _Priority, Parent, Name,_)).

command(tag, Parent, Name, normal) :- 
        ncommand(tag, Parent, Name, normal).
command(tag, Parent, Name, important) :- 
        ncommand(tag, Parent, Name, important).
command(tag, Parent, Name, low) :- 
        ncommand(tag, Parent, Name, low).

ncommand(tag, Parent, Name, Priority) :- 
    db(Stat, Prio, Parent, Name,Desc),
    retractall(db(Stat, Prio, Parent, Name, Desc)),
    assertz(db(Stat, Priority, Parent, Name,Desc)).

print_tree(Root, N, Restrict) :-
    findall((Name, Prio, Desc, R), (db(R,Prio,Root, Name, Desc)), Ds),
    print_forest(Ds, N, Restrict).


print_leaf(N, Name, low, Desc, todo) :-
    write_tab(N),c_white(Name), write(': '), write(Desc),nl.

print_leaf(N, Name, important, Desc, todo) :-
    write_tab(N),c_byellow(Name), write(': '), write(Desc),nl.

print_leaf(N, Name, Prio, Desc, todo) :-
    write_tab(N),c_green(Name), write(': '), write(Desc),nl.

print_leaf(N, Name, Desc, Prio, todo) :-
    write_tab(N),c_green(Name), write(': '), write(Desc),nl.

print_leaf(N, Name, Prio, Desc, done) :-
    write_tab(N),c_blue(Name), write(': '), write(Desc),nl.

print_todo(N, Name, low, Desc, todo) :-
    write_tab(N),c_white(Name), write(': '), write(Desc),nl.

print_todo(N, Name, important, Desc, todo) :-
    write_tab(N),c_byellow(Name), write(': '), write(Desc),nl.

print_todo(N, Name, Prio, Desc, todo) :-
    write_tab(N),c_green(Name), write(': '), write(Desc),nl.

print_todo(_N, _Name, _Prio, _Desc, done).


print_forest([(Name, Prio, Desc, R)|Xs], N, Restrict) :-
    call(Restrict, N, Name, Prio, Desc, R),
    M is N+1,
    print_tree(Name, M, Restrict),
    print_forest(Xs, N, Restrict).

print_forest([],_, _).

do_command :-
    write('todo> '),
    read_line(List),
    string_tokens(List, LstLst),
    process(LstLst),
    !,
    do_command.


main:-
    load_db('.todo.db'),
    do_command.

c_red(Str):- write('[0;31m'), write(Str),write('[0m').
c_green(Str):- write('[0;32m'), write(Str),write('[0m').
c_yellow(Str):- write('[0;33m'), write(Str),write('[0m').
c_byellow(Str):- write('[1;33m'), write(Str),write('[0m').
c_blue(Str):- write('[0;34m'), write(Str),write('[0m').
c_magenta(Str):- write('[0;35m'), write(Str),write('[0m').
c_cyan(Str):- write('[0;36m'), write(Str),write('[0m').
c_white(Str):- write('[0;37m'), write(Str),write('[0m').

:- initialization(main).
