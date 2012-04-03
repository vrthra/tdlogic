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

portray(str(S)):- write('\''), write(S), write('\'').

print_arr([]).
print_arr([X|Xs]):-
    print(X), print('.'), nl,
    print_arr(Xs).

printit(db) :-
    findall(db(Root, Name, str(Desc)), db(Root, Name, Desc), Ds),
    print_arr(Ds).

printit(status) :-
    findall(status(Root, Name, Status), status(Root, Name, Status), Ss),
    print_arr(Ss).

printit(priority) :-
    findall(priority(Root, Name, Priority), priority(Root, Name, Priority), Ss),
    print_arr(Ss).


db_dir('.todo/').

pl_file(FName, File) :-
    db_dir(DbDir),
    atom_concat(DbDir, FName, File).

topl(db,File):-
    pl_file('db.pl', File).

topl(status,File):-
    pl_file('status.pl', File).

topl(priority,File):-
    pl_file('priority.pl', File).

notes_pl(File):-
    pl_file('notes.pl', File).

save(It) :-
    telling(Old),
    topl(It,Db),
    tell(Db),
    !,
    printit(It),
    told,
    tell(Old).

save :-
  save(db),
  save(status),
  save(priority).

touch(File) :- 
    telling(Old),
    tell(File),
    told,
    tell(Old).

create :-
    db_dir(DbDir),
    (file_exists(DbDir); make_directory(DbDir)),
    topl(db,Db),
    touch(Db),
    topl(priority,Prio),
    touch(Prio),
    topl(status,Stat),
    touch(Stat).

       
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
    assertz(db(Parent, Name, Desc)),
    assertz(status(Parent, Name, todo)),
    assertz(priority(Parent, Name, normal)).

:- dynamic([db/3, status/3, priority/3]).
:- public([db/3, status/3, priority/3]).

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
    db(Parent, Name, _),
    status(Parent, Name, todo),
    retractall(status(Parent, Name, todo)),
    assertz(status(Parent, Name, done)).

command(rm, Parent, Name) :- 
    retractall(db(Parent, Name,_)),
    retractall(status(Parent, Name, _)),
    retractall(priority(Parent, Name, _)).

command(delete, Parent, Name) :- 
    status(Parent, Name, todo),
    retractall(db(Parent, Name,_)),
    retractall(status(Parent, Name, todo)).

command(tag, Parent, Name, normal) :- 
        ncommand(tag, Parent, Name, normal).
command(tag, Parent, Name, important) :- 
        ncommand(tag, Parent, Name, important).
command(tag, Parent, Name, low) :- 
        ncommand(tag, Parent, Name, low).

ncommand(tag, Parent, Name, Priority) :- 
    db(Parent, Name,Desc),
    status(Parent, Name, Status),
    priority(Parent, Name, Prio),
    retractall(priority(Parent, Name, Prio)),
    assertz(priority(Parent, Name, Priority)).

print_tree(Root, N, Restrict) :-
    findall((Root, Name, Desc), db(Root, Name, Desc), Ds),
    print_forest(Ds, N, Restrict).


print_leaf(N, Root, Name, Desc) :- status(Root, Name, todo), priority(Root, Name, low),
    write_tab(N),c_white(Name), write(': '), write(Desc),nl.

print_leaf(N, Root, Name, Desc) :- status(Root, Name, todo), priority(Root, Name, important),
    write_tab(N),c_byellow(Name), write(': '), write(Desc),nl.

print_leaf(N, Root, Name, Desc) :- status(Root, Name, todo),
    write_tab(N),c_green(Name), write(': '), write(Desc),nl.

print_leaf(N, Root, Name, Desc) :-
    write_tab(N),c_blue(Name), write(': '), write(Desc),nl.

 
print_todo(N, Root, Name, Desc) :- status(Root, Name, todo), priority(Root, Name, low),
    write_tab(N),c_white(Name), write(': '), write(Desc),nl.

print_todo(N, Root, Name,Desc) :- status(Root, Name, todo), priority(Root, Name, important),
    write_tab(N),c_byellow(Name), write(': '), write(Desc),nl.

print_todo(N, Root, Name, Desc) :- status(Root, Name, todo),
    write_tab(N),c_green(Name), write(': '), write(Desc),nl.

print_todo(_N, Root, Name, _Desc) :- status(Root, Name, done).



print_forest([(Root, Name, Desc)|Xs], N, Restrict) :-
    call(Restrict, N, Root, Name, Desc),
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
    db_dir(DbDir),
    (file_exists(DbDir); create),
    topl(db,Db),
    load_db(Db),
    topl(status,Status),
    load_db(Status),
    topl(priority,Prio),
    load_db(Prio),
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
