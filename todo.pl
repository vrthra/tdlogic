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
    bagof(db(Name, str(Desc)), db(Name, Desc), Ds),
    print_arr(Ds).

printit(parent) :-
    bagof(parent(Parent, Name), parent(Parent, Name), Ds),
    print_arr(Ds).

printit(status) :-
    bagof(status(Name, Status), status(Name, Status), Ss),
    print_arr(Ss).

printit(priority) :-
    bagof(priority(Name, Priority), priority(Name, Priority), Ss),
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

topl(parent,File):-
    pl_file('parent.pl', File).

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
  save(parent),
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
    topl(db,Db),touch(Db),
    topl(parent,Rel),touch(Rel),
    topl(priority,Prio),touch(Prio),
    topl(status,Stat),touch(Stat).

       
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
    assertz(db(Name, Desc)),
    assertz(parent(Parent, Name)),
    assertz(status(Name, todo)),
    assertz(priority(Name, normal)).

:- dynamic([db/2, parent/2, status/2, priority/2]).
:- public([db/2, parent/2, status/3, priority/2]).

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

command(Cmd, Sub, Arg, Rest) :- 
    write('unrec'),nl.

command(done, Name) :- 
    status(Name, todo),
    retractall(status(Name, todo)),
    assertz(status(Name, done)).

command(rm, Name) :- 
    retractall(db(Name,_)),
    retractall(parent(_, Name)),
    retractall(status(Name, _)),
    retractall(priority(Name, _)).

command(delete, Name) :- 
    status(Name, todo),
    parent(Parent, Name),
    retractall(db(Name,_)),
    retractall(parent(Parent,Name)),
    retractall(priority(Name,_)),
    retractall(status(Name, todo)).


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
    write('rm <name> (even for done tasks)'),nl,
    write('delete <name> (works only with tasks with status todo)'),nl,
    write('done <name>'),nl,
    write('show <name>'),nl,
    write('list <name>'),nl,
    write('tag <name> <priority>'),nl,
    write('load <file>'),nl.

command(show) :-
    print_tree(osu, 0, print_leaf).

%command(dump) :-
%    listing(db/5).

command(List) :-
    write('unrec:'), write(List),nl.


command(add, Parent, UName) :-
    ulcaseatom(Name, UName),
    write(Name),
    (db(Name, Desc) -> (
      write(' Already used. '), write(': '), write(Desc), nl
    ) ; (
      write('> '),
      read_line(List),
      atom_codes(SList, List),
      write_to_atom(AList, SList),
      add(Parent, Name, AList)
    )).

command(tag, Name, normal) :- 
        ncommand(tag, Name, normal).
command(tag, Name, important) :- 
        ncommand(tag, Name, important).
command(tag, Name, low) :- 
        ncommand(tag, Name, low).

ncommand(tag, Name, Priority) :- 
    db(Name, Desc),
    status(Name, Status),
    priority(Name, Prio),
    retractall(priority(Name, Prio)),
    assertz(priority(Name, Priority)).

print_tree(Root, N, Restrict) :-
    findall((Name, Desc), (parent(Root, Name), db(Name, Desc)), Ds),
    print_forest(Ds, N, Restrict).


print_leaf(N, Name, Desc) :- status(Name, todo), priority(Name, low),
    write_tab(N),c_white(Name), write(': '), write(Desc),nl.

print_leaf(N, Name, Desc) :- status(Name, todo), priority(Name, important),
    write_tab(N),c_byellow(Name), write(': '), write(Desc),nl.

print_leaf(N, Name, Desc) :- status(Name, todo),
    write_tab(N),c_green(Name), write(': '), write(Desc),nl.

print_leaf(N, Name, Desc) :-
    write_tab(N),c_blue(Name), write(': '), write(Desc),nl.

 
print_todo(N, Name, Desc) :- status(Name, todo), priority(Name, low),
    write_tab(N),c_white(Name), write(': '), write(Desc),nl.

print_todo(N, Name,Desc) :- status(Name, todo), priority(Name, important),
    write_tab(N),c_byellow(Name), write(': '), write(Desc),nl.

print_todo(N, Name, Desc) :- status(Name, todo),
    write_tab(N),c_green(Name), write(': '), write(Desc),nl.

print_todo(_N, Name, _Desc) :- status(Name, done).

print_forest([(Name, Desc)|Xs], N, Restrict) :-
    call(Restrict, N, Name, Desc),
    M is N+1,
    print_tree(Name, M, Restrict),
    print_forest(Xs, N, Restrict).

print_forest([],_, _).

ulcaseatom(L, U) :-
    atom_chars(U, Us), % !!!! this is dependent on sequence.
    ulcase(Ls, Us),
    atom_chars(L, Ls). 

ulcase([L|Ls], [U|Us]) :-
    lower_upper(L, U),
    ulcase(Ls, Us).
ulcase([], []).


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
    topl(parent,Rel),
    load_db(Rel),
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
