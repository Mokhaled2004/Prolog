% ----------- GRID SETUP -----------
grid([
    ['D', '-', 'P', '-', 'O'],
    ['-', '-', 'O', 'P', '-'],
    ['P', 'O', '-', '-', '-'],
    ['-', '-', 'P', 'O', '-']
]).

% ----------- ENTRY POINT -----------
start :-
    grid(Mesh),
    start_position(Mesh, Start),
    dfs(Start, Mesh, [Start], Path),
    write('Path to visit most targets:'), nl,
    print_path(Path), nl,
    draw_states(Mesh, Path).

% ----------- FIND START POSITION -----------
start_position(Mesh, [X, Y]) :-
    nth0(Y, Mesh, Row), nth0(X, Row, 'D').

% ----------- DFS LOGIC -----------
dfs(Pos, Mesh, Visited, Visited) :-
    \+ (focal_target_node_position(Mesh, P), \+ member(P, Visited)),!.
dfs(Pos, Mesh, Visited, Path) :-
    move(Pos, Mesh, Next),
    \+ member(Next, Visited),
	
    dfs(Next, Mesh, [Next|Visited], Path).

% ----------- MOVEMENT RULES -----------
move([X,Y], Mesh, [NewX,Y]) :- NewX is X + 1, legitimate([NewX,Y], Mesh).
move([X,Y], Mesh, [NewX,Y]) :- NewX is X - 1, legitimate([NewX,Y], Mesh).
move([X,Y], Mesh, [X,NewY]) :- NewY is Y + 1, legitimate([X,NewY], Mesh).
move([X,Y], Mesh, [X,NewY]) :- NewY is Y - 1, legitimate([X,NewY], Mesh).

%Wat7a
legitimate([X,Y], Mesh) :-
    Y >= 0, X >= 0,
    nth0(Y, Mesh, Row),
    nth0(X, Row, Cell),
    Cell \= 'O'.

% ----------- TARGET LOCATOR -----------
focal_target_node_position(Mesh, [X,Y]) :-
    nth0(Y, Mesh, Row),
    nth0(X, Row, 'P').
%-----Print path -------------------
print_path(Path) :-
    reverse(Path, OrderedPath),
    print_path_forward(OrderedPath).

print_path_forward([]).
print_path_forward([[X,Y]|Rest]) :-
    write('Step: ('), write(X), write(','), write(Y), write(')'), nl,
    print_path_forward(Rest).


% ----------- STATE DRAWING (COLLECT FIRST THEN PRINT FORWARD) -----------

draw_states(Mesh, Path) :-
    collect_grids(Mesh, Path, [], Grids),
    print_grids(Grids).

collect_grids(_, [], Acc, Acc).
collect_grids(Mesh, [[X,Y]|Rest], Acc, Grids) :-
    capture_grid(Mesh, [X,Y], Rest, CapturedGrid),
    collect_grids(Mesh, Rest, [CapturedGrid|Acc], Grids).

capture_grid(Mesh, [DX,DY], Path, Captured) :-
    length(Mesh, Rows),
    capture_lines(Mesh, 0, Rows, [DX,DY], Path, [], Captured).

capture_lines(_, Y, Rows, _, _, Acc, Acc) :- Y >= Rows.
capture_lines(Mesh, Y, Rows, Pos, Path, Acc, Captured) :-
    nth0(Y, Mesh, Row),
    capture_line(Row, 0, Y, Pos, Path, [], Line),
    append(Acc, [Line], NewAcc),
    NewY is Y + 1,
    capture_lines(Mesh, NewY, Rows, Pos, Path, NewAcc, Captured).

capture_line([], _, _, _, _, Acc, Acc).
capture_line([Cell|Rest], X, Y, [PX,PY], Path, Acc, Line) :-
    (X = PX, Y = PY -> append(Acc, ['D'], NewAcc)
    ; member([X,Y], Path) -> append(Acc, ['+'], NewAcc)
    ; append(Acc, [Cell], NewAcc)),
    NewX is X + 1,
    capture_line(Rest, NewX, Y, [PX,PY], Path, NewAcc, Line).

print_grids([]).
print_grids([Mesh|Rest]) :-
    print_one_grid(Mesh),
    nl,
    print_grids(Rest).

print_one_grid([]).
print_one_grid([Row|Rest]) :-
    print_row(Row),
    nl,
    print_one_grid(Rest).

print_row([]).
print_row([Cell|Rest]) :-
    write(Cell),
    write(' '),
    print_row(Rest).

