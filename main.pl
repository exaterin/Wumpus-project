:- use_module(library(readutil)).

% ---------- HELPER PREDICATES ----------

% Takes a fisrt element of a list 
head([H|_], H).

% Creates a nested list from the map 
world([], []).
world(Map, Result) :- Map = [MH | MR], split_string(MH, " ", " ", L), Result = [L | T], world(MR, T).

% Findes coordinates for an element
coordinates(LL, E, (X, Y)):- member(L, LL), member(E, L),
                            nth1(Y1, LL, L), nth0(X, L, E),
                            Y #= 4 - Y1.

% Items in a nested list 
list_coord(LL, E, R) :- findall(Coord, coordinates(LL, E, Coord), N), make_items(N, E, R).

% Creates a list of items for certain element
rename_item(item(E, (X, Y)), R) :- E = "w", R = item(wumpus, X, Y);
                              E = "s", R = item(player, X, Y);
                              E = "g", R = item(gold, X, Y);
                              E = "p", R = item(pit, X, Y).

make_items([], _, []).
make_items([L | T], E, R) :- rename_item(item(E, L), I), R = [I | R1 ], make_items(T, E, R1).


% Creates list with items 
items(W, I, F) :- maplist(list_coord(W), I, R), flatten(R, F).

% Creates a list of items for a given map from file "map.txt"
list_from_file(L) :- read_map(Map), world(Map, R), items(R, ["s", "w", "g", "p"], L), !.


% ---------- MOVEMENTS ----------

% Moves player to coordinates (DX, DY)
move_player(WorldStart, DX, DY, WorldResult) :-
    select(item(player, X, Y), WorldStart, World),
    X1 #= X + DX, Y1 #= Y + DY,
    \+ member(item(wumpus, X1, Y1), World),
    \+ member(item(pit, X1, Y1), World),
    subtract(World, [item(gold, X1, Y1)], W1),
    WorldResult = [item(player, X1, Y1) | W1].

% ---------- SEARCHING ----------

% Making a move
make_move(WorldStart, X, Y, N, WorldResult) :- (X1 #= X + 1, Y1 = Y;
                                            X1 #= X - 1, Y1 = Y;
                                            X1 = X, Y1 = Y + 1;
                                            X1 = X, Y1 = Y - 1),
                                            X1 #>= 0, Y1 #>= 0, X1 #=< N, Y1 #=< N,
                                            move_player(WorldStart, X1, Y1, WorldResult).
                                            
% Searching in space using BFS

% bfs(World, )
% bfs(L, N) :- findall(X, make_move(L, 0,0, N, X), Next),


% ---------- READING A MAP ----------

% Prints a map
print_list([]) :- nl.
print_list(List) :- List = [X | T], write(X), write(" "), print_list(T).

print_map(Lists) :- maplist(print_list, Lists).

% Read a map from a file: "map.txt"
read_map(Map):- 
    open("map.txt", read, Str), 
    read_map(Str, Map), 
    close(Str), 
    write("Game will be played on a map:"), nl,
    world(Map, List),
    print_map(List),  nl, !. 
    
read_map(Stream, []):- at_end_of_stream(Stream). 
    
read_map(Stream, [X | L]):- 
    \+ at_end_of_stream(Stream), 
    read_line_to_string(Stream, X), 
    read_map(Stream, L).