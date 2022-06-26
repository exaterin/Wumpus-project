:- use_module(library(readutil)).

% ---------- HELPER PREDICATES ----------

% Takes a fisrt element of a list 
head([H|_], H).

% Creates a nested list from the map 
world([], []).
world(Map, Result) :- Map = [MH | MR], split_string(MH, " ", " ", L), Result = [ L | T], world(MR, T).

% Findes coordinates for an element 
coordinates(LL, E, (X, Y)):- member(L, LL), member(E, L),
                            nth1(Y1, LL, L), nth0(X, L, E),
                            Y #= 4 - Y1.

% Usage: read_map(_Map), world(_Map, _R), coordinates(_R, "p", X, Y).

% Replaces element at the place N with  A
replace_l([_|L],[A|L], A, 0).
replace_l([X|L],[X|R], A, N) :- N1 #= N - 1, replace_l(L, R, A, N1).

% Replaces element at the coordinats (X, Y) with A
replace(World1, A, (X,Y), World2) :- X #= X1, Y #= 3 - Y1, 
                            nth0(Y1, World1, NL), replace_l(NL,R,A,X1), replace_l(World1,World2,R,Y1).

% ---------- MOVEMENTS ----------

move_player(WorldStart, east, WorldResult) :- coordinates(WorldStart, "s", (X, Y)), 
                                        X1 #= X + 1, replace(WorldStart, "s", (X1, Y), World1),
                                        replace(World1, ".", (X, Y), WorldResult).

move_player(WorldStart, south, WorldResult) :- coordinates(WorldStart, "s", (X, Y)), 
                                        Y1 #= Y - 1, replace(WorldStart, "s", (X, Y1), World1),
                                        replace(World1, ".", (X, Y), WorldResult).

move_player(WorldStart, west, WorldResult) :- coordinates(WorldStart, "s", (X, Y)), 
                                        X1 #= X - 1, replace(WorldStart, "s", (X1, Y), World1),
                                        replace(World1, ".", (X, Y), WorldResult).

move_player(WorldStart, north, WorldResult) :- coordinates(WorldStart, "s", (X, Y)), 
                                        Y1 #= Y + 1, replace(WorldStart, "s", (X, Y1), World1),
                                        replace(World1, ".", (X, Y), WorldResult).



% ---------- READING A MAP ----------

% Read a map from a file: "map.txt"
read_map(Map):- 
    open("map.txt", read, Str), 
    read_map(Str, Map), 
    close(Str), 
    write("Game will be played on a map:"), nl,
    world(Map, List),
    write(List),  nl, !. 
    
read_map(Stream, []):- at_end_of_stream(Stream). 
    
read_map(Stream, [X | L]):- 
    \+ at_end_of_stream(Stream), 
    read_line_to_string(Stream, X), 
    read_map(Stream, L).
