:- use_module(library(readutil)).

% ---------- HELPER PREDICATES ----------

% Returns the first element from the list
head([H | _], H).

prepend(L, X, [X | L]).

% Creates a nested list from the map 
world([], []).
world(Map, Result) :- Map = [MH | MR], split_string(MH, " ", " ", L), 
                    Result = [L | T], world(MR, T).

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

% Returns coordinates X, Y of player from the given state of a map
pos_player(M, X, Y) :- head(M, item(player, X, Y)).


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

% True if the map does not contain gold 
goal_test(M) :- \+ member(item(gold, _, _), M).
                                            
% Searching in space using BFS
% bfs(Map, N = length of the map Queue, Visited, Solution)

% stops when a map does not contain item(gold, _, _)
bfs(_, M, _, _) :- maplist(head, M , M1),maplist(goal_test, M1),
                    write(M), nl,
                    write(M1), nl.

bfs(N,[State | Queue], Visited, Solution) :- 
        pos_player(State, X, Y), % returns X, Y - coordinates of a player
        findall(S, make_move(State, X, Y, N, S), Next), % finds all possible states player can achieve 
        subtract(Next, Visited, Next1), % deletes from Next states which are in Visited set 
        maplist(prepend([State]), Next1, Next2), % prepend each state to the path
        append(Queue, Next2, Queue2), % adds all new states to queue
        append(Next1, Visited, Visited1), % adds all new states to visited set
        bfs(N, Queue2, Visited1, Solution).


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