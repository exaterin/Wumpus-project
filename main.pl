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
coordinates(N, LL, E, (X, Y)):- member(L, LL), member(E, L),
                            nth1(Y1, LL, L), nth0(X, L, E),
                            Y #= N - Y1.

% Items in a nested list 
list_coord(M, LL, E, R) :- findall(Coord, coordinates(M, LL, E, Coord), N), make_items(N, E, R).

% Creates a list of items for certain element
rename_item(item(E, (X, Y)), R) :- E = "w", R = item(wumpus, X, Y);
                              E = "s", R = item(player, X, Y);
                              E = "g", R = item(gold, X, Y);
                              E = "p", R = item(pit, X, Y).

make_items([], _, []).
make_items([L | T], E, R) :- rename_item(item(E, L), I), R = [I | R1 ], make_items(T, E, R1).


% Creates list with items 
items(N, W, I, F) :- maplist(list_coord(N, W), I, R), flatten(R, F).

% Creates a list of items for a given map from file "map.txt"
list_from_file(N, L) :- read_map(Map), world(Map, R), items(N, R, ["s", "w", "g", "p"], L), !.

% Returns coordinates X, Y of player from the given state of a map
pos_player(M, X, Y) :- head(M, item(player, X, Y)).


% Prints a path

print_path(L1, N) :- sleep(0.5), L1 = [L | []], create_map(L, N, N, R), print_map(R), nl, write("Player climbs out from the cave "), nl, nl.

print_path(P, N) :- sleep(0.5), P = [H |T], T = [T1 | _], create_map(H, N, N, R), print_map(R), nl,
                ( (member(item(gold, X1, Y1), H), member(item(player, X1, Y1), T1), 
                        write("Player grabs a gold "), nl, nl, print_path(T, N));
                  (member(item(wumpus, X1, Y1), H), member(item(player, X1, Y1), T1), 
                        write("Player kills Wumpus "), nl, nl, print_path(T, N));
                  print_path(T, N)).

                % ), print_path(T, N), !.

% Creates a map from the given list  

create_list(_, 0, _, _).
create_list(L, N, X, R) :- length(R, N), N1 #= N - 1, (
                (member(item(player, N1, X), L), R = ["s" | T]);
                (member(item(wumpus, N1, X), L), R = ["w" | T]);
                (member(item(gold, N1, X), L), R = ["g" | T]);
                (member(item(pit, N1, X), L), R = ["p" | T]);
                (\+member(item(player, N1, X), L), R = ["." | T])
                ), create_list(L, N1, X, T), !.

create_map(_, 0, _, R) :- length(R, 0), !.
create_map(M, X, Y, R) :- X1 #= X - 1, create_list(M, Y, X1, R1), reverse(R1, R11), R = [ R11 | R2], create_map(M, X1, Y, R2).

% ---------- MOVEMENTS ----------

% Moves player to direction (DX, DY)
move_player(WorldStart, N, DX, DY, WorldResult) :-
            select(item(player, X, Y), WorldStart, World),
            X1 #= X + DX, Y1 #= Y + DY,
            X1 #< N, Y1 #< N, X1 #>= 0, Y1 #>= 0,
            \+ member(item(pit, X1, Y1), World),
            subtract(World, [item(gold, X1, Y1), item(wumpus, X1, Y1)], W1),
            WorldResult = [item(player, X1, Y1) | W1].

% ---------- SEARCHING ----------

% Making a move - finds all possible moves for the player
make_move(WorldStart, N, WorldResult) :- move_player(WorldStart, N, 1, 0, WorldResult);
                                        move_player(WorldStart, N, -1, 0, WorldResult);
                                        move_player(WorldStart, N, 0, 1, WorldResult);
                                        move_player(WorldStart, N, 0, -1, WorldResult).

% True if the map does not contain gold and player is at the position (0,0)
goal_test(M) :- \+ member(item(gold, _, _), M), head(M, item(player, 0,0)).
                                            
% Searching in space using BFS
% bfs(N = length of the map, Queue, Visited, Solution)

% stops when a map does not contain item(gold, _, _)
bfs(_, [[Goal | Rest] | _], _, [Goal | Rest]) :- goal_test(Goal).

bfs(N,[[State | Path] | Queue], Visited, Solution) :- 
        findall(S, make_move(State, N, S), Next), % finds all possible states player can achieve
        subtract(Next, Visited, Next1), % deletes from Next states which are in Visited set 
        maplist(prepend([State | Path]), Next1, Next2), % prepend each state to the path
        append(Queue, Next2, Queue2), % adds all new states to queue
        append(Next1, Visited, Visited1), % adds all new states to visited set
        bfs(N, Queue2, Visited1, Solution), !.


% ---------- READING A MAP ----------

% Prints a map
print_list([]) :- nl.
print_list(List) :- List = [X | T], write(X), write(" "), print_list(T).

print_map(Lists) :- maplist(print_list, Lists).

% Read a map from a file: "map.txt"
read_m(List, File):- 
                open(File, read, Str), 
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

% ---------- MAIN ----------

% F is a name of file with a map
run(F) :- (read_m(R, F), length(R, N), items(N, R, ["s", "w", "g", "p"], L), bfs(N,[[L]], [L], Solution), 
                        reverse(Solution, Reversed), sleep(1),
                        write("Path:"), nl,
                        print_path(Reversed, N) ; write("There is no solution :(")), !.
                        