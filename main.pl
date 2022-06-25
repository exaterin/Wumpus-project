:- use_module(library(readutil)).


% Read a map from "map.txt"

read(Map):- 
    open("map.txt", read, Str), 
    read_map(Str, Map), 
    close(Str), 
    format('~n~n~n   Game will be played on a map: ~d ~n~n~n', Map),
    write(Map),  nl. 
    
read_map(Stream, []):- at_end_of_stream(Stream). 
    
read_map(Stream, [X | L]):- 
    \+ at_end_of_stream(Stream), 
    read_line_to_string(Stream, X), 
    read_map(Stream, L).
