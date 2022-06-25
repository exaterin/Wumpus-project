main:- 
    open("map.txt", read, Str), 
    read_map(Str, Map), 
    close(Str), 
    write(Map),  nl. 
    
read_map(Stream, []):- at_end_of_stream(Stream). 
    
read_map(Stream, [X | L]):- 
    \+ at_end_of_stream(Stream), 
    read(Stream, X), 
    read_map(Stream, L).