/*

  AoC 2023 Day 1 in SWI Prolog



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

% :- use_module(library(clpfd)).

go :-
    part1,
    part2,
    nl.



part1 :-
    writeln("Part 1:"),
    File = "1.txt",
    parse_file(File,Lines),
    maplist(check1,Lines,Ns),
    sumlist(Ns,Sum),
    writeln(Sum),
    nl.

parse_file(File,Lines) :- 
    read_file_to_string(File,Str,[]),
    split_string(Str,"\n", "\n",Lines).


check1(Line,Num) :-
    name(Line,Cs),
    findall(D, (member(C,Cs),
                C >= 0'1, % '
                C =< 0'9, % '
                D is C - 0'0 % '
               ),
            Ns),
    Ns = [N1|_],
    last(Ns,N2),
    Num is 10*N1 + N2.

/*
  It's quite slow:
  ?- make,time(part2).
  Part 2:
  56324
% 12,115,107 inferences, 0.740 CPU in 0.742 seconds (100% CPU, 16364277 Lips)
*/
part2 :-
    writeln("Part 2:"),
    File = "1.txt",    
    % Collect the key's name/2 version, and the integer 
    findall([K,D],(m(Key,D),name(Key,K)),KeyLookup),
    parse_file(File,Lines),        
    maplist(check2(KeyLookup),Lines,Ns),
    sumlist(Ns,Sum),
    writeln(Sum),
    nl.

check2(KeyLookup,Line,Num) :-
    name(Line,S),        
    findall(D, (append([_,K,_],S),
                member([K,D],KeyLookup)
               ),
            Ns),
    Ns = [N1|_],
    last(Ns,N2),
    Num is 10*N1 + N2.       

m("one",1).
m("two",2).
m("three",3).
m("four",4).
m("five",5).
m("six",6).
m("seven",7).
m("eight",8).
m("nine",9).
m("1",1).
m("2",2).
m("3",3).
m("4",4).
m("5",5).
m("6",6).
m("7",7).
m("8",8).
m("9",9).
