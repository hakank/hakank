/*

  Advent Of Code 2022 - Day 5 in SWI Prolog

  https://adventofcode.com/2022/day/5
 
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).

go :- 
        % File = "5_test.txt",
        File = "5.txt",

        % Separare the two segments
        read_file_to_string(File,Str,[]),
        re_matchsub("(.+?)\n\n(.+?)$"/s,Str,Sub),
        
        Starts1 = Sub.1,
        Procs = Sub.2,

        % Parse the start states, the init stack
        split_string(Starts1,"\n", "",StartLines0),
        maplist(string_chars,StartLines0,StartLines),
                
        length(StartLines,StartLen),
        StartLen1 is StartLen-1,
        length(StartLines1,StartLen1),
        append(StartLines1,_,StartLines),
        transpose(StartLines1,StartLinesT),

        maplist(keep_chars,StartLinesT,StartKeep),
        length(StartKeep,StartKeepLen),
        findall(T,(between(1,StartKeepLen,I),
                   I mod 4 =:= 2,
                   nth1(I,StartKeep,T)
                  ),Stack),

       
        % Parse the procedures
        split_string(Procs,"\n", "",Procs2),
        maplist(proc_re,Procs2,Procedures),
        
        member(Part,[1,2]),
        
        move(Procedures,Part,Stack,FinalStack),
        once(maplist(first,FinalStack,Sol1)),
        atom_chars(Sol,Sol1),
        writeln(Sol),
        
        Part == 2,

        
        nl.

first([X],X).
first([X|_],X).

% Part 1
move([],_Part,Stack,Stack).
move([[N,From,To]|Procs],Part,Stack,New) :-
    % from N element from stack From and place at stack To
    nth1(From,Stack,FromStack),
    pop_n(FromStack,N,Es,NewFromStack),
    nth1(To,Stack,ToStack),
    (Part == 1 ->
        reverse(Es,ToPush)
    ;
        ToPush = Es
    ),
    push(ToStack,ToPush,NewToStack),
    length(Stack,StackLen),
    update_stack(1,StackLen,Stack,From,NewFromStack,To,NewToStack,[],NewStack),
    move(Procs,Part,NewStack,New).

update_stack(Len1,Len,_OldStack,_From,_NewFrom,_To,_NewTo,NewStack,NewStack) :- Len1 > Len.
update_stack(I,StackLen,OldStack,From,NewFrom,To,NewTo,NewStack0,[NewFrom|NewStack]) :-
    I =:= From,
    I1 is I+1,
    update_stack(I1,StackLen,OldStack,From,NewFrom,To,NewTo,NewStack0,NewStack).
update_stack(I,StackLen,OldStack,From,NewFrom,To,NewTo,NewStack0,[NewTo|NewStack]) :-
    I =:= To,
    I1 is I+1,
    update_stack(I1,StackLen,OldStack,From,NewFrom,To,NewTo,NewStack0,NewStack).

update_stack(I,StackLen,OldStack,From,NewFrom,To,NewTo,NewStack0,[Old|NewStack]) :-
    I =\= From,
    I =\= To,
    nth1(I,OldStack,Old),
    I1 is I+1,
    update_stack(I1,StackLen,OldStack,From,NewFrom,To,NewTo,NewStack0,NewStack).


push(L,E,NewL) :-
    (is_list(E) -> 
        append(E,L,NewL)
    ;
        append([E],NewL)
    ).

pop_n(L,N,Es,NewL) :-
    length(Es,N),
    append(Es,NewL,L).

% Keep the Alpha chars from L (i.e. remove ' []')
keep_chars(L,Keep) :-
    keep_chars(L,[],Keep1),
    reverse(Keep1,Keep).
keep_chars([],L,L).
keep_chars([C|Cs],L0,L) :-
        (memberchk(C,[' ','[',']']) ->
         L1 = L0
        ;
         L1 = [C|L0]
        ),        
        keep_chars(Cs,L1,L).

% Regex to the rescue
proc_re(Line,[N,From,To]) :-
    re_matchsub("move (?<n_I>\\d+) from (?<from_I>\\d+) to (?<to_I>\\d+)",Line,Sub),
    N = Sub.n,
    From = Sub.from,
    To = Sub.to.
