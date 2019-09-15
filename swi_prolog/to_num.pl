/*

  to_num in SWI Prolog.

  to_num(List, Base, Num) converts a list of integers to a number for 
  a base Base. It is bidirectional but it is really recommended that
  the length of List is fixed.

  See examples below.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%
% Tests
% 
go :-

        writeln("from number -> digit list"),
        length(A,4),
        A ins 0..9,
        to_num(A, 10, 1234),
        writeln(a=A),

        writeln("from digit list -> number"),
        B = [3,1,4,1,5,9,2,6],
        to_num(B, 10, Num),
        label([Num]),
        writeln(num=Num),

        writeln("show all 2 digit numbers in base 11"),
        length(C,2), 
        C ins 0..10,  % For base 11
        findall([Num2, C], (to_num(C, 11, Num2), labeling([ff],C)),L),
        length(L,LLen),
        writeln([LLen, L]).


go2 :-
        length(L,4),
        L = [_A,B,C,D],
        L ins 0..9,       
        Num in 0..9999,
        
        to_num(L, 10, Num),

        Num #> 5000,
        Num mod 3 #= 1,
        
        B + C #= D,
        all_different(L),
        sum(L, #=, 22),
        
        append(L,[Num], Vars),
        label(Vars),
        writeln([l=L,num=Num]).


%%
%% to_num(List, Base, Num)
%%
%% sum(List) #= Num
%%
/*
to_num(List, Base, Num) :-
        length(List,Len),
        to_num_(List,1,Len,Base,0, Num).

% Num #= sum([List[I]*Base**(Len-I) : I in 1..Len]).
to_num_([],_I,_Len,_Base,Num,Num).
to_num_([H|T],I,Len,Base,Num0,Num) :-
        Len1 #= Len-I,
        Num1 #= Num0 + H*(Base^Len1),
        I1 #= I+1,
        to_num_(T,I1,Len,Base,Num1,Num).
*/