/*

  Euler problem 26 in SWI Prolog

  """
  A unit fraction contains 1 in the numerator. The decimal representation of the 
  unit fractions with denominators 2 to 10 are given:

      1/2	= 	0.5
      1/3	= 	0.(3)
      1/4	= 	0.25
      1/5	= 	0.2
      1/6	= 	0.1(6)
      1/7	= 	0.(142857)
      1/8	= 	0.125
      1/9	= 	0.(1)
      1/10	= 	0.1

  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be 
  seen that 1/7 has a 6-digit recurring cycle.

  Find the value of d < 1000 for which 1/d contains the longest recurring cycle in 
  its decimal fraction part.
  """ 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler26a
            ],
        run_problems(L).

%%
%% 0.56s
%%
euler26a :-
        e26a(2,999,MaxD,MaxLen),
        writeln([d=MaxD,len=MaxLen]).

e26a(From,To,MaxD,MaxLen) :-
        e26a(From,To,0,MaxD,0,MaxLen).

e26a(From,To,MaxD, MaxD,MaxLen,MaxLen) :- From > To.
e26a(From,To,MaxD0,MaxD,MaxLen0,MaxLen) :-
         From1 is From + 1,        
        (
         is_prime(From)
        ->
         (
          get_rep_len(From, Len),
         ( Len > MaxLen0 ->
           MaxLen1 is Len,
           MaxD1 is From
         ;
           MaxLen1 is MaxLen0,
           MaxD1 is MaxD0
         ),
          e26a(From1,To,MaxD1,MaxD,MaxLen1,MaxLen)
         )
        ;
        e26a(From1,To,MaxD0,MaxD,MaxLen0,MaxLen)
        ).


%%
%% Get the length of the repeating cycle for 1/n
%%
/*
%% Picat code
get_rep_len(I,Len) =>
    FoundRemainders = [0 : _K in 1..I+1].to_array(),
    Value = 1,
    Position = 1,
    while (FoundRemainders[Value+1] == 0, Value != 0) 
        FoundRemainders[Value+1] := Position,
        Value := (Value*10) mod I,
        Position := Position+1
    end,
    Len = Position-FoundRemainders[Value+1].
*/
get_rep_len(I,Len) :-
        I1 is I+1,
        %% Create a list of I+1 elements, all are initially
        %% vars (will be filled out later)
        length(FoundRemainders,I1),
        Value0 is 1,
        Position0 is 1,
        get_rep_len(I,_Found,Value0,Value,Position0,Position,FoundRemainders),
        Value1 is Value + 1,
        nth1(Value1,FoundRemainders,FRV),
        Len is Position-FRV.


get_rep_len(_I,Found,Value,Value,Position,Position,_FoundRemainders) :-
        Found == true.

get_rep_len(I,Found, Value0,Value,Position0,Position,FoundRemainders) :-
        var(Found),
        nonvar(Value0),
        Value1 is Value0+1,
        nth1(Value1,FoundRemainders,FRV),
        (nonvar(FRV)
        ->
         get_rep_len(I,true,Value0,Value,Position0,Position,FoundRemainders)        
        ;
         var(FRV),
         nth1(Value1,FoundRemainders,Position0),
         Value2 is (Value0*10) mod I,
         Position1 is Position0 + 1,
         get_rep_len(I,Found,Value2,Value,Position1,Position,FoundRemainders)
        ).
