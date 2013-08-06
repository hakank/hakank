/*

  SEND + MORE = MONEY (any base) in SICStus Prolog.

  The number of solutions for base 10 to 30 is:
  
   [eclipse]: for(I,10,30) do findall(Digits,sendmore(Digits, I), L), length(L, Len), writeln(Len).
     1
     3
     6
    10
    15
    21
    28
    36
    45
    55
    66
    78
    91
   105
   120
   136
   153
   171
   190
   210
   231

  This is the triangular number sequence:
  http://www.research.att.com/~njas/sequences/?q=1+3+6+10+15+21+28+36+45+55+66+&language=english&go=Search

  I blogged about this relation in
  "Some other Gecode/R models, mostly recreational mathematics"
  http://www.hakank.org/constraint_programming_blog/2009/01/some_other_gecoder_models_most_1.html


  Compare with these other models:
  * MiniZinc: http://www.hakank.org/minizinc/send_more_money_any_base.mzn
  * Comet   : http://www.hakank.org/comet/send_more_money_any_base.co
  * Gecode  : http://www.hakank.org/gecode/send_more_money_any_base.cpp
  * Gecode/R: http://www.hakank.org/gecode_r/send_more_money_any_base.rb
  * ECLiPSe: http://www.hakank.org/eclipse/send_more_money_any_base.ecl
  * Tailor/Essence': http://www.hakank.org/tailor/send_more_money_any_base.eprime

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%
% Base 10:
% [9,5,6,7,1,0,8,2]
%
go:-
        sendmory(A,10),
        write(A),nl.

% check the bases 1 to 30
go2 :-
        ( for(Base, 1, 30), 
          foreach(Len, LL) 
        do 
              smm(Base,L), 
              length(L, Len),
              write([Base, Len]),nl
        ),
        write(LL), nl.


% simple wrapper which gives all solutions
smm(Base, L) :-
        findall([Base, Digits], sendmory(Digits, Base),L).


sendmory(Vars, Base):-
        Vars=[S,E,N,D,M,O,R,Y],
        Base1 is Base-1,
        domain(Vars, 0, Base1),
        all_different(Vars),
        S #\= 0,
        M #\= 0,

        SEND_P = [3,2,1,0],
        MONEY_P = [4,3,2,1,0],
        ( foreach(SP,SEND_P),
          foreach(SS,SEND),
          param(Base)
        do
          SS is integer(Base**SP)
        ), 

        ( foreach(MP,MONEY_P),
          foreach(MM,MONEY),
          param(Base)
        do
          MM is -integer(Base**MP)
        ), 

        append([SEND,SEND,MONEY], SEND_MORE_MONEY),
        scalar_product(SEND_MORE_MONEY,
                       [  S,E,N,D,
                          M,O,R,E,
                        M,O,N,E,Y],#=,0),

        % This is the principle (for base 10):
        % scalar_product([        1000, 100, 10, 1,
        %                         1000, 100, 10, 1,
        %                 -10000,-1000,-100,-10,-1], 
        %                [S,E,N,D,
        %                 M,O,R,E,
        %                 M,O,N,E,Y],#=,0),

        labeling([ff],Vars).
