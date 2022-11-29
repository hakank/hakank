/*

  SEND+MORE=MONEY in any base puzzle in SWI Prolog

  Solve the alphametic problem 
    SEND+MORE=MONEY
  using distinct digits for the letters and in any base.

  Cf send_more_money.pl for some other approaches (for base 10).
 
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
    Base = 10,
    send_more_money_any_base(Base,Digits),
    labeling([ff],Digits),
    writeln(Digits),       
    nl.

% All 231 solutions for Base=30
go2 :-
    Base = 30,
    send_more_money_any_base(Base,Digits),
    labeling([ff],Digits),
    writeln(Digits),
    fail,
    nl.


/*
  Number of solutions for Base=10..100.

  Here's the solutions for Base=10..30:
   Base #solutions
   --------------
   10:1
   11:3
   12:6
   13:10
   14:15
   15:21
   16:28
   17:36
   18:45
   19:55
   20:66
   21:78
   22:91
   23:105
   24:120
   25:136
   26:153
   27:171
   28:190
   29:210
   30:231


  It's the triangular number sequence:
  https://oeis.org/A000217
  https://en.wikipedia.org/wiki/Triangular_number

  I blogged about this relation in
  "Some other Gecode/R models, mostly recreational mathematics"
  http://www.hakank.org/constraint_programming_blog/2009/01/some_other_gecoder_models_most_1.html

*/
go3 :-
    % check the bases 10 to 30 and print the number
    % of solutions
    numlist(10,100,Bases),
    member(Base,Bases),
    findall(Digits,(send_more_money_any_base(Base,Digits),labeling([ffc,enum],Digits)),L),
    % writeln(L),
    length(L,Len),
    writeln(Base:Len),
    fail.
        

send_more_money_any_base(Base,Digits) :-
    Digits = [S,E,N,D,M,O,R,Y],
    Base1 is Base-1,
    Digits ins 0..Base1,
    
    all_different(Digits),
    S #> 0,
    M #> 0,
    Base^3*S + Base^2*E + Base*N + D
    +      Base^3*M + Base^2*O + Base*R + E
    #= Base^4*M + Base^3*O + Base^2*N + Base*E + Y.

