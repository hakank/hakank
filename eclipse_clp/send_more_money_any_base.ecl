/*

  SEND + MORE = MONEY (any base) in ECLiPSe.
 

  
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
  * Gecode/R: http://www.hakank.org/gecode_r/send_more_money_any_base.
rb


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/


*/

:- lib(ic).
:- lib(ic_global).

%
% Solve the SEND + MORE = MONEY problem in base Base
%
sendmore(Digits, Base) :-

        Digits = [S,E,N,D,M,O,R,Y],

        Base2 is Base - 1,

        % Assign a finite domain with each letter - S, E, N, D, M, O, R, Y -
        % in the list Digits
        Digits :: [0..Base2],

        % Constraints
        ic_global:alldifferent(Digits),
        S #> 0,
        M #> 0,
                      Base^3*S + Base^2*E + Base*N + D
        +             Base^3*M + Base^2*O + Base*R + E
        #= Base^4*M + Base^3*O + Base^2*N + Base*E + Y,
        
        % Search
        labeling(Digits).


%
% simple wrapper which gives all solutions
% 
smm(Base, L) :-
        findall([Base, Digits], sendmore(Digits, Base),L).



go :-
        Base = 10,
        smm(Base, L),
        writeln(L).

go2 :-
        % check the bases 1 to 30 and collect the number
        % of solutions in the list LL
        ( for(Base, 1, 30), foreach(Len, LL) do 
             smm(Base,L), 
             length(L, Len),
             writeln([Base, Len])
        ),
        writeln(LL).
        
        
