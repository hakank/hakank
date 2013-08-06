/*

  SEND+MORE=MONEY in B-Prolog.

  Standard problem: assign distinct digits 0..9 to the letters
  so that the equation is true:
     SEND+MORE=MONEY

  The more general problem (SEND+MORE=MONEY in "any base") 
  generates this number of solutions for base 10 to 30:
  
    [1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210,231]

  This is the triangular number sequence:
  http://www.research.att.com/~njas/sequences/?q=1+3+6+10+15+21+28+36+45+55+66+&language=english&go=Search

  I blogged about this relation in
  "Some other Gecode/R models, mostly recreational mathematics"
  http://www.hakank.org/constraint_programming_blog/2009/01/some_other_gecoder_models_most_1.html


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        sendmore(Digits),
        writeln(Digits).

go2 :- sendmore2(Digits),
        writeln(Digits).


go3 :-
        Base = 10,
        sendmore_base(Digits, Base),
        writeln(Digits).


% SEND+MORE=MONEY in any base
go4 :-
        foreach(Base in 2..30,
                [Digits,All, Len],
                ac(Lens,[]),
                (
                    nl,
                    writeln(base:Base),
                    findall(Digits, sendmore_base(Digits, Base),All) 
                -> 
                    length(All, Len),
                    writeln(All),
                    writeln(len:Len),
                    Lens^1 = [Len|Lens^0]
                ;
                    Lens^1 = [0|Lens^0],
                    true
                )
               ),
        reverse(Lens,Lens2),
        format('All lengths: ~q\n', [Lens2]).


%
% Standard problem (in base 10)
%
sendmore(Digits) :-

        Digits = [S,E,N,D,M,O,R,Y],
        Digits :: 0..9,

        % Constraints
        alldifferent(Digits),
        S #> 0,
        M #> 0,
                     1000*S + 100*E + 10*N + D
        +            1000*M + 100*O + 10*R + E
        #= 10000*M + 1000*O + 100*N + 10*E + Y,
        
        % Search
        % writeln('Digits before search:'),
        % writeln(Digits),
        labeling(Digits).


%
% Using scalar_product
%
sendmore2(Digits) :-

        Digits = [S,E,N,D,M,O,R,Y],
        Digits :: 0..9,

        % Constraints
        alldifferent(Digits),

        S #> 0,
        M #> 0,

        Base4 = [1000,100,10,1],
        Base5 = [10000,1000,100,10,1],
        scalar_product(Base4, [S,E,N,D],#=, SEND),
        scalar_product(Base4, [M,O,R,E],#=, MORE),
        scalar_product(Base5, [M,O,N,E,Y],#=, MONEY),
        SEND + MORE #= MONEY,
        
        % Search
        labeling(Digits).





%
% Solve the SEND + MORE = MONEY problem in base Base
%
sendmore_base(Digits, Base) :-

        Digits = [S,E,N,D,M,O,R,Y],
        Base2 is Base - 1,
        Digits :: 0..Base2,

        % Constraints
        alldifferent(Digits),
        S #> 0,
        M #> 0,
                       Base**3*S + Base**2*E + Base*N + D
        +              Base**3*M + Base**2*O + Base*R + E
        #= Base**4*M + Base**3*O + Base**2*N + Base*E + Y,
        
        % Search
        % writeln('Digits before search:'),
        % writeln(Digits),
        labeling(Digits).

