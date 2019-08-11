/*

  Seseman problem in SWI Prolog

  Description of the problem:
  
  n is the length of a border
  There are (n-2)^2 "holes", i.e.
  there are n^2 - (n-2)^2 variables to find out.
 
  The simplest problem, n = 3 (n x n matrix)
  which is represented by the following matrix:
 
   a b c 
   d   e 
   f g h 
  
  Where the following constraints must hold:
 
    a + b + c = border_sum
    a + d + f = border_sum
    c + e + h = border_sum
    f + g + h = border_sum
    a + b + c + d + e + f = total_sum


  For a (Swedish) discussion of this problem, see
  "Sesemans matematiska klosterproblem samt lite Constraint Logic Programming"
  http://www.hakank.org/webblogg/archives/001084.html
  and
  Seseman's Convent Problem: http://www.hakank.org/seseman/seseman.cgi
  (using ECLiPSe CLP code)

  It was also is commented in the (Swedish) blog post
  "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
  http://www.hakank.org/webblogg/archives/001209.html
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :- 
        [Rowsum,Total,FirstNum] = [9,24,1],
        writeln([rowsum=Rowsum,total=total]),
        findall(S, seseman(Rowsum, Total, FirstNum, S),L),
        maplist(write_convent,L),
        length(L,Len),
        writeln(solutions=Len),
        nl.


%
% Using symmetry breaking (seseman2).
%
go2 :-
        Rowsum = 9,
        Total = 24,
        FirstNum = 1,
        writeln([rowsum=Rowsum,total=Total]),
        findall(S, seseman2(Rowsum, Total, FirstNum, S),L),
        maplist(write_convent,L),
        length(L,Len),
        writeln(solutions=Len).


write_convent([A,B,C,D,E,F,G,H]) :-
        format("~d ~d ~d~n",[A,B,C]),
        format("~d _ ~d~n",[D,E]),
        format("~d ~d ~d~n",[F,G,H]),
        nl.


% General constraints
constraints([A,B,C,D,E,F,G,H], Rowsum, Total) :-

        A+B+C #= Rowsum,
        A+D+F #= Rowsum,
        C+E+H #= Rowsum,
        F+G+H #= Rowsum,
        A+B+C+D+E+F+G+H #= Total.


%
% Plain problem (no symmetry breaking)
%
seseman(Rowsum, Total, FirstNum, LD) :-
        length(LD, 8),

        % FirstNum = 0: empty rooms allowed
        % FirstNum = 1: empty rooms not allowed
        LD ins FirstNum..9,
        constraints(LD, Rowsum, Total),
        label(LD).



%
% With symmetry breaking
%
seseman2(Rowsum, Total, FirstNum, LD) :-
        LD = [A,B,_C,D,E,_F,G,H],

        % FirstNum = 0: empty rooms allowed
        % FirstNum = 1: empty rooms not allowed
        LD ins FirstNum..9,

        % Row sums/Column sums
        constraints(LD, Rowsum, Total),

        % additional constraints for uniqueness (rotation, mirror)
        A #=< H,
        B #=< D,
        D #=< E,
        E #=< G,

        label(LD).


