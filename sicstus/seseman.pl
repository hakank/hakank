/*

  Seseman problem in SICStus Prolog.

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


  For a (swedish) discussion of this problem, see
  "Sesemans matematiska klosterproblem samt lite Constraint Logic Programming"
  http://www.hakank.org/webblogg/archives/001084.html
  and
  Seseman's Convent Problem: http://www.hakank.org/seseman/seseman.cgi
  (using Eclipse code)

  It was also is commented in the (swedish) blog post
  "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
  http://www.hakank.org/webblogg/archives/001209.html
  

  Also compare with other models:
  - MiniZinc: http://www.hakank.org/minizinc/seseman.mzn
  - JaCoP   : http://www.hakank.org/JaCoP/Seseman.java
  - Choco   : http://www.hakank.org/Choco/Seseman.java
  - Comet   : http://www.hakank.org/comet/seseman.co
  - Gecode/R: http://www.hakank.org/gecode_r/seseman.rb
  - Gecode  : http://www.hakank.org/gecode/seseman.cpp
  - ECLiPSe : http://www.hakank.org/eclipse/seseman.ecl
  - Tailor/Essence': http://www.hakank.org/tailor/seseman.eprime
  - Excel/OpenOffice Scalc: http://www.hakank.org/oocalc_excel/seseman.xls
  - Python  : http://www.hakank.org/seseman/seseman.py 


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        go2_tmp(9,24,1).

go2_tmp(Rowsum, Total, FirstNum) :-
        write(['Rowsum:', Rowsum,'Total',Total]), nl,
        findall(S, seseman(Rowsum, Total, FirstNum, S), L),
        (foreach(X, L) do write_convent(X),nl),
        length(L,Len),
        format('Number of solutions: ~d', Len),
        nl.


write_convent([A,B,C,D,E,F,G,H]) :-
        write([A,B,C]),nl,
        write([D,'_',E]), nl,
        write([F,G,H]),nl.



seseman(Rowsum, Total, FirstNum, LD) :-
        LD = [A,B,C,D,E,F,G,H],

        % FirstNum = 0: empty rooms allowed
        % FirstNum = 1: empty rooms not allowed
        domain(LD, FirstNum, 9),

        A+B+C #= Rowsum,
        A+D+F #= Rowsum,
        C+E+H #= Rowsum,
        F+G+H #= Rowsum,

        % symmetry breaking
        A #=< H,
        % B #=< D,
        % D #=< E,
        % E #=< G,

        A+B+C+D+E+F+G+H #= Total,

        labeling([],LD).
