/*

  Seseman problem in ECLiPSe.

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
  - Excel/OpenOffice Scalc: http://www.hakank.org/oocalc_excel/seseman.xls
  - Python  : http://www.hakank.org/seseman/seseman.py 


  This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  Also, see my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:- lib(ic). 

% for the CGI version
go  :-
        argv(1,RowsumStr),
        argv(2,TotalStr),
        argv(3,FirstNumStr), % 0 or 1, empty room or not
        number_string(Rowsum, RowsumStr),
        number_string(Total, TotalStr),
        number_string(FirstNum, FirstNumStr),
        write(['Rowsum:', Rowsum,'Total',Total]),nl,
        findall(S, seseman(Rowsum, Total, FirstNum, S), L),
        (foreach(X, L) do write_convent(X),nl),
        length(L,Len),nl,
        write(Len),
        nl.

go2 :-
        go2_tmp(9,24,1).

go2_tmp(Rowsum, Total, FirstNum) :-
        write(['Rowsum:', Rowsum,'Total',Total]), nl,
        findall(S, seseman2(Rowsum, Total, FirstNum, S), L),
        (foreach(X, L) do write_convent(X),nl),
        length(L,Len),nl,
        write(Len),
        nl.


write_convent([A,B,C,D,E,F,G,H]) :-
        write([A,B,C]),nl,
        write([D,'_',E]), nl,
        write([F,G,H]),nl.


% tries to get no symmetric solutions
seseman2(Rowsum, Total, FirstNum, LD) :-
        LD = [A,B,C,D,E,F,G,H], % deklarera variablerna

        % FirstNum = 0: empty rooms allowed
        % FirstNum = 1: empty rooms not allowed
        LD :: [FirstNum..9],

        % Radsumma/Kolumnsumma
        A+B+C #= Rowsum,
        A+D+F #= Rowsum,
        C+E+H #= Rowsum,
        F+G+H #= Rowsum,

        % additional constraints for uniqueness (rotation, mirror)
        A #=< H,
        % B #=< D,
        % D #=< E,
        % E #=< G,

        % summan av alla tal = Total
        A+B+C+D+E+F+G+H #= Total,

        labeling(LD).



seseman(Rowsum, Total, FirstNum, LD) :-
        LD = [A,B,C,D,E,F,G,H], % deklarera variablerna

        % FirstNum = 0: empty rooms allowed
        % FirstNum = 1: empty rooms not allowed
        LD :: [FirstNum..9],

        % Radsumma/Kolumnsumma
        A+B+C #= Rowsum,
        A+D+F #= Rowsum,
        C+E+H #= Rowsum,
        F+G+H #= Rowsum,

        % summan av alla tal = Total
        A+B+C+D+E+F+G+H #= Total,

        labeling(LD).



