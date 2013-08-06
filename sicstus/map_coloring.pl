/*

  Map coloring in SICStus Prolog.

  Original example from "The OPL Programming Language", pages 7 & 42.


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/color_simple.mzn
  * MiniZinc: http://www.hakank.org/minizinc/map.mzn
  * Comet   : http://www.hakank.org/comet/country_cp.co
  * Comet   : http://www.hakank.org/comet/map.co
  * Gecode/R: http://www.hakank.org/gecode_r/map.rb
  * Gecode  : http://www.hakank.org/gecode/map.cpp
  * ECLiPSe : http://www.hakank.org/eclipse/map.ecl

  

  This model was created by Hakan Kjellerstrand, hakank@bonetmail.com .
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


countries([france, belgium, luxembourg, germany, netherlands,denmark]).

% the connections between the countries
c(france, belgium).
c(france, luxembourg).
c(france, germany).
c(luxembourg, germany).
c(luxembourg, belgium).
c(netherlands, belgium).
c(germany, belgium).
c(germany, netherlands).
c(germany, denmark).
        

go :-
        countries(Countries),
        length(Countries,Len),
        length(Colors, Len),
        domain(Colors, 1, 4),

        findall([C1,C2],c(C1,C2), Connections),

        % Ensure that connecting countries has
        % different colors
         (
           foreach([N1,N2], Connections),
           param(Countries,Colors)
         do
           % Find the index of the countries and
           % the corresponding color.
           nth1(Ix1,Countries,N1),
           nth1(Ix2,Countries,N2),

           element(Ix1,Colors,C1),
           element(Ix2,Colors,C2),
           
           C1 #\= C2
         ),

        % symmetry breaking: color of first country (france) is 1
        [Col1|_] = Colors,
        Col1 #= 1,

        labeling([ff], Colors),
        
        write(Colors), nl,
        (
            foreach(CS,Countries),
            foreach(C, Colors)
        do
            write(CS:C),nl
        ),
        nl,
        fd_statistics.

