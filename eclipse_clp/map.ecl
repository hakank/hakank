/*

  Map coloring problem in ECLiPSe.

  Simple map coloring. 
  Original example from "The OPL Programming Language", pages 7 & 42.


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/color_simple.mzn
  * MiniZinc: http://www.hakank.org/minizinc/map.mzn
  * Comet   : http://www.hakank.org/comet/country_cp.co
  * Comet   : http://www.hakank.org/comet/map.co
  * Gecode/R: http://www.hakank.org/gecode_r/map.rb
  * Gecode  : http://www.hakank.org/gecode/map.cpp


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).

:- local struct(country(name,color)).

% the countries
countries([france, belgium, luxembourg, germany, netherlands, denmark]).

% the connections between countries
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
        % fetch the country names
        countries(Countries),

        % Loop through each country (name) and create a country struct
        % with name and color (decision variable). 
        % Collect the structures in Countries2.
        % Collect just the color decision variables in Colors
        (foreach(C, Countries),
         foreach(E, Countries2),
         foreach(Color, Colors) do
             E = country{name:C,color:Color}
        ),

        % set the valid values of color
        Colors :: 1..4,

        % find all the connections
        findall([C1,C2], c(C1,C2), Connections),

        % fetch the country structure and extract the colors
        ( foreach([N1,N2], Connections), param(Countries2) do
              member(country{name:N1,color:C1},Countries2),
              member(country{name:N2,color:C2},Countries2),
              % these two colors should be distinct
              C1 #\= C2
        ),
        
        % symmetry breaking: color of first country is 1
        [Col1|_] = Colors,
        Col1 #= 1,

        % search
        labeling(Colors),


        writeln(Colors).
        