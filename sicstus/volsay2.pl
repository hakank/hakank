/*

  Volsay problem in SICStus Prolog.

  From OPL model volsay.mod.

  Compare with the following models:
  * SICStus: http://www.hakank.org/sicstus/volsay1.pl
  * Comet  : http://www.hakank.org/comet/volsay.co
  * ECLiPSe: http://www.hakank.org/eclipse/volsay1.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        Products = [Gas, Chloride],
        domain(Products, 0,100000),

        Gas + Chloride #=< 50,
        3 * Gas + 4 * Chloride #=< 180,
        MaxVal #= 40 * Gas + 50 * Chloride,

        labeling([maximize(MaxVal)],[Gas,Chloride]),

        write(gas:Gas),nl,
        write(chloride:Chloride),nl,
        write(max_val:MaxVal),nl.
