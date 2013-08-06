/*

  Volsay problem in B-Prolog.

  From OPL model volsay.mod.
  Slightly different from volsay1.pl

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        Products = [Gas, Chloride],
        Products :: 0..100000,

        Gas + Chloride #=< 50,
        3 * Gas + 4 * Chloride #=< 180,
        MaxVal #= 40 * Gas + 50 * Chloride,

        labeling([maximize(MaxVal)],Products),

        writeln(gas:Gas),
        writeln(chloride:Chloride),
        writeln(max_val:MaxVal),
        nl.