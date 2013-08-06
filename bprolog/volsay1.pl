/*

  Volsay problem in B-Prolog.

  From OPL model volsay.mod.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        Gas :: 0..100000,
        Chloride :: 0..100000,

        Gas + Chloride #=< 50,
        3 * Gas + 4 * Chloride #=< 180,

        MaxVal #= 40 * Gas + 50 * Chloride,

        labeling([maximize(MaxVal)],[Gas,Chloride]),

        writeln(gas:Gas),
        writeln(chloride:Chloride),
        writeln(max_val:MaxVal),
        nl.
