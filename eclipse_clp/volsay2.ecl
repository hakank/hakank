/*

  Volsay problem in ECLiPSe.

  From OPL model volsay.mod.


  Compare to these models:
  * ECLiPSe: http://www.hakank.org/eclipse/volsay1.ecl
  * Comet  : http://www.hakank.org/comet/volsay2.co

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(eplex).

go :-
        eplex_solver_setup(max(MaxVal)),

        Products = [Gas, Chloride],
        Products :: 0.0..100000.0,

        Gas + Chloride $=< 50,
        3 * Gas + 4 * Chloride $=< 180,

        MaxVal $= 40 * Gas + 50 * Chloride,

        eplex_solve(MaxVal),
        eplex_get(vars, Vars),
        eplex_get(typed_solution, Val),
        Vars = Val, 

        writeln(gas:Gas),
        writeln(chloride:Chloride),
        writeln(max_val:MaxVal).
