/*

  Volsay problem in ECLiPSe.

  From OPL model gas.mod


  Compare to these models:
  * ECLiPSe: http://www.hakank.org/eclipse/volsay1.ecl
  * ECLiPSe: http://www.hakank.org/eclipse/volsay2.ecl
  * Comet  : http://www.hakank.org/comet/volsay3.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(eplex).

go :-
        eplex_solver_setup(max(TotalProfit)),

        demands(Demands),
        profit(Profit),
        stock(Stock),
        products(Products),
        components(_Components), % not used

        dim(Demands,[NumProducts,NumComponents]),
        dim(Production, [NumProducts]),
        
        Production[1..NumProducts] :: 0.0..100.0,

        ( for(C,1,NumComponents), 
          param(Demands,Production, Stock,NumProducts) do
              ( for(P,1,NumProducts),
                fromto(0,In,Out,Sum),
                param(Demands,Production,C) do
                    Out = In + Demands[P,C]*Production[P] 
              ),
              eval(Sum) $=< Stock[C]
        ),

        ( for(P,1,NumProducts),
          fromto(0,In,Out,TotalProfitTmp),
          param(Profit, Production) do
              Out = In + Profit[P]*Production[P]
        ),
        TotalProfit $= eval(TotalProfitTmp),

        eplex_solve(TotalProfit),
        eplex_get(vars, Vars),
        eplex_get(typed_solution, Val),
        Vars = Val, 


        ( foreacharg(P1,Products), 
          foreacharg(P2,Production) do
              printf("%w %2.2f\n", [P1,P2])
        ),
        writeln(total_profit:TotalProfit).



demands([]([](1,3,0), [](1,4,1))).
profit([](30,40)).
stock([](50,180,40)).
products([](gas,chloride)).
components([](nitrogen, hydrogen, chlorine)).
