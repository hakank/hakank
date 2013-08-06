/*

  Blending problem in ECLiPSe.

  From the OPL model blending.mod .

  Compare with the Comet model:
  http://www.hakank.org/comet/blending.co  


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:- lib(eplex).
:-lib(listut).


go :-
        eplex_solver_setup(min(Sum)),

        % get data
        cost(metal,CostMetal),
        cost(raw, CostRaw),
        cost(scrap,CostScrap),
        cost(ingo,CostIngo),

        length(CostMetal, NbMetals),
        length(CostRaw,NbRaw),
        length(CostScrap, NbScrap),
        length(CostIngo, NbIngo),

        low(Low),
        up(Up),

        perc(raw,PercRaw),
        perc(scrap, PercScrap),
        perc(ingo, PercIngo),
        alloy(Alloy),
        

        % decision variables
        length(P,NbMetals),
        P :: 0.0..10000.0,

        length(R,NbRaw),
        R :: 0.0..10000.0,

        length(S,NbScrap),
        S :: 0.0..10000.0,

        length(I,NbIngo),
        I :: 0.0..10000.0,
        integers(I),
        
        length(Metal,NbMetals),
        ( foreach(MetalJ, Metal), 
          foreacharg(L, Low),
          foreacharg(U, Up),
          param(Alloy) do
              MetalJ :: L*Alloy.. U*Alloy
        ),


        Sum $= CostMetal*P + CostRaw*R + CostScrap*S + CostIngo*I,
        Sum $>=0,

        ( for(J,1,NbMetals),
          param(PercRaw,PercScrap,PercIngo,Metal,
                NbRaw,NbScrap,NbIngo,P,R,S,I) do

              PercRawJ is PercRaw[J, 1..NbRaw],
              SumRaw $= PercRawJ*R,

              PercScrapJ is PercScrap[J, 1..NbScrap],
              SumScrap $= PercScrapJ*S,

              PercIngoJ is PercIngo[J, 1..NbIngo],
              SumIngo $= PercIngoJ*I,

              nth1(J,Metal,MetalJ),
              nth1(J,P,PJ),
              MetalJ $= PJ + SumRaw + SumScrap + SumIngo
              
        ),

        Alloy $= sum(Metal),

        eplex_solve(Sum),
        eplex_get(vars, Vars),
        eplex_get(typed_solution, Val),
        Vars = Val, 

        writeln("P":P),
        writeln("Raw":R),
        writeln("Scrap":S),
        writeln("Ingo":I),
        writeln("Metal":Metal),
        writeln("Sum":Sum).


%
% data
%
cost(metal, [22, 10, 13]).
cost(raw,[6, 5]).
cost(scrap,[7, 8]).
cost(ingo,[9]).

low([](0.05, 0.30, 0.60)).
up([](0.10, 0.40, 0.80)).

perc(raw,[]([](0.20, 0.01), [](0.05, 0.00), [](0.05, 0.30))).
perc(scrap,[]([](0.00, 0.01), [](0.60, 0.00), [](0.40, 0.70))).
perc(ingo, []([](0.10), [](0.45), [](0.45))).
alloy(71).