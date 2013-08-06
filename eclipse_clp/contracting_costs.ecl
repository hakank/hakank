/*

  Contracting costs in ECLiPSe.

  From 
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
  """
  Contracting Costs    from "Mathematical Puzzles of Sam Loyd, Volume 2",
  number 20.

  A contractor planning the construction of a house found that he would
  have to pay:

    * $ 1,100 to the paper hanger and the painter,
    * $ 1,700 to the painter and plumber,
    * $ 1,100 to the plumber and electrician,
    * $ 3,300 to the electrician and carpenter,
    * $ 5,300 to the carpenter and mason,
    * $ 3,200 to the mason and painter. 

  What does each man charge for his services?
  """


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/


*/

:-lib(ic).

go :-
        findall([ph:PH, pa:Pa, pl:Pl, el:El, ca:Ca, ma:Ma], contracting_costs([PH, Pa, Pl, El, Ca, Ma]), L),
        writeln(L).

contracting_costs(LD) :-
        LD = [PH, Pa, Pl, El, Ca, Ma],
        LD :: 1..5300,
        1100 #= PH + Pa,
        1700 #= Pa + Pl,
        1100 #= Pl + El,
        3300 #= El + Ca,
        5300 #= Ca + Ma,
        3200 #= Ma + Pa,

        search(LD, 0, first_fail, indomain, complete, []).