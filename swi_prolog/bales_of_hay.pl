/*

  Bales of hay problem in SWI Prolog

  From The Math Less Traveled, 
  "The haybaler", http://www.mathlesstraveled.com/?p=582 
  """
  You have five bales of hay.

  For some reason, instead of being weighed individually, they were weighed 
  in all possible combinations of two. The weights of each of these 
  combinations were written down and arranged in numerical order, without 
  keeping track of which weight matched which pair of bales. The weights, 
  in kilograms, were 80, 82, 83, 84, 85, 86, 87, 88, 90, and 91.

  How much does each bale weigh? Is there a solution? Are there multiple 
  possible solutions? 
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        N = 5,
        length(Bales,N),
        Bales ins 0..50,

        Weights = [80, 82, 83, 84, 85, 86, 87, 88, 90, 91],
        
        numlist(1,10,Ws),
        maplist(bales_of_hay(N,Bales,Weights),Ws,BIJs),
        
        increasing(Bales),

        flatten([Bales,BIJs], Vars),
        labeling([down],Vars),

        writeln(Bales).


bales_of_hay(N,Bales,Weights,W, [BI,BJ]) :-
        [I,J] ins 1..N,
        I #< J,
        element(I,Bales,BI),
        element(J,Bales,BJ),
        element(W,Weights,WeightsW),
        BI + BJ #= WeightsW.
