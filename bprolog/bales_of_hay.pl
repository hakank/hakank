/*

  Bales of hay problem in B-Prolog.


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
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
        N = 5,
        length(Bales,N),
        Bales :: 0..50,

        Weights = [80, 82, 83, 84, 85, 86, 87, 88, 90, 91],

        foreach(W in 1..10,
                [I,J,BI,BJ],
                ac(BIJ,[]),
                ([I,J] :: 1..N,
                 I #< J,
                 element(I,Bales,BI),
                 element(J,Bales,BJ),
                 BI + BJ #= Weights[W],
                 BIJ^1 = [[BI,BJ]|BIJ^0]
                )
               ),

        increasing(Bales),

        term_variables([Bales,BIJ],Vars),
        labeling([down],Vars),

        writeln(Bales),
        nl.
        

increasing(List) :-
        foreach(I in 2..List^length, List[I-1] #=< List[I]).


