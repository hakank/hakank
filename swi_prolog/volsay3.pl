/*

  Volsay problem in SWI Prolog

  From OPL model volsay.mod
  """
  Consider a Belgian company Volsay, which specializes in producing ammoniac gas 
  (NH3) and ammonium chloride (NH4Cl). Volsay has at its disposal 50 units of 
  nitrogen (N), 180 units of hydrogen (H), and 40 units of chlorine (Cl). The company 
  makes a profit of 40 Euros for each sale of an ammoniac gas unit and 50 Euros 
  for each sale of an ammonium chloride unit. Volsay would like a production plan 
  maximizing its profits given its available stocks. 
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   Products = [Gas, Chloride],
   Products ins 0..100000,
   Profits = [40,50],
   Hydrogen = [3,4],
   MaxHydrogen = 180,
   MaxUnits = 50,

   scalar_product(Hydrogen, Products,#=<, MaxHydrogen),
   scalar_product(Profits, Products, #=, MaxVal),
   sum(Products,#=<,MaxUnits),

   labeling([max(MaxVal)],Products),

   writeln(gas=Gas),
   writeln(chloride=Chloride),
   writeln(max_val=MaxVal),
   nl.
