/*

  Knapsack problem in SWI Prolog

  From 
  http://rosettacode.org/wiki/Knapsack_problem/Unbounded
  """
  A traveller gets diverted and has to make an unscheduled stop in
  what turns out to be Shangri La. Opting to leave, he is allowed 
  to take as much as he likes of the following items, so long as 
  it will fit in his knapsack, and he can carry it. He knows that 
  he can carry no more than 25 'weights' in total; and that the 
  capacity of his knapsack is 0.25 'cubic lengths'.

  Looking just above the bar codes on the items he finds their 
  weights and volumes. He digs out his recent copy of a financial 
  paper and gets the value of each item.

  Item               Explanation	    Value (each) weight	Volume (each)
  panacea (vials of) Incredible 	    3000         0.3     0.025
                     healing properties
  ichor (ampules of) Vampires blood	    1800	 0.2     0.015
  gold (bars)        Shiney shiney	    2500	 2.0     0.002
  Knapsack	     For the carrying of    -           <=25     <=0.25 

  He can only take whole units of any item, but there is much  
  more of any item than he could ever carry

  How many of each item does he take to maximise the value of items  
  he is carrying away with him?

  Note: There are four solutions that maximise the value taken. 
  Only one need be given. 
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        Names   = [panacea,ichor,gold ],
        Values  = [ 3000, 1800, 2500 ],
        Weights = [    3,    2,    2 ], % multiply with 10
        Volumes = [   25,   15,    2 ], % multiply with 100

        length(Values, Len),

        % 
        % Variables
        % 
        length(X,Len),
        X ins 0..1000,

        %
        % Constraints
        %         
        scalar_product(Weights,X,#=,TotalWeight),
        scalar_product(Values,X,#=,TotalValue),
        scalar_product(Volumes,X,#=,TotalVolume),
        TotalWeight #=< 250, % multiply with 10
        TotalVolume #=< 250, % multiply with 1000


        %
        % Search
        %
        flatten([X,TotalWeight,TotalVolume],Vars),
        labeling([down,max(TotalValue)],Vars), 

        %
        % Solutions
        % 
        writeln(x:X),
        writeln("\nThese are the items to pick:"),
        findall([Name,Pick,Weight,Value,Volume],
                (between(1,Len,I),
                 nth1(I,X,Pick),
                 Pick #> 0,
                 nth1(I,Names,Name),
                 nth1(I,Weights,Weight),
                 nth1(I,Values,Value),
                 nth1(I,Volumes,Volume)
                ),
                Sol
               ),
        maplist(writeln,Sol),
        nl,
        writeln(total_value:TotalValue),
        writeln(total_weight:TotalWeight),
        writeln(total_volume:TotalVolume).
