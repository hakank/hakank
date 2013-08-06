/*

  Knapsack 0/1 in ECLiPSe.

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


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(branch_and_bound).
%:-lib(listut).
%:-lib(propia).


go :-
        Names   = [panacea,  ichor,  gold    ],
        Values  = [ 3000.0, 1800.0, 2500.0   ],
        Weights = [    0.3,    0.2,    2.0   ],
        Volumes = [    0.025,  0.015,  0.002 ],

        length(Values, Len),

        % 
        % Variables
        % 
        length(X,Len),
        X :: 0..1000,

        %
        % Constraints
        % 
        scalar_product_list(X,Weights,TotalWeight),
        scalar_product_list(X,Values,TotalValue),
        scalar_product_list(X,Volumes,TotalVolume),
        TotalWeight $=< 25.0,
        TotalVolume $=< 0.25,
        TotalValueNeg $= -TotalValue,


        %
        % Search
        %
        term_variables([X,TotalWeight,TotalValue,TotalVolume],Vars),
        % minimize(search(Vars,0,first_fail,indomain_max,complete,
        %                [backtrack(Backtracks)]), TotalValueNeg),
        minimize(locate(Vars, 0.0001), TotalValueNeg),

        %
        % Solutions
        % 
        writeln(x:X),
        writeln('\nThese are the items to pick:'),
        ( foreach(Pick,X),
          foreach(Name,Names),
          foreach(Weight, Weights),
          foreach(Value, Values),
          foreach(Volume, Volumes) do
              Pick $> 0 ->
              writeln([Name,Pick, Weight, Value, Volume])
              % printf("* %-25s %4f %3f %3f\n", [Name,Weight, Value, Volume])
        ;
              true
        ),
        nl,
        writeln(total_weight:TotalWeight),
        writeln(total_value:TotalValue),
        writeln(total_volume:TotalVolume).



scalar_product_list(List1,List2,Result) :-
        ( foreach(L1,List1),
          foreach(L2,List2),
          fromto(0,In,Out,Result) do
              Out $= L1*L2+In
        ).


