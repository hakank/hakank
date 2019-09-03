/*

  Knapsack (Bounded) in SWI Prolog

  From 
  http://rosettacode.org/wiki/Knapsack_problem/Bounded
  """
   A tourist wants to make a good trip at the weekend with his 
   friends. They will go to the mountains to see the wonders of 
   nature. So he needs some items during the trip. Food, clothing, 
   etc. He has a good knapsack for carrying the things, but he knows 
   that he can carry only 4 kg weight in his knapsack, because they 
   will make the trip from morning to evening. He creates a list of 
   what he wants to bring for the trip, but the total weight of all 
   items is too much. He adds a value to each item. The value represents 
   how important the thing for the tourist. The list contains which 
   items are the wanted things for the trip, what is the weight and 
   value of an item, and how many units does he have from each items.
  
   This is the list:
   Table of potential knapsack items item 	weight (dag) (each) 	value (each) 	piece(s)
   map 	9 	150 	1
   compass 	13 	35 	1
   water 	153 	200 	2
   sandwich 	50 	60 	2
   glucose 	15 	60 	2
   tin 	68 	45 	3
   banana 	27 	60 	3
   apple 	39 	40 	3
   cheese 	23 	30 	1
   beer 	52 	10 	3
   suntan cream 	11 	70 	1
   camera 	32 	30 	1
   T-shirt 	24 	15 	2
   trousers 	48 	10 	2
   umbrella 	73 	40 	1
   waterproof trousers 	42 	70 	1
   waterproof overclothes 	43 	75 	1
   note-case 	22 	80 	1
   sunglasses 	7 	20 	1
   towel 	18 	12 	2
   socks 	4 	50 	1
   book 	30 	10 	2
   knapsack 	<=400 dag 	 ? 	 ? 
  
  
   The tourist can choose to take any combination of items from the 
   list, and some number of each item is available (see the column 
   "Piece(s)" of the list!). He may not cut the items, so he can only 
   take whole units of any item.
   
   Which items does the tourist carry in his knapsack so that their 
   total weight does not exceed 4 kg, and their total value is maximised?
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        items(Items),
        length(Items,Rows),
        transpose(Items,ItemsT),
        [Names,Weights,Values,Pieces] = ItemsT,
        
        WeightLimit = 400,

        %% 
        %% Variables
        %% 
        max_list(Pieces,MaxPieces),
        length(X,Rows),
        X ins 0..MaxPieces,

        %%
        %% Constraints
        %% 
        sum(Values,#=,SumValues),
        TotalValue in 0..SumValues,
        TotalWeight in 0..WeightLimit,

        scalar_product(Weights,X,#=,TotalWeight),
        scalar_product(Values,X,#=,TotalValue),

        %% check number of pieces
        maplist(check_capacity,X,Pieces),
        
        %%
        %% Search
        %%
        labeling([ff,down,enum,max(TotalValue)],X), 

        %%
        %% Solutions
        %% 
        writeln(x=X),
        writeln('\nThese are the items to pick:'),
        findall([Pick,Name,Weight,Value],
                (between(1,Rows,I),
                 element(I,X,Pick),
                 Pick #> 0,
                 nth1(I,Names,Name),
                 nth1(I,Weights,Weight),
                 nth1(I,Values,Value)
                ),
                Sol),
        maplist(format("* ~d ~w~29|~d~32| ~d\n"),Sol),
        format('Total weight: ~d\n', [TotalWeight]),
        format('Total value: ~d\n', [TotalValue]).

check_capacity(X,MaxPiece) :-
        X #=< MaxPiece.


       % Item                    Weight   Value  Pieces
items([['map',                     9,       150,   1],
       ['compass',                 13,      35,    1],
       ['water',                   153,     200,   2],
       ['sandwich',                50,      60,    2],
       ['glucose',                 15,      60,    2],
       ['tin',                     68,      45,    3],
       ['banana',                  27,      60,    3],
       ['apple',                   39,      40,    3],
       ['cheese',                  23,      30,    1],
       ['beer',                    52,      10,    3],
       ['suntancream',             11,      70,    1],
       ['camera',                  32,      30,    1],
       ['T-shirt',                 24,      15,    2],
       ['trousers',                48,      10,    2],
       ['umbrella',                73,      40,    1],
       ['waterproof trousers',     42,      70,    1],
       ['waterproof overclothes',  43,      75,    1],
       ['note-case',               22,      80,    1],
       ['sunglasses',              7,       20,    1],
       ['towel',                   18,      12,    2],
       ['socks',                   4,       50,    1],
       ['book',                    30,      10,    2]]).
