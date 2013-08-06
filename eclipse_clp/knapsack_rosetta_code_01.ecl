/*

  Knapsack 0/1 in ECLiPSe.

  From http://rosettacode.org/wiki/Knapsack_problem/0-1
  """
  A tourist wants to make a good trip at the weekend with his friends.
  They will go to the mountains to see the wonders of nature, so he 
  needs to pack well for the trip. He has a good knapsack for carrying 
  things, but knows that he can carry a maximum of only 4kg in it and 
  it will have to last the whole day. He creates a list of what he
  wants to bring for the trip but the total weight of all items is too 
  much. He then decides to add columns to his initial list detailing 
  their weights and a numerical value representing how important the item is for the trip.

  Here is the list:
  Table of potential knapsack items item 	weight (dag) 	value
  map 	9 	150
  compass 	13 	35
  water 	153 	200
  sandwich 	50 	160
  glucose 	15 	60
  tin 	68 	45
  banana 	27 	60
  apple 	39 	40
  cheese 	23 	30
  beer 	52 	10
  suntan cream 	11 	70
  camera 	32 	30
  T-shirt 	24 	15
  trousers 	48 	10
  umbrella 	73 	40
  waterproof trousers 	42 	70
  waterproof overclothes 	43 	75
  note-case 	22 	80
  sunglasses 	7 	20
  towel 	18 	12
  socks 	4 	50
  book 	30 	10
  knapsack 	<=400 dag 	 ?

  The tourist can choose to take any combination of items from the
  list, but only one of each item is available. He may not cut or
  diminish the items, so he can only take whole units of any item.

  Which items does the tourist carry in his knapsack so that their
  total weight does not exceed 400 dag [4 kg], and their total value 
  is maximised?
  """

  Solved in 0.389s:

  backtracks : 270

  These are the items to pick:
    Item                    Weight Value
  * map                          9 150
  * compass                     13  35
  * water                      153 200
  * sandwich                    50 160
  * glucose                     15  60
  * banana                      27  60
  * suntancream                 11  70
  * waterproof trousers         42  70
  * waterproof overclothes      43  75
  * note-case                   22  80
  * sunglasses                   7  20
  * socks                        4  50

  Total weight: 396
  Total value: 1030


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
        items(Items),
        length(Items,Rows),
        ( foreach([Item, Weight, Value],Items),
          foreach(Weight,Weights),
          foreach(Value,Values),
          foreach(Item, AllItems) do
              true
        ),

        % 
        % Variables
        % 
        length(X,Rows),
        X :: 0..1,

        %
        % Constraints
        % 
        scalar_product_list(X,Weights,TotalWeight),
        scalar_product_list(X,Values,TotalValues),
        TotalWeight #=< 400,
        TotalValuesNeg #= -TotalValues,

        %
        % Search
        %
        term_variables([X,TotalWeight,TotalValues],Vars),
        minimize(search(Vars,0,first_fail,indomain_max,complete,
                        [backtrack(Backtracks)]), TotalValuesNeg),

        %
        % Solutions
        % 
        writeln(backtracks:Backtracks),
        writeln('\nThese are the items to pick:'),
        writeln('  Item                    Weight Value'),
        ( foreach(Pick,X),
          foreach(I,AllItems),
          foreach(Weight, Weights),
          foreach(Value, Values) do
              Pick #= 1 ->
              printf("* %-25s %4d %3d\n", [I,Weight, Value])
        ;
              true
        ),
        nl,
        printf('Total weight: %d\n', [TotalWeight]),
        printf('Total value: %d\n', [TotalValues]).



scalar_product_list(List1,List2,Result) :-
        ( foreach(L1,List1),
          foreach(L2,List2),
          fromto(0,In,Out,Result) do
              Out #= L1*L2+In
        ).


       % Item                    Weight   Value
items([["map",                     9,       150],
       ["compass",                 13,      35],
       ["water",                   153,     200],
       ["sandwich",                50,      160],
       ["glucose",                 15,      60],
       ["tin",                     68,      45],
       ["banana",                  27,      60],
       ["apple",                   39,      40],
       ["cheese",                  23,      30],
       ["beer",                    52,      10],
       ["suntancream",             11,      70],
       ["camera",                  32,      30],
       ["T-shirt",                 24,      15],
       ["trousers",                48,      10],
       ["umbrella",                73,      40],
       ["waterproof trousers",     42,      70],
       ["waterproof overclothes",  43,      75],
       ["note-case",               22,      80],
       ["sunglasses",              7,       20],
       ["towel",                   18,      12],
       ["socks",                   4,       50],
       ["book",                    30,      10]]).
