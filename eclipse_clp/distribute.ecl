/*

  Global constraint distribute in ECLiPSe.

  From MiniZinc globals.mzn:
  """
  Requires that 'card[i]' is the number of occurences of 'value[i]' in 'base'.
  XXX: currently the values in 'value' need not be distinct.  Perhaps they
  should be?
  """

  Compare with the Comet model:
  http://www.hakank.org/comet/distribute.co

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
:-lib(propia).


/*

 Solution of the Comet model:
  dcard[4,1,1,1]
  value[6,7,8,9]
  base[6,7,6,8,6,9,6]
 
*/

go :-
        
        % This test is from MiniZinc's testcases.
        Len = 4,
        dim(Card,[Len]), 
        Card :: 1..10, 
        dim(Value,[Len]),
        Value :: 1..10,
        dim(Base,[7]),
        Base :: 1..10,

        % test
        Card = [](4, _, 1, _),
        Value = [](_, 7, 8, _),
        Base  = [](_, 7, 6, 8, 6, 9, _),

        distribute(Card, Value, Base),

        term_variables([Card,Value,Base], Vars),
        search(Vars,0,occurrence,indomain_min,complete,[backtrack(Backtracks)]),

        writeln(card:Card),
        writeln(value:Value),
        writeln(base:Base), 
        writeln(backtracks:Backtracks),nl, fail.


%
% hakank: Compared to the Comet model, I require that Value are
% distinct, but not ordered and not the complete range of values
% (these two requirements seems to be common in the implementations of 
% the global cardinality constraint).
%
distribute(Card, Value, Base) :-

        % Card and Value must have the same length
        dim(Card,[CardLen]),
        dim(Value,[CardLen]), 
        dim(Base,[BaseLen]),

        alldifferent(Value),
        ( for(I,1,CardLen), param(Base,Value,BaseLen,Card) do
          ( for(J,1,BaseLen), fromto(0,In,In + (Value[I] #= Base[J]),ReifSum),
            param(Base,Value,I) do
                true
          ),
          Card[I] #= eval(ReifSum)
        ).

