/*

  Global constraint distribute in SICStus Prolog.

  Decomposition of global constraint distribute.

  From MiniZinc globals.mzn:
  """
  Requires that 'card[i]' is the number of occurences of 'value[i]' in 'base'.
  """

  Compare with the following models:
  * Comet  : http://www.hakank.org/comet/distribute.co
  * ECLiPSe: http://www.hakank.org/eclipse/distribute.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

/*

 Solution from the Comet model:
  dcard[4,1,1,1]
  value[6,7,8,9]
  base[6,7,6,8,6,9,6]
 
*/

go :-
        
        % This test is from MiniZinc's testcases.
        Len = 4,
        length(Card,Len), 
        domain(Card,1,10), 
        length(Value,Len),
        domain(Value,1,10),
        length(Base,7),
        domain(Base,1,10),

        % test values
        Card = [4, _, 1, _],
        Value = [_, 7, 8, _],
        Base  = [_, 7, 6, 8, 6, 9, _],

        distribute(Card, Value, Base),

        append(Card,Value,Vars1),
        append(Vars1,Base, Vars),
        labeling([ff,bisect,up],Vars),

        write(card:Card),nl,
        write(value:Value),nl,
        write(base:Base),nl,
        fd_statistics.


%
% (From the ECLiPSe model):
% hakank: Compared to the Comet model, I require that Value are
% distinct, but not ordered and not the complete range of values
% (these two requirements seems to be common in the implementations of 
% the global cardinality constraint).
%
distribute(Card, Value, Base) :-

        % Card and Value must have the same length
        length(Card,CardLen),
        length(Value,CardLen), 
        length(Base,BaseLen),

        all_different(Value),
        ( for(I,1,CardLen), 
          param(Base,Value,BaseLen,Card) do
          ( for(J,1,BaseLen), 
            fromto(0,In,Out,ReifSum),
            param(Base,Value,I) do
                element(I,Value,ValueI),
                element(J,Base,BaseJ),
                B in 0..1,
                ValueI #= BaseJ #<=> B#=1,
                Out #= In + B
          ),
          element(I,Card,ReifSum)
        ).
