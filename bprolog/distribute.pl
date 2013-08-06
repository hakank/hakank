/*

  Global constraint distribute in B-Prolog.

  Decomposition of global constraint distribute.

  From MiniZinc globals.mzn:
  """
  Requires that 'card[i]' is the number of occurences of 'value[i]' in 'base'.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

/*
 Solution:
   card[4,1,1,1]
   value[6,7,8,9]
   base[6,7,6,8,6,9,6]
 
*/

go :-
        
        % This test is from MiniZinc's testcases.
        Len = 4,
        length(Card,Len), 
        Card :: 1..10, 
        length(Value,Len),
        Value :: 1..10,
        length(Base,7),
        Base :: 1..10,

        % test values
        Card  = [4, _, 1, _],
        Value = [_, 7, 8, _],
        Base  = [_, 7, 6, 8, 6, 9, _],

        distribute(Card, Value, Base),

        term_variables([Card,Value,Base], Vars),
        labeling([ff],Vars),

        writeln(card:Card),
        writeln(value:Value),
        writeln(base:Base),
        nl.


%
% (This comment is from the ECLiPSe model):
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
        foreach(I in 1..CardLen, 
                [Sum],
                (Sum #= 
                 sum([(ValueI #= BaseJ) : J in 1..BaseLen,
                      [ValueI,BaseJ],
                      (
                          element(I,Value,ValueI),
                          element(J,Base,BaseJ)
                      )
                     
                     ]),
                 element(I,Card,Sum)
                )
               ).
