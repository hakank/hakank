/*

  Magic Squares and Cards in B-Prolog.

  Martin Gardner (July 1971)
  """
  Allowing duplicates values, what is the largest constant sum for an order-3
  magic square that can be formed with nine cards from the deck.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-

        N = 3,
        new_array(X,[N,N]),
        array_to_list(X, Vars),
        Vars :: 1..13,
        S :: 0..13*4, % the sum

        % there are 4 cards of each value in a deck
        foreach(I in 1..13, atmost(4, Vars, I)),

        % the standard magic square constraints (sans all_different)
        foreach(C in 1..N, sum([X[R,C] : R in 1..N]) #= S),
        foreach(R in 1..N, sum([X[R,C] : C in 1..N]) #= S),        

        sum([X[I,I] : I in 1..N]) #= S,
        sum([X[I,N+1-I] : I in 1..N]) #= S,


        term_variables([Vars,S], Vars2),
        % maximize S
        maxof(labeling(Vars2), S),
        % labeling(Vars2),

        writeln(s:S),
        Rows @= X^rows,
        foreach(Row in Rows, writeln(Row)),

        nl.
        
