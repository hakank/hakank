/*

  Global constraint among_seq in B-Prolog.

  From Global constraint catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Camong_seq.html
  """
  Constraint

    among_seq(LOW,UP,SEQ,VARIABLES,VALUES)

  Purpose  
  Constrains all sequences of SEQ consecutive variables of the collection 
  VARIABLES to take at least LOW values in VALUES and at most UP values 
  in VALUES.

  Example
    (
    1,2,4,<9,2,4,5,5,7,2>,
    <0,2,4,6,8>
    )

  The among_seq constraint holds since the different sequences of 4 
  consecutive variables contains respectively 2, 2, 1 and 1 even numbers.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-

        Len = 7,

        % The set as a list
        V = [0,2,4,6,8],

        % create X
        length(X,Len),
        X :: 0..9,

        % X = [9,2,4,5,5,7,2], % the example above

        % some symmetry breaking if we let X free
        alldifferent(X),
        increasing(X),

        Low = 1,
        High = 2,
        SeqLen = 4,
        among_seq(Low,High,SeqLen,X,V),

        % search
        term_variables([X,Low,High], Vars),
        labeling([],Vars),

        writeln([low:Low,high:High,seq_len:SeqLen]),
        writeln(x:X),
        writeln(v:V),
        nl,
        fail.



/*
  among_seq(Low, High, SeqLen, X, V)

  See the definition above.

  Ensures that all sequences of length SeqLen in the list X 
  contains at least Low and atmost High occurrences of V.

*/
among_seq(Low,High,SeqLen,X,V) :-
        Len @= X^length,
        Size is Len-SeqLen+1,
        foreach(I in 1..Size,[Seq], 
            (Seq @= [El : J in I..I+SeqLen-1,[El], nth(J,X,El)],
             among_range(Low, High, Seq, V))
        ).



/*
  among_range(Low, High, X, V)

  ensures that the list X contains at least Low and atmost High
  occurrences of V.

*/
among_range(Low, High,X,V) :- 
        Sum #= sum([B :  El in X,[B], B #<=> El in V]),
        Sum #>= Low,
        Sum #=< High.


increasing(List) :-
        foreach(I in 2..List^length, List[I-1] #=< List[I]).
