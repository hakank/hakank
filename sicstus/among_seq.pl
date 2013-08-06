/*

  Global constraint among_seq in SICStus Prolog.

  From Global constraint catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Camong_seq.html
  """
  Constraint

    among_seqâ‹LOW,UP,SEQ,VARIABLES,VALUES)

  Purpose  
  Constrains all sequences of SEQ consecutive variables of the collection 
  VARIABLES to take at least LOW values in VALUES and at most UP values 
  in VALUES.

  Example
    (
    1,2,4,‹9,2,4,5,5,7,2>,
    <0,2,4,6,8>
    )

  The among_seq constraint holds since the different sequences of 4 
  consecutive variables contains respectively 2, 2, 1 and 1 even numbers.
  """


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/among_seq.mzn
  * Comet   : http://www.hakank.org/comet/among_seq.co
  * ECLiPSe : http://www.hakank.org/eclipse/among_seq.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-

        Len = 7,

        % The set as a list
        V = {0,2,4,6,8},

        % create X
        length(X,Len),
        domain(X,0,9),

        % X = [9,2,4,5,5,7,2], % the example above

        % some symmetry breaking if we let X free
        all_different(X),
        my_ordered_lt(X),

        Low = 1,
        High = 2,
        SeqLen = 4,
        among_seq(Low,High,SeqLen,X,V),

        % search
        append(X,[Low,High], Vars),
        labeling([],Vars),

        write(low:Low),nl,
        write(high:High),nl,
        write(seq_len:SeqLen),nl,
        write(x:X),nl,
        write(v:V),nl,
        nl,
        fail.



/*
  among_seq(Low, High, SeqLen, X, V)

  See the definition above.

  Ensures that all sequences of length SeqLen in the list X 
  contains at least Low and atmost High occurrences of V.

*/
among_seq(Low,High,SeqLen,X,V) :-
        length(X,Len),            
        Size is Len-SeqLen+1,
        ( for(I,1,Size), 
          param(Low,High,SeqLen,X,V) do
              ( for(J,I,I+SeqLen-1),
                foreach(El,Seq), param(X) do
                    nth1(J,X,El)
              ),
              among_range(Low, High, Seq, V)
        ).



/*
  among_range(Low, High, X, V)

  ensures that the list X contains at least Low and atmost High
  occurrences of V.

*/
among_range(Low, High,X,V) :- 
        ( foreach(El,X), 
          fromto(0,In,Out,Sum), 
          param(V) do
              B in 0..1,
              El in V #<=> B #= 1,
              Out #= In + B
        ),
        N #= Sum,
        N #>= Low,
        N #=< High.


my_ordered_lt(List) :-
        ( List = [] -> 
              true
        ;
              ( fromto(List, [This,Next | Rest], [Next|Rest],[_])
              do
                This #=< Next
              )
        ).
