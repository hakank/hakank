/*

  Global constraint among_seq in Comet.

  From Global constraint catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Camong_seq.html
  """
  Constraint

    among_seq​(LOW,​UP,​SEQ,​VARIABLES,​VALUES)

  Purpose  
  Constrains all sequences of SEQ consecutive variables of the collection 
  VARIABLES to take at least LOW values in VALUES and at most UP values 
  in VALUES.

  Example
    (
    1,​2,​4,​〈9,​2,​4,​5,​5,​7,​2〉,​
    〈0,​2,​4,​6,​8〉
    )

  The among_seq constraint holds since the different sequences of 4 
  consecutive variables contains respectively 2, 2, 1 and 1 even numbers.
  """


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/among_seq.mzn
  * Comet   : http://www.hakank.org/comet/among_seq.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_sets).
:-lib(ic_global). 
:-lib(listut). % for nth1

go :-

        Len = 7,
        R = 0..9,

        % The set as a list
        V = [0,2,4,6,8],

        % create X
        length(X,Len),
        ic:'::'(X, R),

        % X = [9,2,4,5,5,7,2], % the example above

        % some symmetry breaking if we let X free
        ic_global:alldifferent(X),
        ic_global:ordered(=<, X),

        Low = 1,
        High = 2,
        SeqLen = 4,
        among_seq(Low,High,SeqLen,X,V),

        % search
        term_variables([X,Low,High], Vars),
        labeling(Vars),

        writeln(low:Low),
        writeln(high:High),
        writeln(seq_len:SeqLen),
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
        length(X,Len),            
        Size is Len-SeqLen+1,
        ( for(I,1,Size), param(Low,High,SeqLen,X,V) do
              ( for(J,I,I+SeqLen-1),
                foreach(El,Seq), param(X) do
                    listut:nth1(J,X,El)
              ),
              among_range(Low, High, Seq, V)
        ).



/*
  among_range(Low, High, X, V)

  ensures that the list X contains at least Low and atmost High
  occurrences of V.

*/
among_range(Low, High,X,V) :- 
        ( foreach(El,X), fromto(0,In,In + (El in V),Sum), param(V) do
              true
        ),
        N #= eval(Sum),
        N #>= Low,
        N #=< High.

