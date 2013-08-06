/*

  Set covering in B-Prolog.

  Example from Steven Skiena, The Stony Brook Algorithm Repository
  http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
  """
  Input Description: A set of subsets S_1, ..., S_m of the 
  universal set U = {1,...,n}.
  
  Problem: What is the smallest subset of subsets T subset S such 
  that \cup_{t_i in T} t_i = U?
  """
  Data is from the pictures INPUT/OUTPUT.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/




%
% First find the optimal value (MinVal), then find all the solutions with that value.
%
go :-

        write('Find the optimal solution'),nl,
        belongs(Belongs),
        set_covering_skiena(Belongs, MinVal,_),

        format('\nFinding all optimal solutions with MinVal ~d:\n', [MinVal]),
        findall(X, set_covering_skiena(Belongs,  MinVal,X), L),
        length(L, Len),
        foreach(S in L, writeln(S)),
        format('It was ~d solutions\n', [Len]),nl.

transpose(Matrix,Transposed):-
    N is Matrix^length,
    M is Matrix[1]^length,
    Transposed @= [MJI : J in 1..M, [MJI],
                (MJI @= [Matrix[I,J] : I in 1..N])].

set_covering_skiena(Belongs, MinVal, X) :-

        NumSets @= Belongs^length,

        length(X,NumSets),
        X :: 0..1,

        transpose(Belongs,BelongsTransposed),
        foreach(Belong in BelongsTransposed,
                sum([XX*(B#>0) : (B,XX) in (Belong,X)]) #>= 1 
               ),
        MinVal #= sum(X),

        % either search for all solutions (for the minimum value) or
        % the optimal value
        (
            ground(MinVal) 
        -> 
            labeling([],X)
        ;
            labeling([minimize(MinVal)], X)
        ).


%
% The belong matrix:
%
belongs([[1,1,0,0,0,0,0,0,0,0,0,0],
         [0,1,0,0,0,0,0,1,0,0,0,0],
         [0,0,0,0,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,1,1,0,0,1,1,0],
         [0,0,0,0,0,0,0,0,1,1,0,0],
         [1,1,1,0,1,0,0,0,1,1,1,0],
         [0,0,1,1,0,0,1,1,0,0,1,1]]).
