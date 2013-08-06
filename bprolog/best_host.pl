/*

  Best host (PuzzlOr problem) in B-Prolog.

  From The PuzzlOr, February | Volume 38 | Number 1
  http://www.informs.org/ORMS-Today/Public-Articles/February-Volume-38-Number-1/THE-PUZZLOR
  """
  By John Toczek

  Hosting a dinner party requires several skills to pull off a successful 
  evening. One of your duties, aside from preparing dinner and selecting 
  the drinks, is to make sure your guests enjoy themselves.

  Figure 1 shows a dinner table with six seats for your guests. Some 
  guests, however, do not get along with each other. If two guests who 
  do not get along are seated next to each other, it will create conflict 
  at dinner. As host, you must arrange the guests in a seating order 
  that minimizes conflict.

  Andrew will only sit next to Dave and Frank; 
  Betty will only sit next to Cara and Erica; 
  Cara will only sit next to Betty and Frank; 
  Dave will only sit next to Andrew and Erica; 
  Erica will only sit next to Betty and Dave; 
  Frank will only sit next to Andrew and Cara. 

  [
    Figure 1 shows the following arrangement:
           
                Andrew
         Frank         Betty
         Erica         Cara
                Dave
  
  ]
  
  In the example seating arrangement above, there are three conflicts 
  (Andrew and Betty, Cara and Dave, Erica and Frank).

  Question:

  What seating arrangement will minimize the conflict at dinner?
  """

  Answer: There are 12 possible solutions with no conflict at all.

  By placing Andrew at position 1 (as symmetry breaking) there are 
  2 possible solutions, where the 2nd solution is the mirror of the 1st.

  1) Andrew Frank Cara Betty Erica Dave

                 Andrew
         Dave           Frank
         Erica          Cara
                 Betty

  2) Andrew Dave Erica Betty Cara Frank
     
                 Andrew 
          Frank         Dave
          Cara          Erica
                 Betty 


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        Names = ['Andrew', 'Betty', 'Cara', 'Dave', 'Erica', 'Frank'],
        findall(X, best_host(X), L),
        foreach(LL in L, [T],
                (writeln(LL),
                 T @= [Names[I] : I in LL],
                 writeln(T)
               )),
        nl.


best_host(X) :-
        N = 6,
        Andrew = 1,
        Betty  = 2,
        Cara   = 3,
        Dave   = 4,
        Erica  = 5,
        Frank  = 6,

        % Preferences
        Prefs = [[Dave, Frank],   % Andrew
                 [Cara, Erica],   % Betty
                 [Betty, Frank],  % Cara
                 [Andrew, Erica], % Dave 
                 [Betty, Dave],   % Erica
                 [Andrew, Cara]], % Frank
       

        % decision variables
        length(X, N),
        X :: 1..N,

        % constraints
        alldifferent(X),

        % Ensure that a person X[I] is sitting between 
        % his/her preferred friends, Prefs[X[I]].
        foreach(I in 1..N, [A,B,XI,PrefsXI],
                (place(I-1,N,A),
                 place(I+1,N,B),
                 XI #= X[I],
                 nth1(XI,Prefs,PrefsXI),
                 X[A] in PrefsXI,
                 X[B] in PrefsXI
                )
               ),

        % symmetry breaking
        X[1] #= Andrew,

        labeling(X).

% The placement on a circular table
place(C,N,P) :-
        CN is C mod N,
        (CN =:= 0 -> P is N ; P is CN).