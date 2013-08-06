/*

  Cross word in B-Prolog.

  This is a standard example for constraint logic programming. See e.g.
 
  http://www.cis.temple.edu/~ingargio/cis587/readings/constraints.html
  """
  We are to complete the puzzle
 
       1   2   3   4   5
     +---+---+---+---+---+       Given the list of words:
   1 | 1 |   | 2 |   | 3 |             AFT     LASER
     +---+---+---+---+---+             ALE     LEE
   2 | # | # |   | # |   |             EEL     LINE
     +---+---+---+---+---+             HEEL    SAILS
   3 | # | 4 |   | 5 |   |             HIKE    SHEET
     +---+---+---+---+---+             HOSES   STEER
   4 | 6 | # | 7 |   |   |             KEEL    TIE
     +---+---+---+---+---+             KNOT
   5 | 8 |   |   |   |   |
     +---+---+---+---+---+       
   6 |   | # | # |   | # |       The numbers 1,2,3,4,5,6,7,8 in the crossword
     +---+---+---+---+---+       puzzle correspond to the words 
                                                   that will start at those locations.
  """

  The model was inspired by Sebastian Brand's Array Constraint 
  cross word example but it is slightly generalized
  * http://www.cs.mu.oz.au/~sbrand/project/ac/
  * http://www.cs.mu.oz.au/~sbrand/project/ac/examples.pl

 
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
        overlapping(Overlappings),
        NumOverlappings @= Overlappings^length,
        words(Words),
        length(Words,NumWords),

        ESize = 8,

        % decision variables
        length(E,ESize),
        E :: 1..NumWords,
        alldifferent(E),

        foreach(I in 1..NumOverlappings,
                [O1,O2,O3,O4,E1,E2,Word1,Word2,C],
                (
                    % the overlappings
                    O1 @= Overlappings[I,1], % first word
                    O2 @= Overlappings[I,2], % position in first word
                    O3 @= Overlappings[I,3], % second word
                    O4 @= Overlappings[I,4], % position in second word
                    
                    % fetch the two E variables to use
                    nth1(O1,E,E1),
                    nth1(O3,E,E2), 
                    
                    % Get the words
                    nth1(E1,Words,Word1),
                    nth1(E2,Words,Word2),
                    
                    Word1 \= Word2,
                    
                    % The same char in the overlapping.
                    nth1(O2,Word1,C),
                    nth1(O4,Word2,C)
                )
                
        ),
        
        labeling(E),

        writeln('E':E),
        foreach(I in 1..ESize,[Word],
                (
                    Word @= Words[E[I]],
                    format("~d (~2d): ",[I,E[I]]),
                    print_word(Word),
                    nl
                )
        ).




print_word(Word) :-
        foreach(W in Word,
                (
                ground(W) ->
                    format("~w", [W])
                ; 
                    true
                )).


%
% Definition of the words. A _ is used to fill the row.
%
words([[h, o, s, e, s], %%  HOSES
       [l, a, s, e, r], %%  LASER
       [s, a, i, l, s], %%  SAILS
       [s, h, e, e, t], %%  SHEET
       [s, t, e, e, r], %%  STEER
       [h, e, e, l],    %%  HEEL
       [h, i, k, e],    %%  HIKE
       [k, e, e, l],    %%  KEEL
       [k, n, o, t],    %%  KNOT
       [l, i, n, e],    %%  LINE
       [a, f, t],       %%  AFT
       [a, l, e],       %%  ALE
       [e, e, l],       %%  EEL
       [l, e, e],       %%  LEE
       [t, i, e]]).     %%  TIE


%
% Overlappings of the words
% As an array for easy acces 
%
overlapping([]([](1, 3, 2, 1), 
               [](1, 5, 3, 1), 
             
               [](4, 2, 2, 3), 
               [](4, 3, 5, 1), 
               [](4, 4, 3, 3), 
               
               [](7, 1, 2, 4), 
               [](7, 2, 5, 2), 
               [](7, 3, 3, 4), 
               
               [](8, 1, 6, 2), 
               [](8, 3, 2, 5), 
               [](8, 4, 5, 3), 
               [](8, 5, 3, 5))).  
