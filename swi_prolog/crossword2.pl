/*

  Crossword problem in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        overlapping(Overlappings),
        length(Overlappings,NumOverlappings),
        words(Words),
        length(Words,NumWords),

        ESize = 8,

        %% decision variables
        length(E,ESize),
        E ins 1..NumWords,
        
        all_different(E),

        numlist(1,NumOverlappings,Is),
        maplist(do_overlappings(Overlappings,E,Words),Is),
        label(E),

        writeln(e=E),
        numlist(1,ESize,Ws),
        maplist(print_word(Words,E),Ws),
        nl.

print_word(Words,E,I) :-
        nth1(I,E,EI),
        nth1(EI,Words,Word),
        format("~w (~w) ~s~n",[I,EI,Word]).
        

do_overlappings(Overlappings,E,Words,I) :-
        
        %% first word        
        matrix_element(Overlappings,I,1,O1),       
        %% position in first word
        matrix_element(Overlappings,I,2,O2),
        %% second word
        matrix_element(Overlappings,I,3,O3),
        %% position in second word
        matrix_element(Overlappings,I,4,O4),
        
        %% fetch the two E variables to use        
        nth1(O1,E,E1),
        nth1(O3,E,E2), 

        %% Get the words
        nth1(E1,Words,Word1),
        nth1(E2,Words,Word2),

        Word1 \= Word2,

        %% The same char in the overlapping.
        nth1(O2,Word1,C),
        nth1(O4,Word2,C).

%
% Definition of the words. A _ is used to fill the row.
%
words(Words) :- 
        Words= [[h, o, s, e, s], %%  HOSES
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
                [t, i, e]].      %%  TIE


%
% Overlappings of the words
% As an array for easy acces 
%
overlapping(Overlapping) :- 
        Overlapping =
        [[1, 3, 2, 1], 
         [1, 5, 3, 1], 
         
         [4, 2, 2, 3], 
         [4, 3, 5, 1], 
         [4, 4, 3, 3], 
         
         [7, 1, 2, 4], 
         [7, 2, 5, 2], 
         [7, 3, 3, 4], 
         
         [8, 1, 6, 2], 
         [8, 3, 2, 5], 
         [8, 4, 5, 3], 
         [8, 5, 3, 5]].  
