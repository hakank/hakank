/*

  Labeled dice and Building blocks problems in SWI Prolog

  Labeled dice
  --------------
  From Jim Orlin "Colored letters, labeled dice: a logic puzzle"
  http://jimorlin.wordpress.com/2009/02/17/colored-letters-labeled-dice-a-logic-puzzle/
  """
  My daughter Jenn bough a puzzle book, and showed me a cute puzzle.  There 
  are 13 words as follows:  BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY, 
  JUNK, LIMN, QUIP, SWAG, VISA, WISH.

  There are 24 different letters that appear in the 13 words.  The question 
  is:  can one assign the 24 letters to 4 different cubes so that the 
  four letters of each word appears on different cubes.  (There is one 
  letter from each word on each cube.)  It might be fun for you to try 
  it.  I'll give a small hint at the end of this post. The puzzle was 
  created by Humphrey Dudley.
  """

  Building blocks
  ---------------
  From http://brownbuffalo.sourceforge.net/BuildingBlocksClues.html
  """
  Each of four alphabet blocks has a single letter of the alphabet on each 
  of its six sides. In all, the four blocks contain every letter but 
  Q and Z. By arranging the blocks in various ways, you can spell all of 
  the words listed below. Can you figure out how the letters are arranged 
  on the four blocks?

  BAKE ONYX ECHO OVAL
  GIRD SMUG JUMP TORN
  LUCK VINY LUSH WRAP
  """

  Note: This is a somewhat generalized model for solving both 
        Building blocks and Labeled Dice problems. 


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        go(labeling_dice),
        nl,
        go(building_blocks),
        nl.


go(Problem) :-
        findall(Res,solveit(Problem, Res),L),
        length(L,Len),
        format("It was ~d solutions for problem ~w.~n", [Len,Problem]).


solveit(Problem,Res) :-

   format("~nProblem ~w~n", Problem),
   problem(Problem, NumCubes, NumSides, Letters, Words),


   % Convert the letters to integers so we can use ic

   %% create the integer array
   %% LettersInt = [I : I in 1..Letters.length],
   length(Letters,LettersLength),
   numlist(1,LettersLength,LettersInt),
   
   length(Words,NumWords),
   length(WordsIC,NumWords),

   %%
   %% Convert each word to an integer representation (a=1, b=2, etc).
   %%
   convert_words(Words,WordsIC,Letters,LettersInt),

   CubeLen #= NumCubes * NumSides,
   length(Cube,CubeLen),
   Cube ins 1..NumCubes,

   % each letters in a word must be on a different die
   maplist(letters_on_different_dice(Cube),WordsIC),
   
   
   %% there must be exactly NumSides (6) letters of each die
   findall(I-NumSides,between(1,NumCubes,I),GCC),
   global_cardinality(Cube, GCC),
   
   % simple symmetry breaking: place first letter on cube 1
   element(1,Cube,1),

   % search
   labeling([ffc],Cube),

   writeln("\nSolution:"),
   writeln(cube=Cube),

   %%
   %% pretty print of the solution
   %%
   
   % Letters placed
   % Res = [[L,D]  : I in 1..CubeLen, double_index(Cube,Letters, I,D,L)],
   findall([L,C],(between(1,CubeLen,I),
                nth1(I,Cube,C),
                nth1(I,Letters,L)
               ),
           Res),
   format("Letters placed on which cube: ~w~n", [Res]),

   writeln("\nThe words placed on which cube ([letter:cube]):"),
   findall([Word,WC],(member(Word,Words),
                  findall([W,C],
                          (member(W,Word),
                           double_index(Cube,Letters,_Ix, C,W)
                           ),
                          WC)
                 ),
          CubeWords
          ),
   maplist(writeln,CubeWords),
   
   %% print the cubes
   writeln("\nThe Cubes:"),
   findall([C,Lss],
           (between(1,NumCubes,C),
            findall(L,
                    member([L,C],Res),
                    Lss
                   )
            ),
           TheCubes
          ),
   maplist(writeln,TheCubes),
   nl.

%%
%% each letters in a word must be on a different die
%%
letters_on_different_dice(Cube,WordICs) :-
        extract_from_indices(WordICs,Cube,CubeIx),
        all_different(CubeIx).

%%
%% Lookup a value given an index and/or some value Val1 or Val2
%%
double_index(List1,List2,Ix,Val1,Val2) :-
        nth1(Ix, List1, Val1),
        nth1(Ix, List2, Val2).
    
% convert the matrix of letters (Words) to a matrix of integers.
convert_words(Words,WordsIC,Letters,LettersInt) :-
        maplist(convert_word(Letters,LettersInt),Words,WordsIC).

convert_word(Letters,LettersInt,Word,WordIC) :-
        maplist(convert_char(Letters,LettersInt),Word,WordIC).

convert_char(Letters,LettersInt,Char,CharIC) :-
        double_index(Letters, LettersInt,_Ix,Char,CharIC).

       

%
% Labeling Dice
% 
problem(labeling_dice, N, S, Letters, Words) :- 
        % number of cubes
        N = 4,  
        % number of sides of of a cube
        S = 6,  
        % the letters to use
        Letters = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,y], 
        % the words to place
        Words = [[b,u,o,y],               
                 [c,a,v,e], 
                 [c,e,l,t], 
                 [f,l,u,b], 
                 [f,o,r,k], 
                 [h,e,m,p], 
                 [j,u,d,y], 
                 [j,u,n,k], 
                 [l,i,m,n], 
                 [q,u,i,p], 
                 [s,w,a,g], 
                 [v,i,s,a], 
                 [w,i,s,h]].


%
% Building Blocks
%   From http://brownbuffalo.sourceforge.net/BuildingBlocksClues.html
%   """
%   Each of four alphabet blocks has a single letter of the alphabet on each 
%   of its six sides. In all, the four blocks contain every letter but 
%   Q and Z. By arranging the blocks in various ways, you can spell all of 
%   the words listed below. Can you figure out how the letters are arranged 
%   on the four blocks?
%
%   BAKE ONYX ECHO OVAL
%   GIRD SMUG JUMP TORN 
%   LUCK VINY LUSH WRAP
%   """
problem(building_blocks, N, S, Letters, Words) :-
        N = 4,
        S = 6,
        Letters = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y],
        Words = [[b,a,k,e],
                 [o,n,y,x],
                 [e,c,h,o],
                 [o,v,a,l],
                 [g,i,r,d],
                 [s,m,u,g],
                 [j,u,m,p],
                 [t,o,r,n],
                 [l,u,c,k],
                 [v,i,n,y],
                 [l,u,s,h],
                 [w,r,a,p]].
