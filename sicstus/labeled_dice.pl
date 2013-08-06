/*

  Labeled dice in SICStus Prolog.

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

  Compare with the following models: 
  * ECLiPSe:  http://www.hakank.org/eclipse/building_blocks.co
    (Building Blocks puzzle)
  * Comet  : http://www.hakank.org/comet/labeled_dice.co
  * SICStus: http://www.hakank.org/sicstus/building_blocks.pl


  Note: This is a somewhat generalized model for solving both 
        Building blocks and Labeled Dice problem. 


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        % set solution number
        bb_put(count,0),
        findall(_, solve(labeling_dice, _Res1, _Backtracks1), _L1),
        bb_get(count, Count1),
        format('It was ~d solutions\n', [Count1]),

        bb_put(count,0),
        findall(_, solve(building_blocks, _Res2, _Backtracks2), _L2),
        bb_get(count, Count1),
        format('It was ~d solutions\n', [Count1]).


solve(Problem,Res,_Backtracks) :-
        format('\nProblem ~w\n', [Problem]),


        problem(Problem, NumCubes, NumSides, Letters, Words),

        % create the integer array
        (foreach(_L, Letters),
         count(I,1,_), 
         fromto(LettersInt,[I|In],In,[]) do
             true
        ),

        length(Words, NumWords),

        % Convert the letters to integers so we can use ic
        length(WordsIC, NumWords),
        convert_words(Words,WordsIC,Letters,LettersInt),


        CubeLen is NumCubes * NumSides,
        length(Cube,CubeLen),
        domain(Cube,1,NumCubes),

        % each letters in a word must be on a different die
        ( for(I,1,NumWords), 
          param(WordsIC,Cube) do
              nth1(I,WordsIC,Word),
              ( foreach(W,Word), 
                foreach(D,ThisDie),
                param(Cube) do
                    element(W,Cube,D)
              ),
              all_different(ThisDie)
        ),

        % there must be exactly NumSides (6) letters of each die
        ( for(K,1,NumCubes),
          foreach(C,Cards),
          param(NumSides)
        do
          C = K-NumSides
        ),
        write(Cards),nl,
        global_cardinality(Cube,Cards),

        % simple symmetry breaking: place first letter on cube 1
        element(1,Cube,1),

        %
        % search
        %
        labeling([ffc,bisect,down], Cube),

        (for(I,1,CubeLen), 
         foreach(R, Res), 
         param(Cube,Letters) do 
             double_index(Cube,Letters, I,D,L),
             R = [L,D]
        ),

        % print this solution
        bb_get(count,Count),
        format('\nSolution: ~w\n',[Count]),
        format('Letters placed: ~w\n', [Res]),
        write('\nWords:'),nl,
        (foreach(Word,Words), param(Cube,Letters) do 
             (foreach(W,Word), param(Cube,Letters) do
                  double_index(Cube,Letters,_Ix, C,W),
                  format('~w:~w ',[W,C])
             ),
             nl
        ),
        % print the cubes
        ( for(C,1,NumCubes), 
          param(Cube,CubeLen,Letters) do
              format('Cube ~w: ',[C]),
              ( for(J,1,CubeLen), 
                fromto(ThisCube,Out,In,[]),
                param(C,Cube,Letters) do
                    % is this letter on the C'th cube?
                    double_index(Cube,Letters, J, Val,L),
                    Val == C -> Out = [L|In] ; Out = In
              ),
              % write(ThisCube),nl
              ( foreach(C,ThisCube) do
                    write(C), write(' ')
              ),
              nl
        ),
        nl,
        CountPlus1 is Count + 1,
        bb_put(count,CountPlus1). % increment solution number

%
% Lookup a value given an index and/or some value Val1 or Val2
%
double_index(List1,List2,Ix,Val1,Val2) :-
        nth1(Ix, List1, Val1),
        nth1(Ix, List2, Val2).
        

    
% convert the matrix of letters (Words) to
% a matrix of integers.
convert_words(Words,WordsIC,Letters,LettersInt) :-
        ( foreach(Word,Words), 
          fromto(WordsIC, [ThisWord|In],In, []),
          param(Letters,LettersInt) do 
              ( foreach(W,Word),
                foreach(WI,ThisWord),
                param(Letters,LettersInt)
              do  
                double_index(Letters,LettersInt,_Ix,W,WI)
              )
        ).


%
% Labeling Dice
% 
problem(labeling_dice, 
        4, % number of cubes
        6, % number of sides of of a cube
        % the letters to use
        [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,y], 
        % the words to place
        [[b,u,o,y],                                        
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
         [w,i,s,h]]).


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
problem(building_blocks,
        4,
        6,
        [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y],
        [[b,a,k,e],
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
         [w,r,a,p]]).
        

