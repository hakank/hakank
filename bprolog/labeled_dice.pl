/*

  Labeled dice and Building blocks problems in B-Prolog.

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
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        go(labeling_dice), 
        go(building_blocks).

go(Problem) :-
        
        findall(Res,solve(Problem, Res),L),
        length(L,Len),
        format("It was ~d solutions.\n", [Len]).


solve(Problem,Res) :-

        format("\nProblem ~w\n", [Problem]),
        problem(Problem, NumCubes, NumSides, Letters, Words),


        % Convert the letters to integers so we can use ic

        % create the integer array
        LettersInt @= [I : I in 1..Letters^length],
        length(Words, NumWords),
        length(WordsIC, NumWords),
        convert_words(Words,WordsIC,Letters,LettersInt),
  
        CubeLen is NumCubes * NumSides,
        length(Cube,CubeLen),
        Cube :: 1..NumCubes,

        % each letters in a word must be on a different die
        foreach(I in 1..NumWords,[ThisDie,Word],
                (nth1(I,WordsIC,Word),
                 ThisDie @= [D : W in Word, [D], nth1(W,Cube,D)],
                 alldifferent(ThisDie))
               ),

        % there must be exactly NumSides (6) letters of each die
        foreach(I in 1..NumCubes, 
                sum([(R #= I) : J in 1..CubeLen, [R], nth1(J,Cube,R)]) #= NumSides
               ),

        % simple symmetry breaking: place first letter on cube 1
        nth1(1,Cube,1),

        % search
        labeling(Cube),

        writeln(cube:Cube),

        % print this solution

        % Letters placed
        Res @= [[L,D]  : I in 1..CubeLen, [R,L,D], double_index(Cube,Letters, I,D,L)],
        format("Letters placed: ~w\n", [Res]),

        writeln('\nWords:'),
        foreach(Word in Words,
                (foreach(W in Word,[Ix,C],
                         (double_index(Cube,Letters,Ix, C,W),
                          format("~w: ~w ",[W,C]))
                        ),
                 nl)
               ),

        % print the cubes
        writeln('\nCubes:'),
        foreach(C in 1..NumCubes, [ThisCube,ThisCube2],
                (format("Cube ~w: ",[C]),
                 foreach(J in 1..CubeLen, ac(ThisCube,[]),[Val,L],
                         (% is this letter on the C'th cube?
                             double_index(Cube,Letters, J, Val,L),
                             Val == C -> ThisCube^1 = [L|ThisCube^0] ; ThisCube^1 = ThisCube^0
                         )
                        ),
                 reverse(ThisCube,ThisCube2),
                 writeln(ThisCube2)
                )),
        nl, nl.


%
% Lookup a value given an index and/or some value Val1 or Val2
%
double_index(List1,List2,Ix,Val1,Val2) :-
        nth1(Ix, List1, Val1),
        nth1(Ix, List2, Val2).
        

    
% convert the matrix of letters (Words) to
% a matrix of integers.
convert_words(Words,WordsIC,Letters,LettersInt) :-
        foreach(Word in Words, [ThisWord],ac(WordsIC2,[]),
                (
                    ThisWord @= [WI : W in Word, [Ix,WI], double_index(Letters, LettersInt,Ix,W,WI)],
                    WordsIC2^1 = [ThisWord|WordsIC2^0]
                )
        ),
        WordsIC = WordsIC2.


%
% Labeling Dice
% 
problem(labeling_dice, 
        % number of cubes
        4,  
        % number of sides of of a cube
        6,  
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
