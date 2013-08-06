/*

  Building Blocks puzzle in SICStus Prolog.

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

  Compare with the the following models: 
  * ECLiPSe: http://www.hakank.org/eclipse/building_blocks.ecl
  
  * http://www.hakank.org/eclipse/labeling_dice.ecl
    This is a more general approach and contains this puzzle
    as an instance.

  * http://www.hakank.org/comet/building_blocks.co
  * http://www.hakank.org/comet/labeled_dice.co



  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :- 
        findall(Backtracks,go1(Backtracks),L),
        sum(L,#=,TotalBacktracks),
        write(total_backtracks:TotalBacktracks),nl.


%
% This is a straight translation of the Comet/ECLiPSe models.
%
go1(Backtracks) :-
        NumCubes = 4, % number of cubes
        words(Words),
        length(Words, NumWords),

        % Convert the letters to integers so we can use ic
        length(WordsIC, NumWords),
        convert_words(Words,WordsIC),
  
        DiceLen = 24,
        length(Dice,DiceLen),
        domain(Dice,1,NumCubes),

        % each letters in a word must be on a different die
        ( for(I,1,NumWords), 
          param(WordsIC,Dice) do
              nth1(I,WordsIC,Word),
              ( foreach(W,Word), 
                foreach(D,ThisDie),
                param(Dice) do
                    element(W,Dice,D)
              ),
              all_different(ThisDie)
        ),

        % there must be exactly 6 letters of each die
        ( for(I,1,NumCubes), 
          param(Dice,DiceLen) do
              ( for(J,1,DiceLen), 
                foreach(Reif,Sum),
                param(I,Dice) do
                    Reif in 0..1,
                    element(J,Dice,DiceJ),
                    Reif #= 1 #<=> DiceJ #= I
              ),
              sum(Sum,#=,6)
        ),


        labeling([ffc], Dice),

        (for(I,1,DiceLen), param(Dice) do 
             nth1(I,Dice,D),
             a(L,I),
             write(L:D),write(' ')
        ),
        nl,
        fd_statistics(backtracks,Backtracks),
        write(backtracks:Backtracks),nl,
        nl.


    
% convert the matrix of letters (Words) to
% a matrix of integers.
convert_words(Words,WordsIC) :-
        ( foreach(Word,Words), 
          fromto(WordsIC, [ThisWord|In],In, []) do 
              ( foreach(W,Word),
                foreach(WI,ThisWord)
              do  
                a(W,WI)
              )
        ).
        

words([[b,a,k,e],
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

% note: we don't use q or z, and renumber the rest.
a(a,1).
a(b,2).
a(c,3).
a(d,4).
a(e,5).
a(f,6).
a(g,7).
a(h,8).
a(i,9).
a(j,10).
a(k,11).
a(l,12).
a(m,13).
a(n,14).
a(o,15).
a(p,16).
% a(q,17).
a(r,17).
a(s,18).
a(t,19).
a(u,20).
a(v,21).
a(w,22).
a(x,23).
a(y,24).
% a(z,26).
