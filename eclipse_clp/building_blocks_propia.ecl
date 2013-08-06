/*

  Building Blocks puzzle in ECLiPSe.

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
  * http://www.hakank.org/eclipse/labeling_dice.ecl
    This is a more general approach and contains this puzzle
    as an instance.

  * http://www.hakank.org/comet/building_blocks.co
  * http://www.hakank.org/comet/labeled_dice.co



  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(listut).
:-lib(propia).

go :-
        NumCubes = 4, % number of cubes
        words(Words),
        dim(Words, [NumWords,WordLength]),

        % Convert the letters to integers so we can use ic
        dim(WordsIC, [NumWords,WordLength]),
        convert_words(Words,WordsIC),
  
        DiceLen = 24,
        dim(Dice,[DiceLen]),
        Dice :: 1..NumCubes,

        % each letters in a word must be on a different die
        ( for(I,1,NumWords), param(WordsIC,Dice,WordLength) do
              Word is WordsIC[I],
              dim(ThisDie,[WordLength]),
              ( foreacharg(W,Word), foreacharg(D,ThisDie),
                param(Dice) do
                    D #= Dice[W]
              ),
              ic_global:alldifferent(ThisDie)
        ),

        % there must be exactly 6 letters of each die
        ( for(I,1,NumCubes), 
          param(Dice,DiceLen) do
              ( for(J,1,DiceLen), foreach(Reif,Sum), 
                param(I,Dice) do
                    Reif = (Dice[J] #= I)
              ),
              sum(Sum) #= 6
        ),


        search(Dice,0,occurrence,indomain_min,complete,[backtrack(Backtracks)]),
        (for(I,1,DiceLen), param(Dice) do 
             D #= Dice[I],
             a(L,I),
             writeln(L:D)
        ),
        writeln(backtracks:Backtracks),
        nl,
        % we show all solutions...
        fail.


    
% convert the matrix of letters (Words) to
% a matrix of integers.
convert_words(Words,WordsIC) :-
        dim(Words,[Len,WLen]),
        dim(WordsIC, [Len,WLen]),
        ( foreacharg(Word,Words), 
          foreacharg(WI,WordsIC) do
              convert_word(Word,WI)
        ).
        
convert_word(Word,WordIC) :-
        dim(Word,[Len]),
        dim(WordIC,[Len]),
        ( foreacharg(W,Word), 
          foreacharg(WI,WordIC) do
              a(W,WI)
        ).



words([]([](b,a,k,e),
         [](o,n,y,x),
         [](e,c,h,o),
         [](o,v,a,l),
         [](g,i,r,d),
         [](s,m,u,g),
         [](j,u,m,p),
         [](t,o,r,n),
         [](l,u,c,k),
         [](v,i,n,y),
         [](l,u,s,h),
         [](w,r,a,p))).

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
