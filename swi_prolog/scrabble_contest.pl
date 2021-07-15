/* 

  Scrabble Contest puzzle in SWI-Prolog.

  From https://www.braingle.com/brainteasers/24501/scrabble-contest.html
  """
  At the local games evening, four lads were competing in the Scrabble and 
  chess competitions. Liam beat Mark in chess, James came third and the 16 year old 
  won. Liam came second in Scrabble, the 15 year old won; James beat the 18 year old 
  and the 19 year old came third. Kevin is 3 years younger than Mark. The person who 
  came last in chess, came third in Scrabble and only one lad got the same position in 
  both games. Can you determine the ages of the lads and the positions in the two games?
  """

  (Via https://stackoverflow.com/questions/68301400/simplifying-constraints-in-clp-puzzle )

   
  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI-Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).

go :-
        % At the local games evening, four lads were competing in the Scrabble and 
        % chess competitions.
        N = 4,
        length(Lads,N),
        Lads = [James,Kevin,Liam,Mark],
        Lads = [1,2,3,4],
        Lads ins 1..N,
        LadsS = ['James','Kevin','Liam','Mark'],
        
        length(Chess,N),
        Chess ins 1..N,
        
        length(Scrabble,N),
        Scrabble ins 1..N,

        length(Ages,N),
        Ages ins 15..16 \/ 18..19,

        all_different(Chess),
        all_different(Scrabble),
        all_different(Ages),

        element(Ages15,Ages,15),
        element(Ages16,Ages,16),
        element(Ages18,Ages,18),
        element(Ages19,Ages,19),
        
        % Liam beat Mark in chess, James came third and the 16 year old won.
        element(Liam,Chess,ChessLiam),
        element(Mark,Chess,ChessMark),
        ChessLiam #< ChessMark,
        element(James,Chess,3),
        element(Ages16,Chess,1),

        % Liam came second in Scrabble, the 15 year old won; James beat the 18 year old 
        % and the 19 year old came third.
        element(Liam,Scrabble,2),
        element(Ages15,Scrabble,1),

        element(Ages18,Scrabble,ScrabbleAges18),
        element(James,Scrabble,ScrabbleJames),
        ScrabbleJames #< ScrabbleAges18,
        element(Ages19,Scrabble,3),

        % Kevin is 3 years younger than Mark.
        element(Kevin,Ages,AgesKevin),
        element(Mark,Ages,AgesMark),  
        AgesKevin + 3 #= AgesMark,
        
        % The person who came last in chess, came third in Scrabble and only one lad
        % got the same position in both games.
        element(ChessPlace4,Chess,4),
        element(ChessPlace4,Scrabble,3),

        sums(Chess,Scrabble,0,Sums),
        Sums #= 1,
        
        % Can you determine the ages of the lads and the positions in the two games?

        flatten([Chess,Scrabble,Ages,Sums],Vars),
        label(Vars),
        
        writeln(chess=Chess),
        writeln(scrabble=Scrabble),
        writeln(ages=Ages),
        sol(LadsS,Ages,Scrabble,Chess),
        nl,
        
        fail,
        nl.
go.


sums([],[],Total,Total).
sums([C|Chess],[S|Scrabble],Total0,Total) :-
        B in 0..1,
        C #= S #<==> B #= 1,
        Total1 #= Total0 + B,         
        sums(Chess,Scrabble,Total1,Total).

sol([L|Lads],[A|Ages],[S|Scrabble],[C|Chess]) :-
        format("~w (~d) Scrabble: ~d Chess: ~d~n",[L,A,S,C]),
        sol(Lads,Ages,Scrabble,Chess).