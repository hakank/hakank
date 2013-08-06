/*

  A Round of Golf puzzle (Dell Logic Puzzles) in ECLiPSe.
  
  From http://brownbuffalo.sourceforge.net/RoundOfGolfClues.html
  """
  Title: A Round of Golf
  Author: Ellen K. Rodehorst
  Publication: Dell Favorite Logic Problems
  Issue: Summer, 2000
  Puzzle #: 9
  Stars: 1
 
  When the Sunny Hills Country Club golf course isn't in use by club members, 
  of course, it's open to the club's employees. Recently, Jack and three other 
  workers at the golf course got together on their day off to play a round of 
  eighteen holes of golf. 
  Afterward, all four, including Mr. Green, went to the clubhouse to total 
  their scorecards. Each man works at a different job (one is a short-order 
  cook), and each shot a different score in the game. No one scored below 
  70 or above 85 strokes. From the clues below, can you discover each man's 
  full name, job and golf score?
  
  1. Bill, who is not the maintenance man, plays golf often and had the lowest 
  score of the foursome.
  2. Mr. Clubb, who isn't Paul, hit several balls into the woods and scored ten 
  strokes more than the pro-shop clerk.
  3. In some order, Frank and the caddy scored four and seven more strokes than 
  Mr. Sands.
  4. Mr. Carter thought his score of 78 was one of his better games, even 
     though Frank's score  was lower.
  5. None of the four scored exactly 81 strokes.
  
  Determine: First Name - Last Name - Job - Score 
  """

  Compare with the F1 model: 
  http://www.f1compiler.com/samples/A 20Round 20of 20Golf.f1.html

  Solution:
             Jack, Bill, Paul, Frank
             Clubb Sands Carter Green
             maint cook  caddy clerk
             85    71    78    75
  first_name: [1, 2, 3, 4]
  last_name : [4, 1, 2, 3]
  job       : [2, 1, 4, 3]
  score     : [85, 71, 78, 75]


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/a_round_of_golf.mzn
  * Comet   : http://www.hakank.org/comet/a_round_of_golf.co



  Note: The solution at the Brownbuffalo site (see link above) is
  actually an ECLiPSe/Prolog program, but I have used a CLP approach here. 
  Compared to the MiniZinc and Comet models, ECLiPSe
  don't handle array access in the same way:
  When both the array and index is (free) decision variables subscript/3
  gives an instatiation fault. Hence all these the nth1 constructs,
  which operates on a list, hence the ScoreList.
  


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(listut).
%:-lib(propia).



go :-

        N = 4,

        Jack = 1,
        Bill = 2,
        Paul = 3,
        Frank = 4,
        FirstName = [Jack, Bill, Paul, Frank],

        LastName = [_Green,Clubb,Sands,Carter],
        LastName :: 1..N,

        Job = [_Cook,MaintenanceMan,Clerk,Caddy],
        Job :: 1..N,
        
        dim(Score,[N]),
        Score :: 70..85,
        collection_to_list(Score,ScoreList),

        ic_global:alldifferent(LastName),
        ic_global:alldifferent(Job),
        ic_global:alldifferent(Score),
        
        % 1. Bill, who is not the maintenance man, plays golf often and had 
        %    the lowest score of the foursome.
        Bill #\= MaintenanceMan,

        Score[Bill] #< Score[Jack],
        Score[Bill] #< Score[Paul],
        Score[Bill] #< Score[Frank],
        
        % 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and 
        %    scored ten strokes more than the pro-shop clerk.
        Clubb #\= Paul,

        % Score[Clubb] #= Score[Clerk] + 10
        nth1(Clubb,ScoreList,ScoreClubb),
        nth1(Clerk,ScoreList,ScoreClerk),
        ScoreClubb #= ScoreClerk + 10,
   
       
        % 3. In some order, Frank and the caddy scored four and seven more 
        %    strokes than Mr. Sands.
        Frank #\= Caddy,
        Frank #\= Sands,
        Caddy #\= Sands,
        
        nth1(Frank,ScoreList,ScoreFrank),
        nth1(Sands,ScoreList,ScoreSands),
        nth1(Caddy,ScoreList,ScoreCaddy),
        (
            % Score[Frank] #= Score[Sands] + 4,
            % Score[Caddy] #= Score[Sands] + 7
            ScoreFrank #= ScoreSands + 4,
            ScoreCaddy #= ScoreSands + 7
        ;
            % Score[Frank] #= Score[Sands] + 7,
            % Score[Caddy] #= Score[Sands] + 4
            ScoreFrank #= ScoreSands + 7,
            ScoreCaddy #= ScoreSands + 4
        ),
  
        % 4. Mr. Carter thought his score of 78 was one of his better games, even 
        % though Frank's score was lower.
        Frank #\= Carter,
        
        % Score[Carter] #= 78
        nth1(Carter,ScoreList,ScoreCarter),
        ScoreCarter #= 78,
        ScoreFrank #< ScoreCarter,

        % 5. None of the four scored exactly 81 strokes.
        (foreacharg(S, Score) do
             S #\= 81
        ),

        term_variables([LastName,Job,Score], Vars),

        labeling(Vars),
        
        writeln(first_name:FirstName),
        writeln(last_name:LastName),
        writeln(job:Job),
        writeln(score:ScoreList),fail.
   