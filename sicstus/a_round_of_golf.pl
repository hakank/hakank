/*

  A Round of Golf puzzle (Dell Logic Puzzles) in SICStus Prolog.

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
  * ECLiPSe : http://www.hakank.org/eclipse/a_round_of_golf.ecl



  Note [from the ECLiPSe model]: 
  The solution at the Brownbuffalo site (see link above) is
  actually an ECLiPSe/Prolog program, but I have used a CLP approach here. 
  Compared to the MiniZinc and Comet models, ECLiPSe
  don't handle array access in the same way:
  When both the array and index is (free) decision variables subscript/3
  gives an instatiation fault. Hence all these the nth1 constructs,
  which operates on a list, hence the ScoreList.
  


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%
% Note: This is a translation of the logics of the MiniZinc/Comet model,
%       and it shows...
%
go :-

        N = 4,

        Jack = 1,
        Bill = 2,
        Paul = 3,
        Frank = 4,
        FirstName = [Jack, Bill, Paul, Frank],

        LastName = [_Green,Clubb,Sands,Carter],
        domain(LastName,1,N),

        Job = [_Cook,MaintenanceMan,Clerk,Caddy],
        domain(Job,1,N),
        
        length(Score,N),
        domain(Score,70,85),
        Score = [ScoreJack,ScoreBill,ScorePaul,ScoreFrank],

        all_different(LastName),
        all_different(Job),
        all_different(Score),
        
        % 1. Bill, who is not the maintenance man, plays golf often and had 
        %    the lowest score of the foursome.
        Bill #\= MaintenanceMan,
        ScoreBill #< ScoreJack,
        ScoreBill #< ScorePaul,
        ScoreBill #< ScoreFrank,

        % 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and 
        %    scored ten strokes more than the pro-shop clerk.
        Clubb #\= Paul,
        element(Clubb,Score,ScoreClubb),
        element(Clerk,Score,ScoreClerk),
        ScoreClubb #= ScoreClerk + 10,
   
       
        % 3. In some order, Frank and the caddy scored four and seven more 
        %    strokes than Mr. Sands.
        Frank #\= Caddy,
        Frank #\= Sands,
        Caddy #\= Sands,
        
        element(Sands,Score,ScoreSands),
        element(Caddy,Score,ScoreCaddy),
        element(Carter,Score,ScoreCarter),
        (
            ScoreFrank #= ScoreSands + 4,
            ScoreCaddy #= ScoreSands + 7
        ;
            ScoreFrank #= ScoreSands + 7,
            ScoreCaddy #= ScoreSands + 4
        ),
  
        % 4. Mr. Carter thought his score of 78 was one of his better games, even 
        % though Frank's score was lower.
        Frank #\= Carter,
        ScoreCarter #= 78,
        ScoreFrank #< ScoreCarter,

        % 5. None of the four scored exactly 81 strokes.
        (foreach(S, Score) do
             S #\= 81
        ),

        % labeling
        append(LastName,Job,Vars1),
        append(Vars1,Job,Vars2),
        append(Vars2,Score,Vars),
        labeling([], Vars),

        % output
        write(['Jack', 'Bill', 'Paul', 'Frank']),nl,
        write(first_name:FirstName),nl,
        write(last_name:LastName),nl,
        write(job:Job),nl,
        write(score:Score),nl,nl,

        % and another point of view
        Names = ['Jack', 'Bill', 'Paul', 'Frank'],
        ( foreach(Name,Names),
          foreach(First,FirstName),
          foreach(Last,LastName),
          foreach(J,Job),
          foreach(S2,Score) do
              write([Name,First,Last,J,S2]),nl
        ),nl,
        fd_statistics.
   