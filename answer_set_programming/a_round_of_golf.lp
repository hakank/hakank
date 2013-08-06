% 
% A Round of Golf puzzle (Dell Logic Puzzles) in ASP.
% 
% From http://brownbuffalo.sourceforge.net/RoundOfGolfClues.html
% """
% Title: A Round of Golf
% Author: Ellen K. Rodehorst
% Publication: Dell Favorite Logic Problems
% Issue: Summer, 2000
% Puzzle #: 9
% Stars: 1
%
% When the Sunny Hills Country Club golf course isn't in use by club members, 
% of course, it's open to the club's employees. Recently, Jack and three other 
% workers at the golf course got together on their day off to play a round of 
% eighteen holes of golf. 
% Afterward, all four, including Mr. Green, went to the clubhouse to total 
% their scorecards. Each man works at a different job (one is a short-order 
% cook), and each shot a different score in the game. No one scored below 
% 70 or above 85 strokes. From the clues below, can you discover each man's 
% full name, job and golf score?
% 
% 1. Bill, who is not the maintenance man, plays golf often and had the lowest 
% score of the foursome.
% 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and scored ten 
% strokes more than the pro-shop clerk.
% 3. In some order, Frank and the caddy scored four and seven more strokes than 
% Mr. Sands.
% 4. Mr. Carter thought his score of 78 was one of his better games, even 
%    though Frank's score  was lower.
% 5. None of the four scored exactly 81 strokes.
% 
% Determine: First Name - Last Name - Job - Score 
% """
%
% Compare with the F1 model: 
% http://www.f1compiler.com/samples/A%20Round%20of%20Golf.f1.html
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

% Solution:
%            Jack  Bill  Paul   Frank
%            Clubb Sands Carter Green
%            maint cook  caddy  clerk
%            85    71    78     75
% first_name: [1, 2, 3, 4]
% last_name : [4, 1, 2, 3]
% job       : [2, 1, 4, 3]
% score     : [85, 71, 78, 75]


% domains
first_names(jack;bill;paul;frank).
last_names(green;clubb;sands;carter).
jobs(cook;maintenance_man;clerk;caddy).
scores(70..85).

% index for last name, job, and score
1 { last_name(Name, LastName) : last_names(LastName) } 1 :- first_names(Name).
1 { job(Name, Job) : jobs(Job) } 1 :- first_names(Name).
1 { score(Name, Score) : scores(Score) } 1 :- first_names(Name).

% alldifferent for last_name, job, and score
1 { last_name(Name, LastName) : first_names(Name) } 1 :- last_names(LastName).
1 { job(Name, Job) : first_names(Name) } 1 :- jobs(Job).
{ score(Name, Score) : first_names(Name) } 1 :- scores(Score). % 0..1 occurrences


%
% clues
%

% 1. Bill, who is not the maintenance man, plays golf often and had 
%    the lowest score of the foursome.
:- job(bill, maintenance_man).

%:- score(bill, BillScore), score(Other, OtherScore), first_names(Other), 
%   bill != Other, BillScore >= OtherScore.
%% alternative encoding:
:- Min = #min [score(P, Score) = Score ], score(bill, BillScore), BillScore > Min.


% 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and 
%    scored ten strokes more than the pro-shop clerk.
:- last_name(paul, clubb).

% score[Clubb] = score[clerk] + 10
:- last_name(Clubb, clubb), score(Clubb, ClubbScore),
   job(Clerk, clerk), score(Clerk, ClerkScore),
   first_names(Clubb), first_names(Clerk),   
   ClubbScore != ClerkScore + 10.

% alternative (postive) encoding:
% p2b :- last_name(Clubb, clubb), score(Clubb, ClubbScore),
%       job(Clerk, clerk), score(Clerk, ClerkScore),
%       ClubbScore = ClerkScore + 10.
%:- not p2b.
                        

% 3. In some order, Frank and the caddy scored four and seven more 
%    strokes than Mr. Sands.
:- job(frank, caddy).
:- last_name(frank, sands).
:- job(Caddy, caddy), last_name(Caddy, sands), first_names(Caddy).

% MiniZinc code:
% (
%  (score[Frank] = score[Sands] + 4 /\ score[caddy] = score[Sands] + 7 )
%   \/
%  (score[Frank] = score[Sands] + 7 /\ score[caddy] = score[Sands] + 4 )
% )

% This could probably be encoded better...
p3d1 :-
        score(frank, FrankScore),
        last_name(Sands, sands),
        first_names(Sands),first_names(Caddy),
        score(Sands, SandsScore),
        job(Caddy, caddy),
        score(Caddy, CaddyScore),
        FrankScore == SandsScore + 4,
        CaddyScore == SandsScore + 7.

p3d2 :-
        score(frank, FrankScore),
        last_name(Sands, sands),
        first_names(Sands),first_names(Caddy),
        score(Sands, SandsScore),
        job(Caddy, caddy),
        score(Caddy, CaddyScore),
        FrankScore = SandsScore + 7,
        CaddyScore = SandsScore + 4.

% not both false
:- not p3d1, not p3d2.

% 4. Mr. Carter thought his score of 78 was one of his better games, even 
%    though Frank's score was lower.
:- last_name(frank, carter).
:- last_name(Carter, carter), score(Carter, CarterScore), CarterScore != 78.      
:- score(frank, FrankScore), last_name(Carter, carter),
        first_names(Carter),
        score(Carter, CarterScore), FrankScore >= CarterScore.

% 5. None of the four scored exactly 81 strokes.
:- score(P, 81), first_names(P).


#hide.
#show last_name(P,L).
#show job(P, J).
#show score(P, S).