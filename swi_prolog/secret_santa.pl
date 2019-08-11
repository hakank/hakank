/*

  Secret Santa problem in SWI Prolog

  From Ruby Quiz Secret Santa
  http://www.rubyquiz.com/quiz2.html
  """
  Honoring a long standing tradition started by my wife's dad, my friends 
  all play a Secret Santa game around Christmas time. We draw names and 
  spend a week sneaking that person gifts and clues to our identity. On the 
  last night of the game, we get together, have dinner, share stories, and, 
  most importantly, try to guess who our Secret Santa was. It's a crazily 
  fun way to enjoy each other's company during the holidays.
  
  To choose Santas, we use to draw names out of a hat. This system was 
  tedious, prone to many "Wait, I got myself..." problems. This year, we 
  made a change to the rules that further complicated picking and we knew 
  the hat draw would not stand up to the challenge. Naturally, to solve 
  this problem, I scripted the process. Since that turned out to be more 
  interesting than I had expected, I decided to share.
  
  This weeks Ruby Quiz is to implement a Secret Santa selection script.
  
  Your script will be fed a list of names on STDIN. 
  ...
  Your script should then choose a Secret Santa for every name in the list. 
  Obviously, a person cannot be their own Secret Santa. In addition, my friends 
  no longer allow people in the same family to be Santas for each other and your 
  script should take this into account.
  """

  Comment: Well, this model skips the file input and mail parts. We 
           assume that the friends are identified with a number from 1..n,
           and the families is identified with a number 1..num_families. 


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   %% Ruby Quiz example
   % Family = [1,1,2,2, 3, 4,4],
   Family = [1,1,1,1, 2, 3,3,3,3,3, 4,4], % extended version
   length(Family, N),

   length(X,N),
   X ins 1..N,

   %% Everyone gives and receives a Secret Santa
   all_different(X),

   %% Can't be one own's Secret Santa
   numlist(1,N,Is),
   maplist(no_self_santa(X),Is),
   
   %% No Secret Santa to a person in the same family
   maplist(no_santa_in_family(X,Family), Is),
   label(X),

   %% Solution
   writeln(X),
   findall([I,FamilyI,XI,FamilyXI],
           (between(1,N,I),
            element(I,Family,FamilyI),
            element(I,X,XI),
            element(XI,Family,FamilyXI)
           ),
           Res
          ),
   maplist(format("Person ~d (family ~d) is a Secret Santa of ~d (family ~d)~n"),Res),
            
   nl.

no_self_santa(X,I) :-
        element(I,X,XI),
        XI #\= I.

no_santa_in_family(X, Family, I) :-
        element(I,X,XI),
        element(XI,Family,FXI),
        element(I,Family,FamilyI),
        FamilyI #\= FXI.
