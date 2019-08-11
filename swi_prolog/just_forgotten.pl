/*

  Just forgotten puzzle (Enigma 1517) in SWI Prolog

  From http://www.f1compiler.com/samples/Enigma 201517.f1.html
  """
  Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.
 
  Joe was furious when he forgot one of his bank account numbers. 
  He remembered that it had all the digits 0 to 9 in some order, so he tried
  the following four sets without success:
 
      9 4 6 2 1 5 7 8 3 0
      8 6 0 4 3 9 1 2 5 7 
      1 6 4 0 2 9 7 8 5 3
      6 8 2 4 3 1 9 0 7 5
 
  When Joe finally remembered his account number, he realised that in each set
  just four of the digits were in their correct position and that, if one knew
  that, it was possible to work out his account number.
  What was it? 
  """
 
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   A = [[9,4,6,2,1,5,7,8,3,0],
        [8,6,0,4,3,9,1,2,5,7],
        [1,6,4,0,2,9,7,8,5,3],
        [6,8,2,4,3,1,9,0,7,5]],

   length(Xs,10),
   Xs ins 0..9,

   all_different(Xs),
   maplist(four_correct_digits(Xs),A),
   
   labeling([],Xs),
   
   writeln(Xs),
   nl.

%%
%% Each row contains exactly 4 correct digits.
%%
four_correct_digits(Xs,Row) :-
        sum_row(Xs,Row,0,4). 

sum_row([],[],Sum,Sum).
sum_row([X|Xs],[R|Rs],Sum0,Sum) :-
        R #= X,
        Sum1 #= Sum0 + 1,
        sum_row(Rs,Xs,Sum1,Sum).
sum_row([_X|Xs],[_R|Rs],Sum0,Sum) :-
        %% R #\= X, % slightly faster without this
        sum_row(Xs,Rs,Sum0,Sum).
        