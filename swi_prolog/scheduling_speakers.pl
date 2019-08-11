/*

  Scheduling speakers in SWI Prolog

  From Rina Dechter, Constraint Processing, page 72
  Scheduling of 6 speakers in 6 slots.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   N = 6, % number of speakers

   % the available slots 
   Available = 
       [
                       %% Reasoning:        
         [3,4,5,6],    %% 2) the only one with 6 after speaker F -> 1
         [3,4],        %% 5) 3 or 4
         [2,3,4,5],    %% 3) only with 5 after F -> 1 and A -> 6
         [2,3,4],      %% 4) only with 2 after C -> 5 and F -> 1 
         [3,4],        %% 5) 3 or 4
         [1,2,3,4,5,6] %% 1) the only with 1
       ],

   findall(Xs,schedule_speakers(N,Available,Xs),L),
   maplist(writeln,L),
   nl.


schedule_speakers(N,Available,Xs) :-

   % the alotted speaker slot
   length(Xs,N),
   Xs ins 1..N,

   all_different(Xs),
   maplist(check_slots,Xs,Available),

   label(Xs).

check_slots(X,Available) :-
        member(X,Available).