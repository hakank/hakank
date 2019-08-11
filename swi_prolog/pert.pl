/*

  Simple PERT model in SWI Prolog

  From Pascal van Hentenryck 
  "Scheduling and Packing In the Constraint Language cc(FD)", page 7f
  http://citeseer.ist.psu.edu/300151.html

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        MaxTime = 30,
        times(Times),
        dependencies(Dependencies),
        pert(Dependencies,Times,MaxTime, Start,SEnd,SumTimes),

        writeln(sEnd=SEnd),
        writeln(start_times=Start),
        writeln(sum_times=SumTimes),
        nl.

pert(Dependencies,Times,MaxTime, Start,SEnd,SumTimes) :-
        
        length(Times,N),

        length(Start,N),
        Start ins 0..MaxTime,   
        maplist(check_dependencies(Start,Times),Dependencies),
        
        element(N,Start,SEnd),
        sum(Start,#=,SumTimes),

        labeling([min(SEnd)], Start).
        

check_dependencies(Start,Times,[D1,D2]) :-
        element(D1,Start,StartD1),
        element(D2,Start,StartD2),
        element(D2,Times,TimesD2),
        StartD1 #>= StartD2 + TimesD2.


% Times for each action
%                        a  b  c  d  e  f  g  h  j  k  Send 
times(Times) :- Times = [7, 3, 1, 8, 1, 1, 1, 3, 2, 1, 1].

% Dependencies
% Note: There is no Si
dependencies(Dependencies) :-
        Dependencies =
        [[2,1],  % Sb >= Sa + 7
         [4,1],  % Sd >= Sa + 7
         [3,2],  % Sc >= Sb + 3
         [5,3],  % Se >= Sc + 1
         [5,4],  % Se >= Sd + 8
         [7,3],  % Sg >= Sc + 1
         [7,4],  % Sg >= Sd + 8
         [6,4],  % Sf >= Sd + 8
         [6,3],  % Sf >= Sc + 1
         [8,6],  % Sh >= Sf + 1
         [9,8],  % Sj >= Sh + 3
         [10,7], % Sk >= Sg + 1
         [10,5], % Sk >= Se + 1
         [10,9], % Sk >= Sj + 2
         [11,10] % Send >= Sk + 1
        ].
        