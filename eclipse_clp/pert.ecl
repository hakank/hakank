/*

  Simple PERT model in ECLiPSe.

  From Pascal van Hentenryck 
  "Scheduling and Packing In the Constraint Language cc(FD)", page 7f
  http://citeseer.ist.psu.edu/300151.html

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/pert.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/pert.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(listut).
:-lib(ic_global).
%:-lib(ic_search).
:-lib(branch_and_bound).

%:-lib(propia).



go :-
        MaxTime = 30,
        times(Times),
        length(Times, N),
        dependencies(Dependencies),

        % decision variable
        length(Start, N),
        Start :: 0..MaxTime,
        
        % constraints
        ( foreach([D1,D2], Dependencies),
          param(Start,Times) do
              nth1(D1,Start,SD1),
              nth1(D2,Start,SD2),
              nth1(D2,Times,T2),
              SD1 #>= SD2 + T2
        ),
        nth1(N,Start,SEnd), % to minimize
        sum(Start)#=SumTimes,

        % search
        % labeling([max,step,up,minimize(SEnd)], Start),
        minimize(labeling(Start),SEnd),

        % output
        write(start_times:Start),nl,
        write(sum_times:SumTimes),nl,nl.


% Times for each action
%      a  b  c  d  e  f  g  h  j  k  Send 
times([7, 3, 1, 8, 1, 1, 1, 3, 2, 1, 1]).

% Dependencies
% Note: There is no Si
dependencies([[2,1],  % Sb >= Sa + 7
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
             ]).
             