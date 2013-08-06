/*

  Simple PERT model in B-Prolog.

  From Pascal van Hentenryck 
  "Scheduling and Packing In the Constraint Language cc(FD)", page 7f
  http://citeseer.ist.psu.edu/300151.html

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        MaxTime = 30,
        times(Times),
        length(Times, N),
        dependencies(Dependencies),

        % decision variable
        length(Start, N),
        Start :: 0..MaxTime,
        
        % constraints
        foreach([D1,D2] in Dependencies,
                [SD1,SD2,T2],
                (
                    element(D1,Start,SD1),
                    element(D2,Start,SD2),
                    element(D2,Times,T2),
                    SD1 #>= SD2 + T2
                )
        ),
        element(N,Start,SEnd), % to minimize
        SumTimes #= sum(Start),

        % search
        labeling([max,minimize(SEnd)], Start),

        % output
        writeln(sEnd:SEnd),
        writeln(start_times:Start),
        writeln(sum_times:SumTimes),
        nl.


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
             