% 
% Scheduling problem (yet another) in MiniZinc.
% 
% From Dennis E. Sasha "Puzzles for Programmers and Pros",
% page 131f
% """
% In which order do you schedule the tasks starting from current
% day 0?
% Task  T1 takes 4 days with deadline on day 45
% Task  T2 takes 4 days with deadline on day 48
% Task  T3 takes 5 days with deadline on day 25
% Task  T4 takes 2 days with deadline on day 49
% Task  T5 takes 5 days with deadline on day 36
% Task  T6 takes 2 days with deadline on day 31
% Task  T7 takes 7 days with deadline on day 9
% Task  T8 takes 5 days with deadline on day 39
% Task  T9 takes 4 days with deadline on day 13
% Task T10 takes 6 days with deadline on day 17
% Task T11 takes 4 days with deadline on day 29
% Task T12 takes 1 days with deadline on day 19
% """
% 
% The answer (according to page 132) is:
%
% T7 T9 T10 T12 T3 T11 T6 T5 T8 T1 T2 T4
%               ---------
% 
% This MiniZinc model gives the following start_time for 
% last_time = 49 (the minimum last_time) using solve satisfy:
% 
% The only differences between these solutions are where to place 
% the tasks T3, T6, and T11, (i.e. the place 5,6,7)
%
% index           1   2   3   4   5   6   7   8   9  10  11  12
% durations       4   4   5   2   5   2   7   5,  4   6   4   1
%
% position       10  11   5  12   8   6   1   9   2   3   7   4
% start_times = [39, 43, 18, 47, 29, 23,  0, 34,  7, 11, 25, 17]
% order: 7,9,10,12,3,6,11,5,8,1,2,4
%                  ------
%
% position       10  11   6  12   8   5   1   9   2   3   7   4
% start_times = [39, 43, 20, 47, 29, 18,  0, 34,  7, 11, 25, 17]
% order: 7,9,10,12,6,3,11,5,8,1,2,4
%                  ------
%
% position       10  11   5  12   8   7   1   9   2   3   6   4
% start_times = [39, 43, 18, 47, 29, 27,  0, 34,  7, 11, 23, 17]
% order: 7,9,10,12,3,11,6,5,8,1,2,4
%                  -------


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 12;
array[1..n] of int: duration;
array[1..n] of int: deadline;
int: max_deadline = max(deadline);

array[1..n] of var 0..max_deadline: start_times;
array[1..n] of var 0..max_deadline: end_times;
var 0..max_deadline: last_time;
% array[1..n] of var  0..1: used; % for disjunctive

%
% disjunctive predicate
% See http://www.hakank.org/minizinc/disjunctive.mzn
%
predicate disjunctive(array[int] of var int: v_origin, 
                      array[int] of var int: v_duration, 
                      array[int] of var int: v_end_time, 
                      array[int] of var int: v_used 
                      ) =
  assert(
        length(v_origin) == length(v_duration) /\
        length(v_duration) == length(v_end_time) /\
        length(v_end_time) == length(v_used),
        "All arrays must be of same length!")
  /\
  forall(i in index_set(v_origin)) (
     ( 
       v_duration[i] > 0 <-> (
          v_used[i] = 1 
          /\
          v_end_time[i] = v_origin[i] + v_duration[i]
          /\
          forall(j in index_set(v_origin) where i < j) (
            v_end_time[i] <= v_origin[j] % i left of j
            \/
            v_origin[i] >= v_end_time[j] % i right of j
          )     
       )
     )
     /\ % zero duration -> not used
     ( 
      v_duration[i] = 0 <-> (v_used[i] = 0 /\ v_end_time[i] = 0)

     )        
  )
;


predicate disjunctive2(array[int] of var int: start_times,
                       array[int] of var int: end_times) =
     forall(i,j in index_set(start_times) where i != j) (
         end_times[i] <= start_times[j] \/
         start_times[i] >= end_times[j] 
     )
;

solve minimize last_time;
% solve satisfy;


constraint
   maximum(last_time,end_times) /\
   forall(i in 1..n) (
      end_times[i] = start_times[i] + duration[i] /\
      end_times[i] <= deadline[i]
   )  

   /\
   % disjunctive(start_times,duration,end_times,used)
   disjunctive2(start_times,end_times)
   
   % /\ last_time = 49 % for solve satisfy

;

duration = [ 4, 4, 5, 2, 5, 2, 7, 5, 4, 6, 4, 1];
deadline = [45,48,25,49,36,31, 9,39,13,17,29,19];


output 
[
  "start_times: " ++ show(start_times) ++ "\n" ++
  "duration   : " ++ show(duration)    ++ "\n" ++
  "end_times  : " ++ show(end_times)   ++ "\n" ++
  "deadline   : " ++ show(deadline)    ++ "\n"
]
++
[
 "task" ++ show(t) ++ ": " ++ 
  show(start_times[t]) ++ " -> (" ++ show(duration[t]) ++ ") -> " ++ 
  show(end_times[t]) ++ " (deadline: " ++ show(deadline[t]) ++ ")\n"

 | t in 1..n
] 
++ [ "\nTimeline:\n" ] ++
[
  if task = 1 then "Time " ++ show(time) ++ 
      if time < 10 then " " else "" endif ++
      ": task " else "" endif ++
  
  if time >= fix(start_times[task]) /\ time < fix(end_times[task]) then
      show(task) ++ "\n"
  else 
     ""
  endif
  | time in 0..max_deadline, task in 1..n
]
;