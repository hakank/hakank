% 
% Bus driver scheduling (CSPLib problem 22) in ASP.
% 
% From 
% http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob022/spec.html
% """
% Specification
% Bus driver scheduling can be formulated as a set paritioning problem. 
% We propose 12 set partitioning problems derived from small bus driver 
% scheduling problems. These consist of a given set of tasks (pieces of 
% work) to cover and a large set of possible shifts, where each shift 
% covers a subset of the tasks and has an associated cost. We must select 
% a subset of possible shifts that covers each piece of work once and 
% only once: this is called a partition. Further,
% 
% In the driver scheduling (unlike air crew scheduling) the main aim is 
% to reduce the number of shifts used in the solution partition and the 
% total cost of the partition is secondary. To simplify the problem we have 
% made the cost of each shift the same. This means that the goal is to 
% minimise the number of shifts.
% 
% The problems come from four different bus companies: 
% Reading (r1 to r5a), 
% CentreWest Ealing area (c1, c1a, c2), 
% the former London Transport (t1 and t2). 
%
% The problems have differing regulations and features (e.g. urban and 
% short distance rural bus schedules can have very different features). Note 
% that r1 and r1a are the same problem, but have different numbers of 
% generated shifts. Similarly with the problems: c1, c1a and r5, r5a. 
% 
% Problems are presented in the same format as the set partitioning 
% examples in ORLIB. The first line gives the number of rows (pieces of work), 
% columns (shifts) and the minimum number of columns need for a partition. 
% Then each line after that corresponds to one column. It starts with 
% the cost (which is always 1 in our case) then the number of rows it 
% covers, followed by the rows it covers. 
% """
%
% Notes:
%  - this is a set partition problem
%  - This program skips the cost parameter.
% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

% domains
% works(0..num_work-1).
% shifts(1..num_shifts).
works(Work) :- shift(Shift, Work).
shifts(Shift) :- shift(Shift, Work).

% x: selected shift
% we must have at least min_num_shifts selected
min_num_shifts { x(Shift) : shifts(Shift) }.

% all works must be selected exactly once
1 { x(Shift) : shift(Shift, Work) } 1 :- works(Work).

% minimize total number of shifts
#minimize {x(Shift) : shifts(Shift)}.


#hide.
#show x(Shift).

