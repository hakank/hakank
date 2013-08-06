% 
% 3-group split in MiniZinc.
% 
% From 
% http://stackoverflow.com/questions/8762230/what-is-an-algorithm-to-split-a-group-of-items-into-3-separate-groups-fairly
% """
% I have this problem in my textbook: Given a group of n items, each with a distinct value 
% V(i), what is the best way to divide the items into 3 groups so the group with the 
% highest value is minimIzed? Give the value of this largest group.
%
% I know how to do the 2 pile variant of this problem: it just requires running the 
% knapsack algorithm backwards on the problem. However, I am pretty puzzled as how to 
% solve this problem. Could anyone give me any pointers?
% 
% Answer: Pretty much the same thing as the 0-1 knapsack, although 2D
% """

% (Via Mathias Brandewinder, Clear Lines blog
% http://www.clear-lines.com/blog/post/Fair-split-into-3-groups-using-Bumblebee.aspx )

% Note: I assume that "so the group with the highest value is minimIzed" means
% that the _sum_ of the elements in this group is minimized.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;
array[1..n] of int: a;
% set of int: s = {a[i] | i in 1..n};
int: k;

% decision variables
% array[1..k] of var set of s: x;
array[1..n] of var 1..k: x;
array[1..k] of var 1..2*(sum(a) div k): sums;
var 1..2*(sum(a) div k): max_sum = max(sums);

% solve satisfy;
% solve minimize max_sum;

solve :: int_search(x, most_constrained, indomain_split, complete) minimize max_sum;
% solve :: int_search(x, first_fail, indomain_min, complete) minimize max_sum;

% solve :: seq_search(
%           [
%            int_search(x, first_fail, indomain_split, complete) ,
%            int_search(sums, first_fail, indomain_min, complete),
%           ])
%       minimize max_sum;


% % Set based approach:
% constraint
%     partition_set(x, s)
%     /\
%     forall(i in 1..k) (
%        sums[i] = sum(b in 1..n) ( bool2int( a[b] in x[i])*a[b]) 
%        /\
%        sums[i] > 0
%     )
%     /\
%     increasing(x)
%     /\
%     alldifferent(a)
% ;

constraint
    forall(i in 1..k) (
       sums[i] = sum(j in 1..n) ( bool2int(x[j] = i)*a[j] ) 
    )

    /\ sum(sums) = sum(a)
    % Very few solvers are better with bin_packing()
    % /\ bin_packing(sum(a), x, a) 
    /\ increasing(sums)
;

output [
  "x: " ++ show(x) ++ "\n" ++
  "sums: " ++ show(sums) ++ "\n" ++
  "max_sum: " ++ show(max_sum) ++ "\n"
]
++
[  if i = 1 then "\n" ++ show(j) ++ "(" ++ show(sums[j]) ++ "): " else "" endif ++
      if fix(x[i] = j) then
        show(a[i]) ++ " "
      else 
        ""
      endif
  | j in 1..k, i in 1..n
]
++ ["\n"]
;


%
% data
%

% This example is from a comment at
% http://stackoverflow.com/questions/8762230/what-is-an-algorithm-to-split-a-group-of-items-into-3-separate-groups-fairly
% n = 7;
% a = [100, 51, 49, 40, 30, 20, 10];
% k = 3;

% n = 101;
% a = [i | i in 1..n];
% k = 3;


% n = 100;
% a = [285,969,598,34,339,711,918,543,301,64,333,910,150,334,462,741,200,92,122,324,235,295,804,415,590,768,599,781,397,789,259,19,358,823,697,396,17,687,47,965,575,217,453,708,257,772,167,927,548,159,400,613,805,27,635,430,485,979,479,521,388,602,24,379,963,827,207,765,721,853,451,403,924,298,498,975,271,353,974,517,836,619,352,959,817,609,411,806,512,145,510,729,387,797,155,580,201,494,109,505];
% k = 3;

% n = 100;
% a = [i | i in 1..n];
% k = 3;


% For n = 1000: max_sum = 166834
% with first_fail/indomain_split:
%   - Chuffed: 1.2s (with bin_packing) (162 conflicts) 
%     first_fail/indomain_split with the parameter --free -toggle_vsids=true
%     without bin_packing: 0.929s 180 conflicts
%   - Gecode/fz: 8.93 (29560 failures)  first_fail/indomain_split
%     without bin_packing: 1.2s 29560 failures
%   - G12/LazyFD (with bin_packing): 5.5s
%   - SICStus: 35400 ms (1470981 backtracks)
%   - fzn2smt: 40.511s
%   - ECLiPSe/fd: Total time 47.460s cpu (0.280 setup + 47.180 search
%   - JaCoP: 58060ms (627871 wrong search decisions)
%     without bin_packing: 5020ms 30953 wrong search decisions
%   - ECLiPSe/ic: Total time 119.270s cpu (0.220 setup + 119.040 search)
%     without bin_packing: 9.251s  (0.230 setup + 8.230 search)      
%   - BProlog: 1:28.64min
%   - G12/fd: 2:44.82min (637551 choice points explored)
%     without bin_packing: 9.297s 31114 choice points explored
%   - fzntini: UNBOUNDED (7.572s)
%   - Choco: >5min
n = 1000;
a = [i | i in 1..n];
k = 3;
