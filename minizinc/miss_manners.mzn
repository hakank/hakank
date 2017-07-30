% 
% Miss Manners seating problem in MiniZinc.
% 
% From http://4c110.ucc.ie/cpstandards/index.php/en/standards/java/examples/27
% """
% The "Miss Manners" problem is a notorious benchmark for rule engines. 
% The problem is to find an acceptable seating arrangement for guests at 
% a dinner party.  It should match people with the same hobbies (at 
% least one), and to seat everyone next to a member of the opposite sex. 
% """

% The data is presented in the Excel file: 
% http://4c110.ucc.ie/cpstandards/files/Manners.xls
% 
% The MiniZinc versions:
%  http://www.hakank.org/minizinc/miss_manners_16.dzn
%  http://www.hakank.org/minizinc/miss_manners_64.dzn
%  http://www.hakank.org/minizinc/miss_manners_128.dzn
%

%
% Also, see 
%  - http://docs.codehaus.org/display/DROOLS/Miss+Manners+Example
%  - http://blog.athico.com/2009/05/miss-manners-2009-yet-another-drools.html
%  - http://it.toolbox.com/blogs/thinking-out-loud/industry-analysts-and-rules-engines-2349
%    Refers to OPS5 benchmark suite: 
%    ftp://ftp.cs.utexas.edu/pub/ops5-benchmark-suite/
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


%
% Note: I assume a circular table placement here.
%
% Also, a more interesting (and demanding) problem should be to maximize 
% the number of common hobbies of the neighbours.
% (On the other hand, not sharing all hobbies with one's neighbours could 
% give raise to new interests an more animated discussions.)
%

include "all_different.mzn"; 
int: n;
int: M = 1; % male
int: F = 2; % female
array[1..n] of M..F: gender;
int: num_hobbies; % maximum number of hobbies
array[1..n] of set of 1..num_hobbies: hobbies;

% decision variable: Who will sit where?
array[1..n] of var 1..n: seating;

% total number of common hobbies of the two neighbours 
% (to be maximized)
var 0..(n+1)*num_hobbies: common_hobbies = 
            sum(i in 2..n) (
                  card(hobbies[seating[i]] intersect hobbies[seating[i-1]])
                  ) 
            + card(hobbies[seating[1]] intersect hobbies[seating[n]]);


% solve satisfy;
solve :: int_search(
        seating,
        most_constrained, % first_fail, % largest, 
        indomain_split, % indomain_max,
        complete) 
    maximize common_hobbies;
    % satisfy;

constraint
  all_different(seating) % :: domain

  % /\ % symmetry breaking
  % seating[1] = n
;

%
% mix genders
%
constraint
  forall(i in 2..n) (
       gender[seating[i]] != gender[seating[i-1]]
  )
  /\
  gender[seating[1]] != gender[seating[n]]
;


predicate match_hobbies(var int: a, var int: b) =
   card(hobbies[a] intersect hobbies[b]) >= 1
;

%
% match hobbies
%
constraint
  forall(i in 2..n) (
     match_hobbies(seating[i], seating[i-1])
  )
  /\
  match_hobbies(seating[n], seating[1])
;

output 
[show(seating) ++ "\n"] ++
[ "common_hobbies: ", show(common_hobbies), "\n" ] ++
[
   show(seating[i]) ++ ": " ++ show(gender[seating[i]]) ++ " " ++ show(hobbies[seating[i]]) ++ "\n"
  | i in 1..n
] 
++ 
[ "common_hobbies: ", show(common_hobbies), "\n" ] 
++
["\n"]
;