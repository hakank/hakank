% 
% OneToMany in MiniZinc.
%
% "Constraint programming suitable for extracting OneToMany relationships from records"
% https://stackoverflow.com/questions/56843065/constraint-programming-suitable-for-extracting-onetomany-relationships-from-reco
% """
% I don’t have much experience with constraint programming but I’m trying to figure whether it is suitable for
% a certain kind of problem I am currently facing. I will explain the problem type with an easy fictive example,
% maybe someone has an idea.
% 
% Imagine a table of projects (e.g. school projects where kids do something). Each project has one or more children
% participating. For each child we store its name and the name of its mother. But for each project there is only
% one cell that contains all mothers and one cell that contains all children. Both cells are not necessarily
% ordered in the same way.

% Example:

% +-----------+-----------+------------+
% |           |           |            |
% |   Project |   Parents |   Children |
% |           |           |            |
% +-----------+-----------+------------+
% |           |           |            |
% |   1       |   Jane;   |   Brian;   |
% |           |   Claire  |   Stephen  |
% |           |           |            |
% +-----------+-----------+------------+
% |           |           |            |
% |   2       |   Claire; |   Emma;    |
% |           |   Jane    |   William  |
% |           |           |            |
% +-----------+-----------+------------+
% |           |           |            |
% |   3       |   Jane;   |   William; |
% |           |   Claire  |   James    |
% |           |           |            |
% +-----------+-----------+------------+
% |           |           |            |
% |   4       |   Jane;   |   Brian;   |
% |           |   Sophia; |   James;   |
% |           |   Claire  |   Isabella |
% |           |           |            |
% +-----------+-----------+------------+
% |           |           |            |
% |   4       |   Claire  |   Brian    |
% |           |           |            |
% +-----------+-----------+------------+
% |           |           |            |
% |   5       |   Jane    |   Emma     |
% |           |           |            |
% +-----------+-----------+------------+

% I hope this example visualizes the problem. As I said both cells only contain the names separated by a delimiter,
% but are not necessarily ordered in a similar way. So for technical applications you would transform
% the data into this:

% +-------------+-----------+----------+
% |   Project   |   Name    |   Role   |
% +-------------+-----------+----------+
% |   1         |   Jane    |   Mother |
% +-------------+-----------+----------+
% |   1         |   Claire  |   Mother |
% +-------------+-----------+----------+
% |   1         |   Brian   |   Child  |
% +-------------+-----------+----------+
% |   1         |   Stephen |   Child  |
% +-------------+-----------+----------+
% |   2         |   Jane    |   Mother |
% +-------------+-----------+----------+
% |   2         |   Claire  |   Mother |
% +-------------+-----------+----------+
% |   2         |   Emma    |   Child  |
% +-------------+-----------+----------+
% |   2         |   William |   Child  |
% +-------------+-----------+----------+
% |             |           |          |
% |                                    |
% |              And so on             |

% The number of parents and children is equal for each project, so it is given
% that within the context of a certain project each child belongs to exactly
% one mother. Knowing that it is possible to assign each mother to all of her
% children by logical inference starting with the projects that involve
% only one child (i.e. 4 and 5).

% Results:
%
% Jane has Emma, Stephen and James;
%
% Claire has Brian and William;
%
% Sophia has Isabella
%
% The real dataset I am working with is very large and traversing it with loops,
% that try to capture information about mother-child relations iteratively, is very cumbersome.
% I am wondering if it is possible to solve such a problem with constraint programming.
% Additionally, the real data set might be underdetermined and I am wondering if it is possible
% to isolate records that, when solved manually (i.e. when the mother-child assignments are
% done manually), would break the underdetermination.
% 
% Can someone tell me if I am on the right path?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

enum mothers = {jane,claire,sophia};
enum children = {brian,stephen,emma,william,james,isabella};      

% decision variables

% who is the mother of this child?
array[children] of var mothers: x;

% combine all the combinations of mothers and children in a project
predicate check(array[int] of mothers: mm, array[int] of children: cc) =
let {
  int: n = length(mm);
  array[1..n] of var 1..n: y;
} in
all_different(y) /\
forall(i in 1..n) (
  x[cc[i]] = mm[y[i]]
)
;    

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

%% explicit enumeration of the combinations
% constraint
% % project 1
% (
%   ( x[brian] = jane /\ x[stephen] = claire) \/
%   ( x[stephen] = jane /\ x[brian] = claire)
% ) 
% /\
% % project 2
% (
%   ( x[emma] = claire /\ x[william] = jane) \/
%   ( x[william] = claire /\ x[emma] = jane) 
% )

% /\
% % project 3
% (
%   ( x[william] = claire /\ x[james] = jane) \/
%   ( x[james] = claire /\ x[william] = jane) 
% )
% /\
% % project 4
% ( % [isabella,brian,james],[isabella,james,brian]]
%     ( x[brian] = jane /\ x[james] = sophia /\ x[isabella] = claire) \/
%     ( x[james] = jane /\ x[brian] = sophia /\ x[isabella] = claire) \/
%     ( x[james] = jane /\ x[isabella] = sophia /\ x[brian] = claire) \/
%     ( x[brian] = jane /\ x[isabella] = sophia /\ x[james] = claire) \/
%     ( x[isabella] = jane /\ x[brian] = sophia /\ x[james] = claire) \/
%     ( x[isabella] = jane /\ x[james] = sophia /\ x[brian] = claire) 
%  )
% /\

% % project 4(sic!)
% ( x[brian] = claire) /\

% % project 5
% ( x[emma] = jane)
% ;

constraint
% All mothers has at least one child
forall(m in mothers) (
  exists(c in children) (
    x[c] = m
  )
)
;


constraint
% project 1    
check([jane,claire], [brian,stephen]) /\

% project 2
check([claire,jane],[emma,william]) /\

% project 3
check([claire,jane],[william,james]) /\

% project 4
check([claire,sophia,jane],[brian,james,isabella]) /\

% project 4(sic!)
check([claire],[brian]) /\

% project 5
check([jane],[emma])

;

output [
 "\(c): \(x[c])\n"
  | c in children
];

