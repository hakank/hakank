% https://open.kattis.com/problems/countingclauses
% 1s
% 1.6 Easy

% I first tried to implement a 3-SAT solver (in counting_clauses_back.pl)
% But the problem is actually much simpler than that:
%  - Number of clauses >= 8: sat
%  - Number of clauses < 8: unsat

main :- read_line_to_string(user_input,NV),split_string(NV," ","",[N0,_]),
number_string(N,N0),(N>=8->T=satisfactory;T=unsatisfactory),writeln(T).
