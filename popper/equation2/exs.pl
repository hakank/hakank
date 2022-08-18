%% Equation
% 11x11=4 
% 22x22=16 
% 33x33=?

pos(f(1,1,1,1,4)).
pos(f(2,2,2,2,16)).
% pos(f(3,3,3,3,X)).

neg(f(1,1,1,1,2)).

neg(f(3,3,3,3,36)). % Test: forbid the first solution

