% Test of using clpfd 

% all_different (finds all_distinct which is ok)
% pos(target([1,2,3,4])).
% pos(target([3,2,1])).
% pos(target([4,2,1,3])).

% neg(target([2,2,1])).
% neg(target([2,2,2,1])).


% increasing OK
% pos(target([1,2,3,4])).
% pos(target([1,2,3])).
% pos(target([3,4,5,6,10])).

% neg(target([2,2,1])).
% neg(target([4,3,2,1])).


% alldifferent_except_0 OK
% pos(target([1,2,3,4,0,0])).
% pos(target([3,2,1,0])).
% pos(target([0,4,2,1,3])).

% neg(target([2,2,1,0])).
% neg(target([2,2,2,1])).


% alldifferent_modulo.  OK
% pos(target([1,2,3,4],5)).
% pos(target([18,16,10,12],5)).
% pos(target([18,16,14,12],5)).
% pos(target([9,16,13,12],5)).
% pos(target([9,16,25,12],5)).
% pos(target([18,13,11,12],4))
% pos(target([22,13,11,12],4))
% neg(target([9,16,25,12],2)).
% neg(target([9,16,25,12],3)).


% circuit OK
% pos(target([2,3,4,5,1])).
% pos(target([2,3,5,1,4])).
% pos(target([2,4,5,3,1])).
% pos(target([2,4,1,5,3])).
% pos(target([2,5,4,1,3])).
% neg(target([1,2,3,4,5])).

% min_list_clp (i.e. minimum) OK
% pos(target([1,2,3,4,5],1)).
% pos(target([2,1,2,3,4,5],1)).
% pos(target([4],4)).

% neg(target([2,1,2,3,4,5],5)).
% neg(target([2,1,2,3,4,5],2)).
% neg(target([4],1)).


% max_list_clp (maximum) OK
% pos(target([1,2,3,4,5],5)).
% pos(target([2,5,2,3,4,1],5)).
% pos(target([4],4)).

% neg(target([2,1,2,3,4,5],2)).
% neg(target([5,1,2,3,4,5],2)).
% neg(target([4],9)).



% to_num OK
% pos(target([1,2,3],123)).
% pos(target([8,4],84)).
% neg(target([8,4],48)).


% inverse OK
% pos(target([1,2,3,4,5,6])).
% pos(target([1,2,3,4,6,5])).
% pos(target([1,2,3,5,4,6])).
% pos(target([1,2,3,6,5,4])).

% neg(target([1,1,2])).
% neg(target([2,3,4,5,6,1])).


%% two lists: one alldifferent (all_distinct) and one increasing  OK! (Didn't work in old Popper)
% pos(target([1,3,4,2],[1,2,3,4])).
% pos(target([4,3,7,1],[1,3,4,7])).

% neg(target([4,4,7,1],[1,3,4,7])).
% neg(target([4,3,7,1],[3,1,4,7])).

%% atmost(N,L,V)
%% The value V in list L can have at most N occurrences.
%% This don't work 
% pos(target(2,[1,1,2,3,4,5],1)).
% pos(target(2,[1,2,1,2,3,2],1)).
% pos(target(3,[1,2,1,2,3,2],1)).
% pos(target(1,[1,2,6,2,2,3],6)).
% neg(target(1,[1,2,1,2,3,2],1)).
% neg(target(2,[1,2,6,2,2,3],6)).

%% exactly(N,L,V)  OK
%% The value V in list L have exactly N occurrences.
pos(target(2,[1,1,2,3,4,5],1)).
pos(target(2,[1,2,1,2,3,2],1)).
pos(target(1,[1,2,6,2,2,3],6)).

neg(target(1,[1,2,1,2,3,2],1)).
neg(target(2,[1,2,6,2,2,3],6)).




%% Sudoku/Latin Square! Nope. How do I encode this in the modes file?
%% type(...,list_of_lists) don't work...
%% I would like to induce maplist(list,all_different) but don't know
%% how to type maplist/n
%% If target is defined as a matrix this is found directly:
%% target(A) :- function_all_different(B),maplist3(B,A).
%%
% pos(target([[1,2,3,4],[3,4,1,2],[2,1,4,3],[4,3,2,1]])).
% pos(target([[1,2,3,4],[3,4,1,2],[2,3,4,1],[4,1,2,3]])).
% neg(target([[1,1,3,4],[3,4,1,2],[2,3,4,1],[4,1,2,3]])).
% neg(target([[1,2,3,4],[3,3,1,2],[2,3,4,4],[4,2,2,3]])).


% This works: It's now up to the system to realize it's a Sudoku
% or at least a Latin Square OK
% pos(target([1,2,3,4, 3,4,1,2, 2,1,4,3, 4,3,2,1])).
% pos(target([1,2,3,4, 3,4,1,2, 2,3,4,1, 4,1,2,3])).

% neg(target([1,1,3,4, 3,4,1,2, 2,3,4,1, 4,1,2,3])).
% neg(target([1,2,3,4, 3,3,1,2, 2,3,4,4, 4,2,2,3])).


% element(Ix,L,Val): OK! (Didn't work in old Popper)
% pos(target(1,[1,2,3,4],1)).
% pos(target(2,[1,2,3,4],2)).
% pos(target(3,[4,3,2,1],2)).

% neg(target(1,[4,3,2,1],1)).   


% scalar_product2(X,Y,ScalarProduct) OK
% pos(target([1,2,3],[2,3,4],20)).
% pos(target([3,2,6],[3,6,3],39)).
% neg(target([1,2,3],[1,2,3],1)).


% Matrix of increasing lists OK
% Program:
% target(A) :- function_increasing(B),maplist3(B,A).
% 
% pos(target([[1,2,3,4],[1,1,5,7],[3,4,4,6],[1,1,1,1]])).


% increasing_with 
% This is not what I expected:
% target(A,B,C) :- scalar_product2(F,C,E),min_list_clp(F,D),exactly(D,F,B),max_list_clp(A,E).
% pos(target([1,2,3,4],1,[2,3,4,5])).
% pos(target([1,2,3,4],2,[3,4,5,6])).
% neg(target([1,2,3,4],1,[3,4,5,6])).
   