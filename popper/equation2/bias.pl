% 
%   Cf http://hakank.org/jgap/equation.conf
%   Solve this equation
%   11x11=4 
%   22x22=16 
%   33x33=?
%
%   Later:
%   MindYourDecision (Presh Talwalkar) has blogged/youtubed about it:
%    "Viral Puzzle 11 x 11 = 4. The Correct Answer Explained" 
%   - https://mindyourdecisions.com/blog/2016/09/21/viral-puzzle-11x11-4-the-correct-answer-explained/
%   - https://www.youtube.com/watch?v=IQd1oDsHVSc

%   My Symbolic Regression program finds a lot of different answers but it seems that it
%   always tends towards the solution of 36. E.g.
%    (b + b) * (b + b) [4021]
%    (d + b) * (b + b) [1449]
%    (b + b) * (d + b) [1394]
%    (b + b) * (b + d) [1269]
%    (b + d) * (b + b) [1173]
%    (b + d) * (b + d) [1098]
%    (b + b) * (c + a) [981]
%
%%
%% First test: only plus and mult, after 17s this is found
%%    ********** SOLUTION **********
%%    Precision:1.00 Recall:1.00 TP:2 FN:0 TN:1 FP:0 Size:4
%%    f(A,B,C,D,E):- plus(C,D,G),plus(C,D,F),mult(G,F,E).
%%    ******************************
%% which gives f(3,3,3,3,36)

%% Second test: add pow and const1..4
%% and add this in exs.pl
%%   neg(f(3,3,3,3,36)).
%%
%% After 4s this solution is found
%% ********** SOLUTION **********
%% Precision:1.00 Recall:1.00 TP:2 FN:0 TN:2 FP:0 Size:3
%% f(A,B,C,D,E):- const4(F),pow(F,A,E).
%% ******************************
%%


max_vars(8).
max_body(8).
% max_clauses(4).

% enable_pi.
% enable_recursion.
allow_singletons.
non_datalog.

head_pred(f,5).

body_pred(plus,3).
% body_pred(minus,3).
body_pred(mult,3).
% body_pred(div,3).
body_pred(pow,3).

body_pred(const1,1).
body_pred(const2,1).
body_pred(const3,1).
body_pred(const4,1).

direction(f,(in,in,in,in,out)).
direction(plus,(in,in,out)).
% direction(minus,(in,in,out)).
direction(mult,(in,in,out)).
% direction(div,(in,in,out)).
direction(pow,(in,in,out)).

direction(const1,(out,)).
direction(const2,(out,)).
direction(const3,(out,)).
direction(const4,(out,)).


% functional(plus,3).
% functional(mult,3).

% irreflexive(plus,2).
% irreflexive(mult,2).