max_vars(6).
max_body(12).
max_clauses(1).


head_pred(won,3).
type(won,(side_type,nat,nat)).
direction(won,(in,in,out)).

body_pred(side,1).
type(side,(side_type,)).
direction(side,(in,)).

% body_pred(red,1).
% type(red,(nat,)).
% direction(red,(in,)).

% body_pred(other,2).
% type(other,(side_type,side_type)).
% direction(supports,(in,out)).

body_pred(move,2).
type(move,(nat,nat)).
direction(move,(in,out)).

body_pred(divisible,2).
type(divisible,(nat,nat)).
direction(divisible,(in,out)).

body_pred(num,1).
type(num,(nat,)).
direction(num,(in,)).


body_pred(num0,1).
type(num0,(nat,)).
direction(num1,(in,)).

body_pred(num1,1).
type(num1,(nat,)).
direction(num1,(in,)).

body_pred(num2,1).
type(num2,(nat,)).
direction(num2,(in,)).

body_pred(num3,1).
type(num3,(nat,)).
direction(num3,(in,)).

body_pred(num4,1).
type(num4,(nat,)).
direction(num4,(in,)).


% body_pred(to_list,4).
% type(to_list,(element,element,element,list)).
% direction(to_list,(in,in,in,out)).