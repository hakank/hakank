max_vars(4).
max_body(4).
max_clauses(2).

enable_pi.
% enable_recursion.

% head
head_pred(leapyear,1).
type(leapyear,(int,)).
direction(leapyear,(in,)).

% body
% body_pred(leapyear,1).
% type(leapyear,(int,)).
% direction(leapyear,(in,)).

%% Ah, known_divider is used in bk.pl 
body_pred(known_divider,1).
type(known_divider,(int,)).
direction(known_divider,(out,)).

body_pred(divisible,2).
type(divisible,(int,int)).
direction(divisible,(in,out)).

body_pred(not_divisible,2).
type(not_divisible,(int,int)).
direction(not_divisible,(in,out)).

%% hard code constants

% body_pred(known_divider1,1).
% type(known_divider1,(int,)).
% direction(known_divider1,(in,)).

% body_pred(known_divider2,1).
% type(known_divider2,(int,)).
% direction(known_divider2,(in,)).

body_pred(known_divider4,1).
type(known_divider4,(int,)).
direction(known_divider4,(in,)).

% body_pred(known_divider5,1).
% type(known_divider5,(int,)).
% direction(known_divider5,(in,)).

% body_pred(known_divider8,1).
% type(known_divider8,(int,)).
% direction(known_divider8,(in,)).

% body_pred(known_divider10,1).
% type(known_divider10,(int,)).
% direction(known_divider10,(in,)).

% body_pred(known_divider16,1).
% type(known_divider16,(int,)).
% direction(known_divider16,(in,)).

% body_pred(known_divider20,1).
% type(known_divider20,(int,)).
% direction(known_divider20,(in,)).

% body_pred(known_divider25,1).
% type(known_divider25,(int,)).
% direction(known_divider25,(in,)).

% body_pred(known_divider40,1).
% type(known_divider40,(int,)).
% direction(known_divider40,(in,)).

% body_pred(known_divider50,1).
% type(known_divider50,(int,)).
% direction(known_divider50,(in,)).

% body_pred(known_divider80,1).
% type(known_divider80,(int,)).
% direction(known_divider80,(in,)).

body_pred(known_divider100,1).
type(known_divider100,(int,)).
direction(known_divider100,(in,)).

% body_pred(known_divider200,1).
% type(known_divider200,(int,)).
% direction(known_divider200,(in,)).

body_pred(known_divider400,1).
type(known_divider400,(int,)).
direction(known_divider400,(in,)).

