%
% Mr Greenguest puzzle (fancy dress) in Minizinc
%
% Problem (and LPL) code in
%
% http://diuflx71.unifr.ch/lpl/GetModel?name=/demo/demo2
%
% """
% (** Mr. Greenfan wants to give a dress party where the male guests
%  * must wear green dresses. The following rules are given:
%  * 1 If someone wears a green tie he has to wear a green shirt.
%  * 2 A guest may only wear green socks and a green shirt 
%  *   if he wears a green tie or a green hat.
%  * 3 A guest wearing a green shirt or a green hat or who does
%  *   not wear green socks must wear a green tie.
%  * 4 A guest who is not dressed according to rules 1-3 must
%  *   pay a $11 entrance fee.
%  * Mr Greenguest wants to participate but owns only a green shirt 
%  * (otherwise he would have to pay one for $9). He could buy 
%  * a green tie for $10, a green hat (used) for $2 and green socks
%  * for $12.
%  * What is the cheapest solution for Mr Greenguest to participate?
%  *)
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

var bool: t;
var bool: h;
var bool: r;
var bool: s;
var bool: n;
var int: cost;

constraint
  % This is a straight translation from the LPL code
  ( (t->r) \/ n) /\
  ( ((s \/ r) -> (t \/ h)) \/ n )  /\
  ( ((r \/ h \/ not s) -> t) \/ n ) /\
  % we must translate the booleans to integers...
  cost = 10*bool2int(t) + 2*bool2int(h) + 12*bool2int(s) + 11*bool2int(n)
;

% solve satisfy;
solve minimize cost;


output [
  show(cost), "\n",
 "t:", show(t), " h:", show(h), " r:", show(r), " s:", show(s), " n:", show(n),"\n"
];
