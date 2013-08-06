% 
% Family relation in MiniZinc.
% 
% This is a port of the Prolog family database in
% Marriott & Stuckey, Programming with Constraints, page 167f
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 9;
int: num_father_r = 6;
int: num_mother_r = 2;
set of int: r = 1..n;

r: maggy   = 1;
r: helen   = 2;
r: kitty   = 3;
r: fi      = 4;
r: lillian = 5;
r: jim     = 6;
r: edward  = 7;
r: peter   = 8;
r: bill    = 9;

array[1..num_father_r, 1..2] of r: is_father;
array[1..num_mother_r, 1..2] of r: is_mother;
array[r] of int: age;

var r: X;
var r: Y;


% father
predicate father(var r: f, var r: c) =
  exists(i in 1..num_father_r) (
    is_father[i, 1] = f
    /\ 
    is_father[i,2] = c
  )
;

% mother
predicate mother(var r: m, var r: c) =
  exists(i in 1..num_mother_r) (
     is_mother[i, 1] = m
     /\ 
     is_mother[i,2] = c
  )
;


% parent
predicate parent(var r: p, var r: c) =
   father(p, c) \/ mother(p, c)
;

% sibling
predicate sibling(var r: s1, var r: s2) =
   exists (p in r) (
     parent(p, s1) /\ parent(p, s2)
     /\ 
     s1 != s2
   )
;

% cousin
predicate cousin(var r: c1, var r: c2) =
   exists(p1, p2, p3 in r) (
      parent(p1, c1)
      /\
      parent(p2, c2)
      /\
      sibling(p1, p2)
      /\
      c1 != c2
      /\ 
      p1 != p2
   )
;

% older
predicate older(var r: p1, var r: p2) = 
   age[p1] > age[p2]

;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint

    % give all father relations
    % father(X, Y)

    % who is sibling to Peter?
    % X = peter
    % /\
    % sibling(X, Y)

    % who is cousin to Peter?
    cousin(X, Y) % Y -> fi
    /\ 
    Y = peter

    % all cousin pairs where X is older than Y
    % cousin(X,Y) /\ older(X, Y)

;

% 
% database
% 
is_father = array2d(1..6, 1..2, [
        jim,edward,
        jim,maggy,
        edward,peter,
        edward,helen,
        edward,kitty,
        bill,fi]);

is_mother = array2d(1..2, 1..2, [
        maggy,fi,
        fi,lillian]);


age = [63, 37, 35, 43, 22, 85, 60, 33, 65];


output
[
  "X: " ++ show(X) ++ "\n" ++
  "Y: " ++ show(Y) ++ "\n"
];
