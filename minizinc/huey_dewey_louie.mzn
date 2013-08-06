% 
% Huey, Dewey and Louie problem in MiniZinc.
% 
% From Marriott & Stucket, Programming with Constraints, page 42
% """
% Huey, Dewey and Louie are being questioned by their uncle. These are the 
% statements the make:
%  Huey: Dewey and Louie has equal share in it; if one is quitly, so is the other.
%  Dewey: If Huey is guilty, then so am I.
%  Louie: Dewey and I are not both quilty.
% 
% Their uncle, knowing that they are cub scouts, realises that they cannot tell a lie.
% Has he got sufficient information to decide who (if any) are quilty?
% """
% 

%
% Solution:
%   dewey = false
%   huey = false
%   louie = false
% i.e. no one is guitly.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

var bool: huey;
var bool: dewey;
var bool: louie;



solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   %  Huey: Dewey and Louie has equal share in it; if one is quitly, so is the other.
   (dewey <-> louie)
  
   %  Dewey: If Huey is guilty, then so am I.
   /\
   (huey -> dewey)

   %  Louie: Dewey and I are not both quilty.
   /\
   (not (dewey /\ louie))

    
;

output
[
  show([huey, dewey, louie])
];
