% 
% Two circles problem in MiniZinc.
% 
% From Chris Smith Math Newsletter #516
% """
% Two circles.
%
% Add their circumferences and you'll get 10322*Pi.
% Add their areas and you'l get 13946281*Pi.
% Can you work out the radius of each circle?
% """

% Reminder:
%  - Area of a circle = Pi*r^2
%  - Circumference = 2*Pi*r

% JaCoP, Gecode:
% r1 = 2020.0;
% r2 = 3141.0;


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

float: pi = 3.141592653589793;

function var float: area(var float: r) = pi*r*r;
function var float: circumference(var float: r) = 2*pi*r;

% decision variables
var 0.0..10000.0: r1;
var 0.0..10000.0: r2;

solve satisfy;

constraint
  r1 <= r2 /\

  circumference(r1) + circumference(r2) = 10322*pi /\
  area(r1) +area(r2) = 13946281*pi
;

% output [

% ];

