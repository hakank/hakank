% 
% in MiniZinc.
% 
% https://mindyourdecisions.com/blog/2017/12/20/can-you-solve-the-rectangle-area-puzzle/
% """
% A large rectangle is divided into four rectangles, three of which have areas 16, 13, and 39 as 
% drawn in the following figure:
%    
%  [
%            x        len
%       --------------
%       | 13 |  39   | 
%       --------------  y
%       | 16 |   ?   |      
%       -------------- height
%   
%  ]

% What is the area of the fourth rectangle
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

var 1..100: area1;
var 1..100: area2;
var 1..100: area3;
var 1..100: area4;

var 1..100: len;
var 1..100: height;
var 1..100: x;
var 1..100: y;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
     area1 = 13 /\  area2 = 39 /\ area3 = 16 /\
     x*y = area1 /\
     (len-x)*y = area2 /\
     (height-y)*x = area3 /\
     (height-y)*(len-x) = area4 /\
     (len*height) = area1+area2+area3+area4
;

% output [ "area4:\(area4)\n"];

