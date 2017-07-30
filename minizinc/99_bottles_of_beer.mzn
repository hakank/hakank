% 
% 99 bottles of beer in MiniZinc.
% 
% From
% http://www.rosettacode.org/wiki/99_Bottles_of_Beer
% """
% In this puzzle, write code to print out the entire "99 bottles of beer on the wall" song. 
%  For those who do not know the song, the lyrics follow this form:
%
% X bottles of beer on the wall
% X bottles of beer
% Take one down, pass it around
% X-1 bottles of beer on the wall
%
% X-1 bottles of beer on the wall
% ...
% Take one down, pass it around
% 0 bottles of beer on the wall
%
% Where X and X-1 are replaced by numbers of course. Grammatical support for "1 bottle of 
% beer" is optional. As with any puzzle, try to do it in as creative/concise/comical 
% a way as possible (simple, obvious solutions allowed, too). 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

solve satisfy;

int: n = 100;

constraint 1=1;

output [
   show(n-i-1) ++ " bottles of beer on the wall\n" ++
   if i < n-1 then
     show(n-i-1) ++ " bootles of beer\n" ++ "Take one down, pass it around\n"
   else "" endif
  | i in 0..n-1
];
