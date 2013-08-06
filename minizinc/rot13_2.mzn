%
% Rot 13 (Caesar cipher) in Minizinc.
%
% In this version rot is as a variable as well.
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: len = 10;       % string length
int: alpha_len = 26; % length of alphabet

array[1..len] of 1..alpha_len: text;
array[1..len] of var 1..alpha_len: rotated; % the rotated string
var 1..26: rot;       % the rotate number

solve satisfy;

constraint
  forall(i in 1..len) (
    let {
      var 0..alpha_len: m = (text[i] + rot) mod alpha_len
     } 
     in 
       (m = 0 -> rotated[i] = alpha_len)
       /\
       (m > 0 -> rotated[i] = m)
  )
;

%
% data
%
text = [1,2,3,13,14,15,16,24,25,26];

output [
  "text: " ++ show(text) ++ "\n" ++
  "rotated: " ++ show(rotated) ++ "\n" ++
  "rot: " ++ show(rot) ++ "\n"
];