%
% Rot 13 (Caesar cipher) in Minizinc.
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: rot = 13;       % the rotate number
int: len = 10;       % string length
int: alpha_len = 26; % length of alphabet

array[1..len] of 1..alpha_len: text;
array[1..len] of var 1..alpha_len: rotated; % the rotated string

solve satisfy;

constraint
  forall(i in 1..len) (
    % to simplify the assignment to rotated[i]
    let {
      0..alpha_len: m = (text[i] + rot) mod alpha_len
     } 
     in 
     rotated[i] = if m = 0 then alpha_len else m endif
  )
;

%
% data
%
text = [1,2,3,13,14,15,16,24,25,26];

output [show(rotated)];