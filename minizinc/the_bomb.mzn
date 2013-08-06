% 
% The Bomb puzzle in MiniZinc.
% 
% From http://blog.kutterer.at/?p=7
% """
% The bomb has 7 flip switches. To disarm the bomb you must put the 
% switches in their correct position. And you got this inforamtions: 
% the bomb detonates when:
%
%   1. if switch 3 is on plus 2 and 4 are off, boom!
%   2. if 1 and 4 off plus 7 is on, boom!
%   3. if 1, 3 and 4 are off, boom!
%   4. if 6 is off plus 2 and 3 are on, boom!
%   5. if 4 and 3 are on, boom!
%   6. if 6 is on and if, as long as 7 is on, 1 is on too, boom!
%   7. if 6 is on and, provided 7 is on, 1 is on too, boom!
%   8. if 1 and 5 are on plus 7 is off, boom!
%   9. if 3 is off plus 4 and 5 are on, boom!
%  10. if 1 and 7 are on, boom!
%  11. if 5 off and if, as long as 2 and 6 are on, 3 is on too, boom!
%  12. if 7 off plus 3 or 4 on, boom!
%  13. if switch 6 and 7 are in different positions, boom!
%  14. if switch 2, 3 and 5 are off, boom!
%  15. if switch 1 and 2 are on and switch 5 and 7 are off, boom!
%  16. if 6 and 7 are off, boom!
% """
% 
% Originally from the German puzzle Die Bombe by Angela and Otto Janko
% http://www.janko.at/Raetsel/Logik/012.a.htm
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


%
% This model use the English version, and gives the same result as the 
% Prolog code from the English blog.
%


% include "globals.mzn"; 
int: n = 7;
array[1..n] of var bool: x; % the switches

solve satisfy;
% solve :: bool_search(x, input_order, indomain_min, complete) satisfy;

constraint

  not(

    %   1. if switch 3 is on plus 2 and 4 are off, boom!
    (x[3] /\ x[2] = false /\ x[4] = false)

    %   2. if 1 and 4 off plus 7 is on, boom!
    \/ (x[1] = false /\ x[4] = false /\ x[7])

    %   3. if 1, 3 and 4 are off, boom!
    \/ (x[1] = false /\  x[3] = false /\ x[4] = false)

    %   4. if 6 is off plus 2 and 3 are on, boom!
    \/ (x[6] = false /\ x[2] /\ x[3])

    %   5. if 4 and 3 are on, boom!
    \/ (x[4] /\ x[3])

    %   6. if 6 is on and if, as long as 7 is on, 1 is on too, boom!
    \/  (x[6] /\ x[7] /\ x[1])

    %   7. if 6 is on and, provided 7 is on, 1 is on too, boom!
    \/  (x[6] /\ x[7] /\ x[1])

    %   8. if 1 and 5 are on plus 7 is off, boom!
    \/  (x[1] /\ x[5] /\ x[7] = false)

    %   9. if 3 is off plus 4 and 5 are on, boom!
    \/  (x[3] = false /\ x[4] /\ x[5]) 

    %  10  if 1 and 7 are on, boom! 
    \/  (x[1] /\ x[7]) 

    %  11. if 5 off and if, as long as 2 and 6 are on, 3 is on too, boom!
    \/  (x[5] = false /\ x[2] /\ x[6] /\ x[3]) 

    %  12. if 7 off plus 3 or 4 on, boom!
    \/  (x[7] = false /\ (x[3] \/ x[4]))

    %  13. if switch 6 and 7 are in different positions, boom!
    \/  (x[6] != x[7])

    %  14. if switch 2, 3 and 5 are off, boom!
    \/  (x[2] = false /\ x[3] = false /\ x[5] = false)

    %  15. if switch 1 and 2 are on and switch 5 and 7 are off, boom!
    \/ (x[1] /\ x[2] /\ x[5] = false /\ x[7] = false) 

    %  16. if 6 and 7 are off, boom!
    \/ (x[6] = false /\ x[7] = false)

  )

;

output [show(x)];