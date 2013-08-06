%
% SEND+MORE=MONEY problem in ASP.
%
% x(e,5) x(n,6) x(d,7) x(m,1) x(o,0) x(r,8) x(y,2) x(s,9)
% 
% 
% x(d,7) 
% x(e,5) 
% x(m,1) 
% x(n,6) 
% x(o,0) 
% x(r,8) 
% x(s,9)
% x(y,2) 
%
% This version tests two improvements:
%   - use #domain for all values/1. 
%     Does it do any difference in grounder/solver time?
%     No!
%   - Setting M = 1
%     Ah, now gringo/clasp solves it in 5s instead of 45s.
%     And lparse/smodel in 2.5s
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

letter(s;e;n;d;m;o;r;y).
values(0..9).

#domain values(S).
#domain values(E).
#domain values(N).
#domain values(D).
#domain values(M).
#domain values(O).
#domain values(R).
#domain values(Y).

% #domain values(Val).
% #domain letter(L).


% exact 1 occurrence of each letter
1 { x(L,Val) : values(Val) } 1 :- letter(L).

% 0..1 occurrences of each value
{ x(L,Val) : letter(L) } 1 :- values(Val).


% no digit can be given to two different symbols
:- letter(L), letter(L1), values(V1), x(L,V1), x(L1,V1), L != L1.


smm :- 
    % values(S;E;N;D;M;O;R;Y),
    x(s,S), x(e,E), x(n,N), x(d,D),
    x(m,M), x(o,O), x(r,R), x(y,Y), 
    % M > 0, 
    % hard coding M to 1 -> gringo/clasp solves in 5 seconds
    M = 1,
    S > 0,
    S*1000+E*100+N*10+D + M*1000+O*100+R*10+E == M*10000+O*1000+N*100+E*10+Y.

:- not smm.

#hide.
#show x(L,Val).

