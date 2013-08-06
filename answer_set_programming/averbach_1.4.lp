% 
% Recreational mathematics in ASP. 
%
% Problem 1.4 from 
% Averbach & Chein "Problem Solving Through Recreational Mathematics".
% """
% Messr Baker, Dyer, Farmer, Glover, and Hosier are seated around a circular table,
% playing poker. Each gentleman is the namesake of the profession of one of the others.
%
% The dyer is seated two places to the left of Mr Hosier.
% The baker sits two places to Mr Baker's right.
% The farmer is seated to the left of Mr Farmer.
% Mr Dyer is on the glover's right.
%
% What is the name of the dyer?
% """


%
% domains
%
names(n_baker;n_dyer;n_farmer;n_glover;n_hosier).
professions(p_baker;p_dyer;p_farmer;p_glover;p_hosier).

#const n = 5.
places(0..n-1).

names2(n_baker, "Baker").
names2(n_dyer, "Dyer").
names2(n_farmer, "Farmer").
names2(n_glover, "Glover").
names2(n_hosier, "Hosier").

prof2(p_baker, baker).
prof2(p_dyer, dyer).
prof2(p_farmer, farmer).
prof2(p_glover, glover).
prof2(p_hosier, hosier).


% index and alldifferent
1 { x(N, P, Pos) : professions(P) : places(Pos) } 1 :- names(N). 
1 { x(N, P, Pos) : names(N) : places(Pos) } 1 :- professions(P). 
1 { x(N, P, Pos) : names(N) : professions(P) } 1 :- places(Pos). 

% Each gentleman is the namesake of the profession
% of one of the others.
:- x(n_baker,  p_baker,  _).
:- x(n_dyer,   p_dyer,   _).
:- x(n_farmer, p_farmer, _).
:- x(n_glover, p_glover, _).
:- x(n_hosier, p_hosier, _).

% The dyer is seated two places to the left of Mr Hosier.
% dyer = (Hosier - 2) mod 5
:- x(_, p_dyer, P1), x(n_hosier, _, P2), P1 != (P2 - 2) #mod 5.

% The baker sits two places to Mr Baker's right.
% baker = (Baker + 2) mod 5
:- x(_, p_baker, P1), x(n_baker, _, P2), P1 != (P2 + 2) #mod 5.

% The farmer is seated to the left of Mr Farmer.
% farmer = (Farmer - 1) mod 5
:- x(_, p_farmer, P1), x(n_farmer, _, P2), P1 != (P2 - 1) #mod 5.

% Mr Dyer is on the glover's right.
% Dyer = (glover + 1) mod 5
:- x(n_dyer, _, P1), x(_, p_glover, P2), P1 != (P2 + 1) #mod 5.

% symmetry breaking
% Mr Baker is on place 0
:- x(n_baker,_,P), P != 0.

dyer(Name, Pos) :- x(N, p_dyer, Pos), names2(N,Name), places(Pos).

% sligthly nicer representation of the solution
sol(Name, Prof, Pos) :- 
    x(N,P,Pos),
    names2(N, Name),
    prof2(P,Prof).

#hide.
% #show x(N,P, Pos).
#show dyer(Name, Pos).
#show sol(Name, Prof, Pos).