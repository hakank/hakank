/*
 
  Recreational mathematics in ECLiPSE.
 
  Problem 1.4 from 
  Averbach & Chein "Problem Solving Through Recreational Mathematics".
  """
  Messr Baker, Dyer, Farmer, Glover, and Hosier are seated around a 
  circular table, playing poker. Each gentleman is the namesake of 
  the profession of one of the others.
 
  The dyer is seated two places to the left of Mr Hosier.
  The baker sits two places to Mr Baker's right.
  The farmer is seated to the left of Mr Farmer.
  Mr Dyer is on the glover's right.
 
  What is the name of the dyer?
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/averbach_1.4.mzn
  * SICStus Prolog: http://www.hakank.org/minizinc/averbach_1.4.pl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
% :-lib(propia).
:-lib(listut).
:-lib(ic_kernel). % for the definition of modulo

:- op(500,xfy,'modulo').

go :-
        findall(_,go2,_).

go2 :-
        % Names
        % Numbering the names and professions as 0..4 
        % so we easily can use modulo 5.
        Names = [Baker,Dyer,Farmer,_Glover,Hosier],
        Names :: 0..4,
        
        % Professions
        Professions = [PBaker,PDyer,PFarmer,PGlover,_PHosier],
        Professions :: 0..4,

        alldifferent(Names),
        alldifferent(Professions),

        % Symmetry breaking
        Baker   #= 1,

        % Each gentleman is the namesake of the profession of one of the others.
        % PBaker  #\= Baker,
        % PDyer   #\= Dyer,
        % PFarmer #\= Farmer,
        % PGlover #\= Glover,
        % PHosier #\= Hosier,

        ( foreach(N,Names),
          foreach(P,Professions) do
              N #\= P
        ),

        % Note: ECLiPSe ic don't directly support modulo so we have 
        % to use some trickery...
        % Later note: I have now implemented a version of modulo 
        % which seems to work (at least in these cases). 
        % I keep the "trickery version" in the comments.

        % The dyer is seated two places to the left of Mr Hosier.
        % Later note: I 
        % C1 :: 0..1,
        % PDyer+C1*5 #= (Hosier - 2), % mod 5,
        PDyer #= (Hosier-2) modulo 5,

        % The baker sits two places to Mr Baker's right.
        % C2 :: 0..1,
        % PBaker+C2*5 #= (Baker + 2), % mod 5,
        PBaker #= (Baker+2) modulo 5,

        % The farmer is seated to the left of Mr Farmer.
        % C3 :: 0..1,
        % PFarmer+C3*5 #= (Farmer - 1), % mod 5,
        PFarmer #= (Farmer-1) modulo 5,

        % Mr Dyer is on the glover's right.
        % C4 :: 0..1,
        % Dyer+C4*5 #= (PGlover + 1), % mod 5,
        Dyer #= (PGlover+1) modulo 5,

        % search
        term_variables([Names,Professions],Vars),
        search(Vars,0, first_fail, indomain_min, complete, [backtrack(Backtracks)]),


        writeln('[Baker, Dyer, Farmer, Glover, Hosier]':Names),
        writeln('[baker, dyer, farmer, glover, hosier]':Professions),
        NamesStr = ['Baker', 'Dyer', 'Farmer', 'Glover', 'Hosier'],
        ProfessionsStr = [baker, dyer, farmer, glover, hosier],
        ( for(I,0,4),
          param(Professions, Names,NamesStr,ProfessionsStr) do
              nth0(I,Professions,ProfI),
              nth0(ProfI,ProfessionsStr,ProfStr),
              nth0(I,Names,NameI),
              nth0(NameI,NamesStr,NameStr),
              write([I,NameStr,ProfStr]),nl
        ),
        writeln(backtracks:Backtracks),
        nl.
        

% This is an implementation of a propagator for modulo.
%
% Heavily inspired by the MiniZinc definition of mod:
% http://www.g12.cs.mu.oz.au/mzn/div_mod/div_mod.mzn    
% Also, see some comments on the MiniZinc Wiki:
% http://www.g12.csse.unimelb.edu.au/wiki/doku.php?id=tests:div_mod:wiki
% 
modulo(X1,Y1,R1) :-
        % These three evals is to be able to use expressions 
        % in the arguments, e.g. (Hosier-2) . 
        % There expressions must be bracketed, though.
        X #= eval(X1),
        Y #= eval(Y1),
        R #= eval(R1),

        Y #\= 0,        
        get_min(X,LBX),
        get_max(X,UBX),
        UBXNeg is -UBX,
        LBXNeg is -LBX,
        min(LBX,UBXNeg,MinX),
        max(UBX,LBXNeg,MaxX),
        D :: MinX..MaxX,
        X #= Y * D + R,
        -abs(Y) #< R, R #< abs(Y),
        MinX #=< D,
        D #=< MaxX.