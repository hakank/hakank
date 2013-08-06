/*
 
  Recreational mathematics in SICStus Prolog.
 
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


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        findall(_,go2,_).

go2 :-
        % Names
        % Numbering the names and professions as 0..4 
        % so we easily can use modulo 5.
        Names = [Baker,Dyer,Farmer,Glover,Hosier],
        domain(Names,0,4),
        
        % Professions
        Professions = [PBaker,PDyer,PFarmer,PGlover,PHosier],
        domain(Professions, 0,4),

        all_different(Names),
        all_different(Professions),

        % Symmetry breaking
        Baker   #= 1,

        % Each gentleman is the namesake of the profession of one of the others.
        PBaker  #\= Baker,
        PDyer   #\= Dyer,
        PFarmer #\= Farmer,
        PGlover #\= Glover,
        PHosier #\= Hosier,

        % The dyer is seated two places to the left of Mr Hosier.
        PDyer #= (Hosier - 2) mod 5,

        % The baker sits two places to Mr Baker's right.
        PBaker #= (Baker + 2) mod 5,

        % The farmer is seated to the left of Mr Farmer.
        PFarmer #= (Farmer - 1) mod 5,

        % Mr Dyer is on the glover's right.
        Dyer #= (PGlover + 1) mod 5,

        % search
        append(Names,Professions,Vars),
        labeling([], Vars),

        % output
        nl,
        write('[Baker, Dyer, Farmer, Glover, Hosier]':Names),nl,
        write('[baker, dyer, farmer, glover, hosier]':Professions),nl,
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
        nl,
        fd_statistics.
        


