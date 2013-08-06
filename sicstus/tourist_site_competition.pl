/*

  Tourist Site Competition in SICStus Prolog.

  From Pierre Flener's presentation 
  "Constraint Technology - A Programming Paradigm on the Rise"
  http://www.it.uu.se/edu/course/homepage/ai/vt08/AI-CT.pdf
     pages 5f: problem statement 
     pages 12f: model
     pages 21ff: walktrough of a solution

  With 7 tourist sites and 7 judges:
  """
  Every tourist site is visited by r = 3 judges.
  Every judge visits c = 3 tourist sites.
  Every pair of sites is visited by lambda = 1 common judge.
  """

  There are 151200 solutions to this problem.
  With the additional constraint that Ali should visit Birka, Falun and Lund
  there are 4320 solutions.


  This problem was also presented as "The Airline-of-the-Year Problem"
  in his (Flener's) presentation
  "Constraint Programming - Programming Paradigm on the Rise"
  http://www.it.uu.se/research/group/astra/ATM-CT/Flener.pdf
  page 4f
  The problem is stated as follows for 7 airlines and 7 judges:
  """
  Constant jury: Every airline is tested by 3 judges.
  Constant load: Every judge tests 3 airlines.
  Equity: Every airline pair is tested by 1 common judge.
  """
  
  Compare with the following models:
  * Comet   : http://www.hakank.org/comet/tourist_site_competition.co
  * MiniZinc: http://www.hakank.org/minizinc/tourist_site_competition.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        R = 3,
        C = 3,
        Lambda = 1,

        % Sites
        Birka = 1,
        Falun = 2,
        Lund = 3,
        Mora = 4,
        Sigtuna = 5,
        Uppsala = 6,
        Ystad = 7,
        Sites = [Birka, Falun, Lund, Mora, Sigtuna, Uppsala, Ystad],
        SitesStr = ['Birka', 'Falun', 'Lund', 'Mora', 'Sigtuna', 'Uppsala', 'Ystad'],
        % Judges
        Ali = 1,
        Dan = 2,
        Eva = 3,
        Jim = 4,
        Leo = 5,
        Mia = 6,
        Ulla = 7,
        Judges = [Ali, Dan, Eva, Jim, Leo, Mia, Ulla],
        JudgesStr = ['Ali', 'Dan', 'Eva', 'Jim', 'Leo', 'Mia', 'Ulla'],
        
        SymmetryBreaking = true,

        tourist_site_competition(Sites,Judges,R,C,
                                 Lambda,SymmetryBreaking,X),
        ( foreach(Row,X) do write(Row),nl ),
        print_assignments(X,SitesStr,JudgesStr),
        % fail,
        fd_statistics.


tourist_site_competition(Sites,Judges,R,C,Lambda,SymmetryBreaking,X) :-

        length(Sites,NumSites),
        length(Judges,NumJudges),

        % decision variable
        matrix(X,[NumSites,NumJudges]),
        append(X,XList),
        domain(XList,0,1),

        % Symmetry breaking: Assigns the first site to judges {1,2,3}
        ( SymmetryBreaking ->
              ( for(I1,1,3),
                param(X) do
                    matrix_element(X,I1,1,1)
              )
        ;
              true
        ),

        % Every tourist site is visited by R judges.
        ( foreach(XRow,X),
          param(R) do
              sum(XRow,#=,R)
        ),

        % Every judge visits C tourist sites.
        transpose(X,XTransposed),
        ( foreach(XColumn,XTransposed),
          param(C) do
              sum(XColumn,#=,C)
        ),

        % Every pair of sites is visited by Lambda common judges.
        ( foreach(X1,X),
          count(I,1,_),
          param(X,Lambda) do
              ( foreach(X2,X),
                count(J,1,_),
                param(I,X1,Lambda) do
                    I < J ->
                    (
                       ( foreach(XX1,X1),
                         foreach(XX2,X2),
                         fromto(0,In,Out,Sum) do
                           Reif in 0..1,
                           (XX1 #= 1 #/\ XX1 #= XX2) #<=> Reif #= 1,
                           indomain(Reif),
                           Out #= In + Reif
                       ),
                       Sum #>= Lambda
                    )
                    ;
                    true
              )
        ),
                
        % search
        labeling([], XList).



print_assignments(X,SitesStr,JudgesStr) :-
        ( foreach(Row,X),
          foreach(S,SitesStr),
          param(JudgesStr) do
              format('~w: ',[S]),
              ( foreach(R,Row),
                count(I,1,_),
                fromto(Where,Out,In,[]),
                param(JudgesStr) do
                    R = 1 ->
                    (
                     nth1(I,JudgesStr,Judge),  
                     Out = [Judge|In]
                    )
              ;
                    Out = In
              ),
              write(Where),nl
        ),
        nl.




matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).
