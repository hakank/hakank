/*

  Tourist Site Competition in B-Prolog.

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
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

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
        Rows @= X^rows,
        foreach(Row in Rows, writeln(Row)),
        nl,
        print_assignments(X,SitesStr,JudgesStr).


tourist_site_competition(Sites,Judges,R,C,Lambda,SymmetryBreaking,X) :-

        length(Sites,NumSites),
        length(Judges,NumJudges),

        % decision variable
        new_array(X,[NumSites,NumJudges]),
        array_to_list(X,Vars),
        Vars :: 0..1,


        % Symmetry breaking: Assigns the first site to judges {1,2,3}
        ( SymmetryBreaking ->
              foreach(I1 in 1..3, X[I1,1] #= 1)
        ;
              true
        ),
        
        % Every tourist site is visited by R judges.
        Rows @= X^rows,
        foreach(Row in Rows, sum(Row) #= R),

        % Every judge visits C tourist sites.
        Columns @= X^columns,
        foreach(Column in Columns, sum(Column) #=C),


        % Every pair of sites is visited by Lambda common judges.
        foreach((X1,I) in (Rows,1..NumSites),
                (foreach((X2,J) in (Rows,1..NumSites),
                         (
                           I < J ->
                           sum([ (XX1 #= 1 #/\ XX1 #= XX2) :
                               (XX1,XX2) in (X1, X2) ]) #>= Lambda
                         ;
                           true
                         )
                        )
                )
               ),
        
        % search
        labeling([ff], Vars).



print_assignments(X,SitesStr,JudgesStr) :-
        Rows @= X^rows,
        Len @= Rows^length,
        foreach((Row,S) in (Rows,SitesStr),
                [Where],
                (
                    format('~w\t: ',[S]),
                    Where @= 
                    [ Judge :
                    (R,I) in (Row, 1..Len),
                     [Judge],
                     (R =:= 1, 
                      nth1(I,JudgesStr,Judge)
                     )
                    ],
                    writeln(Where)
                )
               ),
        nl.


