/*

  Tourist Site Competition in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        R = 3,
        C = 3,
        Lambda = 1,

        %% Sites (Swedish sites)
        Birka = 1,
        Falun = 2,
        Lund = 3,
        Mora = 4,
        Sigtuna = 5,
        Uppsala = 6,
        Ystad = 7,
        Sites = [Birka, Falun, Lund, Mora, Sigtuna, Uppsala, Ystad],
        SitesStr = ["Birka", "Falun", "Lund", "Mora", "Sigtuna", "Uppsala", "Ystad"],
        
        %% Judges
        Ali = 1,
        Dan = 2,
        Eva = 3,
        Jim = 4,
        Leo = 5,
        Mia = 6,
        Ulla = 7,
        Judges = [Ali, Dan, Eva, Jim, Leo, Mia, Ulla],
        JudgesStr = ["Ali", "Dan", "Eva", "Jim", "Leo", "Mia", "Ulla"],
        
        SymmetryBreaking = true,
        tourist_site_competition(Sites,Judges,R,C, Lambda,SymmetryBreaking,X),
        
        %% foreach(Row in X) writeln(Row) end,
        maplist(writeln,X),
        nl,
        print_assignments(X,SitesStr,JudgesStr),
        nl.

%%
%% Checking the number of solutions
%%
%% 
go2 :-
        NumJudges = 7,
        numlist(1,NumJudges,Judges),
        NumSites = 7,
        numlist(1,NumSites,Sites),        
        R = 3,
        C = 3,
        Lambda = 1,
        findall(X, tourist_site_competition(Sites,Judges,R,C, Lambda,true,X),L1),
        length(L1,Len1),
        format("With symmetry breaking: ~d solutions.\n", [Len1]),
        
        findall(X, tourist_site_competition(Sites,Judges,R,C, Lambda,false,X),L2),
        length(L2,Len2),
        format("Without symmetry breaking: ~d solutions.\n", [Len2]),
        nl.


% num_solutions(Sites,Judges,R,C,Lambda) = NumSolutions :-
%   L = findall(X, tourist_site_competition(Sites,Judges,R,C, Lambda,_,X)),
%   NumSolutions = L.length.

tourist_site_competition(Sites,Judges,R,C,Lambda,SymmetryBreaking,X) :-

        length(Sites,NumSites),
        length(Judges,NumJudges),
        writeln([numSites=NumSites, numJudges=NumJudges,lambda=Lambda]),

        %% decision variable
        new_matrix(NumSites,NumJudges, 0..1, X),
        flatten(X,Vars),

        %% Every tourist site is visited by R judges.
        maplist(site_visited_by_r_judges(R),X),
        
        %% Every judge visits C tourist sites.
        transpose(X,XT),
        maplist(judge_visits_c_sites(C),XT),
        
        %% Every pair of sites is visited by Lambda common judges.
        findall([S1,S2],
                (member(S1,Sites),
                 member(S2,Sites),
                 S1 < S2
                ),
                SSs),
        maplist(visited_by_lambda_common_judges(X, Judges, Lambda),SSs),        
                
        %% Symmetry breaking: Assigns the first three sites to judge 1
        (
         SymmetryBreaking == true
        ->
         numlist(1,R,Rs),
         maplist(symmetry_breaking(X),Rs)
        ;
         true
        ),
        
        labeling([ff], Vars).

%% Every tourist site is visited by R judges.
site_visited_by_r_judges(R,Row) :-
        sum(Row,#=,R).

%% Every judge visits C tourist sites.
judge_visits_c_sites(C,Column) :-
        sum(Column,#=,C).

%% Every pair of sites is visited by Lambda common judges.
visited_by_lambda_common_judges(X,Judges,Lambda,[S1,S2]) :-
        sum_common_judges_on_site(Judges,X,S1,S2,0,Lambda).
sum_common_judges_on_site([],_X,_S1,_S2,Sum,Sum).
sum_common_judges_on_site([J|Js],X,S1,S2,Sum0,Sum) :-
        matrix_element(X,S1,J,XS1J),
        matrix_element(X,S2,J,XS2J),
        B in 0..1,
        (XS1J #= 1 #/\ XS2J #= 1) #<==> B #= 1,
        Sum1 #= Sum0 + B,
        sum_common_judges_on_site(Js,X,S1,S2,Sum1,Sum).

symmetry_breaking(X,R) :-
        matrix_element(X,R,1,1).
        

print_assignments(X,SitesStr,JudgesStr) :-
        length(X,XLen),
        length(JudgesStr,JLen),
        findall([Site,JudgeList],
                (between(1,XLen,S),
                 nth1(S,SitesStr,Site),
                 findall(Judge,
                         (
                          between(1,JLen,J),
                          matrix_element(X,S,J,1),                         
                          nth1(J,JudgesStr,Judge)
                         ),
                         JudgeList
                        )
                ),
                Sol),
        maplist(format("~w~t~10|: ~w~n"),Sol),
        nl.


