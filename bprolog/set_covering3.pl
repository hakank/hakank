/*

  Set covering in B-Prolog.

  Problem from 
  Katta G. Murty: "Optimization Models for Decision Making", page 302f
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
 
  10 senators making a committee, where there must at least be one 
  representative from each group:
  group:        senators:
  southern      1 2 3 4 5
  northern      6 7 8 9 10
  liberals      2 3 8 9 10
  conservative  1 5 6 7
  democrats     3 4 5 6 7 9
  republicans   1 2 8 10

  The objective is to minimize the number of senators.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



%
% First find the optimal value (MinVal), then find all the solutions with that value.
%
go :-

        writeln('Find the optimal solution'),
        belongs(Belongs),
        set_covering3(Belongs, MinVal,_),

        format("Finding all optimal solutions with MinVal ~d:\n", [MinVal]),
        findall(X, set_covering3(Belongs,  MinVal,X), L),
        length(L, Len),
        % writeln(L),
        format("It was ~d solutions\n", [Len]).


set_covering3(Belongs, MinVal, X) :-

        % The dimensions of Belongs matrix
        NumGroups @= Belongs^length,
        NumSenators @= Belongs[1]^length,

        % which senator to choose
        length(X,NumSenators),
        X :: 0..1,

        % cover all groups with the senators
        foreach(I in 1..NumGroups,
                sum([X[J]*Belongs[I,J]: J in 1..NumSenators]) #>= 1),


        % objective: minimize the number of senators
        MinVal #= sum(X),

        % Either search for all solutions (for the minimum value) or
        % the optimal value.
        (
            ground(MinVal) 
        -> 
            labeling(X)
        ;
            minof(labeling(X),MinVal)
        ),

        writeln(x:X),
        senators(SenatorNames),
        foreach(I in 1..NumSenators,ac(S,[]),[Name],
                (X[I] =:= 1 -> 
                     Name @= SenatorNames[I],
                     S^1 @= [Name|S^0] 
                ; 
                     S^1 = S^0
                )),
        reverse(S,S2),
        format("These senators are selected: ~q\n", [S2]),
        writeln(minVal:MinVal),
        nl.


%
% The Belong matrix:
%
% 1 if a senator belongs to the group, 
% 0 if senator don't belong to the group
%
belongs([]([](1, 1, 1, 1, 1, 0, 0, 0, 0, 0),   % 1 southern
           [](0, 0, 0, 0, 0, 1, 1, 1, 1, 1),   % 2 northern
           [](0, 1, 1, 0, 0, 0, 0, 1, 1, 1),   % 3 liberals
           [](1, 0, 0, 0, 1, 1, 1, 0, 0, 0),   % 4 conservative
           [](0, 0, 1, 1, 1, 1, 1, 0, 1, 0),   % 5 democrats
           [](1, 1, 0, 0, 0, 0, 0, 1, 0, 1))). % 6 republicans

senators([a,b,c,d,e,f,g,h,i,j]).