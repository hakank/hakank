/*

  Set covering problem in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%
% First find the optimal value (MinVal), then find all the solutions with that value.
%
go :-

        writeln('Find the optimal solution'),
        belongs(Belongs),
        set_covering3(Belongs, MinVal,_),
        
        format("\nFinding all optimal solutions with MinVal ~d:\n", [MinVal]),
        findall(X, set_covering3(Belongs,  MinVal,X),L),
        length(L,Len),
        format("It was ~d solutions~n", [Len]).


set_covering3(Belongs, MinVal, X) :-

        transpose(Belongs,BelongsT),
        length(BelongsT,NumSenators),

        % which senator to choose
        length(X,NumSenators),
        X ins 0..1,

        % cover all groups with the senators
        maplist(cover_groups(X),Belongs),

        % objective: minimize the number of senators
        sum(X,#=,MinVal),

        % Either search for all solutions (for the minimum value) or
        % the optimal value.
        ( ground(MinVal)
        -> 
          label(X)
        ;
          labeling([min(MinVal)], X)
        ),

        writeln(x=X),
        senators(SenatorNames),
        findall(S,(between(1,NumSenators,I),element(I,X,1),
                   nth1(I,SenatorNames,S)
                  ),
               SelectedSenators),
        format("These selectors are selected: ~w~n",[SelectedSenators]),
        writeln(minVal=MinVal).

% Ensure that each group (row in Belong) is covered by at least one senator.
cover_groups(X,Belong) :-
        scalar_product(Belong,X,#>=,1).


%
% The Belong matrix:
%
% 1 if a senator belongs to the group, 
% 0 if senator don't belong to the group
%
belongs(Matrix) :- 
        Matrix = [[1, 1, 1, 1, 1, 0, 0, 0, 0, 0],  % 1 southern
                  [0, 0, 0, 0, 0, 1, 1, 1, 1, 1],  % 2 northern
                  [0, 1, 1, 0, 0, 0, 0, 1, 1, 1],  % 3 liberals
                  [1, 0, 0, 0, 1, 1, 1, 0, 0, 0],  % 4 conservative
                  [0, 0, 1, 1, 1, 1, 1, 0, 1, 0],  % 5 democrats
                  [1, 1, 0, 0, 0, 0, 0, 1, 0, 1]]. % 6 republicans

senators(Senators) :- 
        Senators = [a,b,c,d,e,f,g,h,i,j].

