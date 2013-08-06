/*

  Miss Manners seating problem in ECLiPSe.

  
  From http://4c110.ucc.ie/cpstandards/index.php/en/standards/java/examples/27
  """
  The "Miss Manners" problem is a notorious benchmark for rule engines. 
  The problem is to find an acceptable seating arrangement for guests at 
  a dinner party.  It should match people with the same hobbies (at 
  least one), and to seat everyone next to a member of the opposite sex. 
  """

  The data is presented in the Excel file: 
  http://4c110.ucc.ie/cpstandards/files/Manners.xls
  

  Also, see 
   - http://docs.codehaus.org/display/DROOLS/Miss+Manners+Example
   - http://blog.athico.com/2009/05/miss-manners-2009-yet-another-drools.html
   - http://it.toolbox.com/blogs/thinking-out-loud/industry-analysts-and-rules-engines-2349
     Refers to OPS5 benchmark suite: 
     ftp://ftp.cs.utexas.edu/pub/ops5-benchmark-suite/
  
 
  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/miss_manners.mzn
  Data for the MiniZinc version:
  * http://www.hakank.org/minizinc/miss_manners_16.dzn
  * http://www.hakank.org/minizinc/miss_manners_64.dzn
  * http://www.hakank.org/minizinc/miss_manners_128.dzn

  * SICStus Prolog: http://www.hakank.org/sicstus/miss_manners.pl

  Notes: 
  - This model (as well as the MiniZinc and SICStus models) assumes 
    a circular table placement here.
  - It maximized the number of common hobbies. 
  - It don't care about the preferences (order) of the hobbies.


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_sets).
:-lib(branch_and_bound).
:-lib(listut).
:-lib(ic_global).
:-lib(ic_global_gac).
%:-lib(ic_search).

%:-lib(propia).



go :-
        miss_manners(1),
        miss_manners(2),
        miss_manners(3).


miss_manners(P) :-        
        problem(P,Size, GenderList, NumHobbies, Hobbies),
        printf('\nProblem %w (size %d)\n',[P,Size]),
        writeln(""),

        length(Seating,Size),
        ic:(Seating :: 1..Size),

        ic_global:alldifferent(Seating),

        % total number of common hobbies of the two neighbours 
        % (to be maximized)
        NH is (Size+1)*NumHobbies,
        ic:(CommonHobbies :: 0..NH),

        % mix genders
        ( fromto(Seating, [This,Next | Rest], [Next|Rest],[_]),
          param(GenderList) do
              nth1(This,GenderList,ThisG),
              nth1(Next,GenderList,NextG),
              ThisG #\= NextG
        ),

        % "around the corner"
        nth1_gender(1,Seating,GenderList,Gender1),
        nth1_gender(Size,Seating,GenderList,GenderLast),
        Gender1 #\= GenderLast,

        % match hobbies: there should at least be one common
        %                hobby (in the maximimzation phase
        %                we may get more)
        ( fromto(Seating, [This2,Next2 | Rest2], [Next2|Rest2],[_]),
          fromto(0,In,Out,TotalCommonHobbies),
          param(Hobbies) do
              nth1(This2,Hobbies,ThisHobbies),
              nth1(Next2,Hobbies,NextHobbies),
              match_hobbies(ThisHobbies,NextHobbies,NumMatched),
              NumMatched #>= 1,
              Out #= In + NumMatched
        ),

        % "around the corner"
        nth1(1,Hobbies,Hobbies1),
        nth1(Size,Hobbies,HobbiesLast),
        match_hobbies(Hobbies1,HobbiesLast,NumMatchedAround),
        NumMatchedAround #>= 1,

        % total number of common hobbies
        CommonHobbies #= TotalCommonHobbies + NumMatchedAround,


        % search
        CommonHobbiesNeg #= -CommonHobbies,
        % minimize(search(Seating, 0, first_fail,indomain_min, complete,
        %                [backtrack(Backtracks)]), CommonHobbiesNeg),

        bb_min(search(Seating, 0, first_fail,indomain_random, complete,
                        [backtrack(Backtracks)]), CommonHobbiesNeg,
               bb_options{strategy:restart}),


        % output
        write(seating:Seating),nl,
        write(common_hobbies:CommonHobbies),nl,
        write(backtracks:Backtracks), nl.



match_hobbies(H1,H2,NumMatched) :-
        ( foreach(A,H1),
          fromto(0,In,Out,NumMatched),
          param(H2) do
              ( foreach(B,H2),
                fromto(0,In2,Out2,Matches2),
                param(A) do
                    Out2 #= In2 + (A #= B)
              ),
              Out #= In + Matches2
        ).
        % list_to_fdset(H1,H1Set),
        % list_to_fdset(H2,H2Set),
        % fdset_intersection(H1Set,H2Set,Intersection),
        % Intersection #= H1Set /\ H2Set,
        % Intersection #= H1 /\ H2,
        % ic_sets:all_intersection([H1,H2],Intersection),
        % writeln([h1:H1,h2:H2,intersection:Intersection]),
        % fdset_size(Intersection,NumMatched).     
        % #(H1 /\ H2,NumMatched).


nth1_gender(Place,Seating,GenderList, Gender) :-
        nth1(Place,Seating,Who),
        nth1(Who, GenderList, Gender).


%
% data
%

%
% Data Guest guests16					
% Name	Gender	Hobbies			
% 		Hobby 1	Hobby 2	Hobby 3	Hobby 4
% 1	m	2	1	3	
% 2	f	2	1	3	
% 3	m	3	2		
% 4	m	3	2	1	
% 5	m	2	1	3	
% 6	m	2	3	1	
% 7	f	1	2	3	
% 8	m	3	1		
% 9	m	2	3	1	
% 10	m	3	2	1	
% 11	f	1	3	2	
% 12	f	3	1	2	
% 13	f	2	3		
% 14	f	1	2		
% 15	f	2	3	1	
% 16	f	2	3		

% Male = 1
% Female = 2
problem(1, 16, [1,2,1,1,1,1,2,1,1,1,2,2,2,2,2,2], 3, 
        [
            [2,1,3],
            [2,1,3],
            [3,2],
            [3,2,1],
            [2,1,3],
            [2,3,1],
            [1,2,3],
            [3,1],
            [2,3,1],
            [3,2,1],
            [1,3,2],
            [3,1,2],
            [2,3],
            [1,2],
            [2,3,1],
            [2,3]]).


% Male = 1
% Female = 2
problem(2, 64, [1,2,1,1,1,1,2,1,1,1,1,2,1,1,1,2,2,1,2,2,1,1,2,2,2,1,2,
                1,2,2,1,1,1,2,2,1,1,2,1,2,1,1,1,1,1,2,1,2,1,1,2,1,2,2,
                2,2,2,2,2,2,2,2,2,2], 3,
        [[2,1,3],
         [2,1,3],
         [3,2],
         [3,2,1],
         [2,1,3],
         [2,3,1],
         [1,2,3],
         [3,1],
         [2,3,1],
         [3,2,1],
         [1,3,2],
         [3,1,2],
         [2,3],
         [1,2],
         [2,3,1],
         [2,3],
         [3,2],
         [1,3,2],
         [3,1],
         [1,3,2],
         [2,3],
         [2,3],
         [1,2],
         [3,1,2],
         [3,1,2],
         [2,1,3],
         [2,3,1],
         [1,2],
         [2,3,1],
         [2,1,3],
         [1,2,3],
         [1,2],
         [2,3,1],
         [2,1,3],
         [2,3],
         [2,1],
         [2,1],
         [1,3,2],
         [3,1,2],
         [1,2,3],
         [2,1,3],
         [3,1],
         [1,3,2],
         [3,1,2],
         [1,2],
         [1,2,3],
         [1,2],
         [3,2],
         [3,2],
         [2,3],
         [2,1,3],
         [1,2,3],
         [2,1],
         [1,2,3],
         [1,2,3],
         [2,1,3],
         [3,2,1],
         [3,1,2],
         [1,2,3],
         [3,1],
         [3,2,1],
         [2,3],
         [3,1,2],
         [3,2]]).


% Data Guest guests128						
% Male = 1
% Female = 2
problem(3, 
        128,
        [1,2,2,1,1,2,2,1,1,1,1,2,2,1,1,1,1,2,1,1,1,1,2,2,1,1,1,2,2,1,
          1,2,2,1,2,1,2,2,1,1,2,1,1,2,1,1,1,2,1,1,1,2,2,2,1,1,2,1,2,2,
          1,2,2,2,1,2,2,1,1,2,1,1,2,2,2,2,1,1,2,1,1,1,2,1,2,1,1,1,2,1,
          2,2,2,2,2,1,2,1,2,1,2,2,1,1,2,2,2,2,2,1,2,2,2,1,1,1,2,2,1,1,
          2,1,1,2,2,2,2,2], 5,        
        [
            [2,3,1,4,5],
            [3,2,1,4,5],
            [5,4,2],
            [3,2,1,4],
            [2,5,3],
            [1,4,2,5,3],
            [1,2,3,5],
            [3,5],
            [3,5,2,4],
            [2,3,4,5,1],
            [2,4,5,1],
            [3,5,2],
            [5,1],
            [3,5,1,4],
            [5,2,1,3,4],
            [2,5,4],
            [4,3],
            [2,4,5,1],
            [5,2],
            [2,3],
            [4,3,2,1],
            [1,2,4],
            [4,5,2],
            [4,3,2,1,5],
            [4,5],
            [1,2],
            [3,5,2],
            [4,1,3,2,5],
            [3,5],
            [2,1],
            [3,1,4],
            [2,1,5],
            [5,4],
            [4,5,2],
            [3,1,4,5,2],
            [1,4,2,3],
            [4,2,1],
            [3,1,2,4],
            [5,2],
            [2,3],
            [5,3],
            [5,4],
            [3,4,5],
            [2,4,3,1],
            [2,4],
            [4,1,5],
            [2,5,4],
            [3,1],
            [1,5,4,3,2],
            [2,4,3],
            [1,2,5,4],
            [5,3,1,4],
            [5,1,4,2,3],
            [1,3],
            [2,4],
            [2,3],
            [2,1,4],
            [5,3,2,1,4],
            [2,3,5],
            [4,2,3,5],
            [1,2,4,3],
            [3,2,5,4,1],
            [2,3,4,1,5],
            [2,4,1,5,3],
            [5,3,2,1,4],
            [5,3,4],
            [1,4],
            [4,2,1,5],
            [3,4,1],
            [3,5,1,4,2],
            [3,1,4],
            [4,3,1],
            [4,1,3],
            [1,4,2,5,3],
            [4,2],
            [5,4,3,2,1],
            [1,5],
            [5,1],
            [1,3,4,2,5],
            [5,1,2,4],
            [4,1],
            [3,4,1,2,5],
            [1,3,4],
            [3,4],
            [4,1],
            [3,4],
            [5,4,2,3],
            [2,5,3,1,4],
            [5,2,4,1],
            [2,4,5],
            [2,3,5,1,4],
            [3,1,2,5],
            [3,1,2],
            [2,5],
            [3,4],
            [1,4,2,5,3],
            [4,2,5],
            [5,4],
            [2,5],
            [2,3,5,4],
            [2,1,5,3,4],
            [2,1,5,3,4],
            [2,1,4,3,5],
            [5,3,4,1,2],
            [4,2],
            [4,5,1],
            [3,1,2,5,4],
            [4,5,1,2,3],
            [1,2],
            [5,4,1,2],
            [3,2],
            [4,3,5,2,1],
            [1,3,5,2],
            [3,2],
            [2,1,5,4],
            [3,4,5,1],
            [4,5,2],
            [2,4,3],
            [5,3],
            [5,1,3,2,4],
            [4,2,3,5],
            [1,2,5],
            [2,3,1,5],
            [2,3,4],
            [3,5,2,4],
            [2,3,1,4],
            [5,1],
            [2,4,1,3]]).
