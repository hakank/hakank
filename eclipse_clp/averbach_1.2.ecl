/*

  Seating puzzle in ECLiPSe.
 
  From Averbach & Chein "Problem Solving Through Recreational Mathematics", 
  page 2, problem 1.2
  
  """
  Ms X, Ms Y, and Ms Z - and American woman, and Englishwoman, and a 
  Frenchwoman, but not neccessarily in that order, were seated around a 
  circular table, playing a game of Hearts. 
  Each passed three cards to the person on her right.
  Ms Y passed three hearts to the American, 
  Ms X passed the queen of spades and two diamonds to the person who
  passed her cards to the Frenchwoman
  
  Who was the American? The Englishwoman? The Frenchwoman?
  """"

  This model gives the following solution
  table: [1, 2, 3]
  [American, English, French]: [1, 3, 2]
 
          1                      American
                       
      3      2               English   French
   

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/averbach_1.2.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/averbach_1.2.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
:-lib(listut).
%:-lib(propia).


go :-
        Women = [American,_English,French],
        Women :: 1..3,

        Table = [X,Y,_Z],
        Table :: 1..3,

        alldifferent(Table),
        alldifferent(Women),

        rightTo(Y, American),
        leftTo(X, French),

        X #= 1, % symmetry breaking

        term_variables([Women,Table], Vars),
        labeling(Vars),
        writeln(women:Women),
        writeln(table:Table),
        Str = [american, english, french],
        ( foreach(Place,Table),
          foreach(P,Placing),
          param(Str,Women) do
              element(Place,Women,WI),
              nth1(WI,Str,P)
        ),
        writeln(placing:Placing).



% x is right to y
rightTo(X, Y) :-
    X #= Y + 1 
    ;
    X #= Y - 2. % around the corner


leftTo(X, Y) :- 
    rightTo(Y,X).
