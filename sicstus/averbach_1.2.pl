/*

  Seating puzzle in SICStus Prolog.

 
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
   



  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        Women = [American,_English,French],
        domain(Women,1,3),

        Table = [X,Y,_Z],
        domain(Table,1,3),

        all_different(Table),
        all_different(Women),

        rightTo(Y, American),
        leftTo(X, French),

        X #= 1, % symmetry breaking

        labeling([],Women),

        Str = [american, english, french],
        ( foreach(Place,Table),
          foreach(P,Placing),
          param(Str,Women) do
              element(Place,Women,WI),
              nth1(WI,Str,P)
        ),
        write(placing:Placing),nl,
        fd_statistics.



% x is right to y
rightTo(X, Y) :-
    X #= Y + 1 ;
    X #= Y - 2. % around the corner


leftTo(X, Y) :- 
    rightTo(Y,X).
