/*

  Babysitting puzzle (Dell Logic Puzzles) in SICStus Prolog.

  """
  Title: Babysitting
  Author: Scott Marley
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Each weekday, Bonnie takes care of five of the neighbors' children. 
  The children's names are Keith, Libby, Margo, Nora, and Otto; last 
  names are Fell, Gant, Hall, Ivey, and Jule. Each is a different
  number of years old, from two to six. Can you find each child's 
  full name and age?

  1. One child is named Libby Jule.
  2. Keith is one year older than the Ivey child, who is one year 
     older than Nora.
  3. The Fell child is three years older than Margo.
  4. Otto is twice as many years old as the Hall child.

  Determine: First name - Last name - Age 
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/eclipse/babysitting.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/baby_sitting.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        
        N = 5,

        Keith = 1, 
        Libby = 2, 
        Margo = 3, 
        Nora  = 4, 
        Otto  = 5,
        First = [Keith, Libby, Margo, Nora, Otto],

        Last  = [Fell, _Gant, Hall, Ivey, Jule],
        domain(Last, 1, N),

        length(Age,N),
        domain(Age,2,6),
        Age = [AgeKeith,_AgeLibby,AgeMargo,AgeNora,AgeOtto],

        all_different(Last),
        all_different(Age),

        % 1. One child is named Libby Jule.
        Libby #= Jule,


        element(Hall,Age,AgeHall),
        element(Ivey, Age,AgeIvey),

        % 2. Keith is one year older than the Ivey child, who is one year 
        %    older than Nora.
        AgeKeith #= AgeIvey + 1,
        AgeIvey #= AgeNora + 1,

        % 3. The Fell child is three years older than Margo.
        element(Fell, Age,AgeFell),
        AgeFell #= AgeMargo + 3,

        % 4. Otto is twice as many years old as the Hall child.
        AgeOtto #= AgeHall*2,

        % search
        append(Last,Age, Vars),
        labeling([], Vars),

        % print solution
        FirstS = ['Keith', 'Libby', 'Margo', 'Nora', 'Otto'],
        LastS  = ['Fell', 'Gant', 'Hall', 'Ivey', 'Jule'],
        % calc_size([FirstS,LastS], Size, 2),
        print_all(First,FirstS,_Size),
        print_all(Last,LastS,_Size),
         ( foreach(A, Age), 
           param(Format) do
               format("~w\t", [A])
         ),
        nl.


print_all(X,S,Size) :-
        length(X,Len),
        (for(I,1,Len), 
         param(X,S,Size) do            
             nth1(IX,X,I),
             nth1(IX,S,This),
             format('~w\t', [This])
        ),nl.


% % Size is 2 + the length of the largest string.
calc_size(A, Size, Add) :-
        ( foreach(String,A), 
          foreach(Len, Lengths) do
              length(String,Len)
        ),
        % Size is maxlist(Lengths) + Add.
        maximum(Max,Lengths),
        Size is Max + Add.
        
