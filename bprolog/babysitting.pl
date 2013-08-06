/*

  Babysitting puzzle (Dell Logic Puzzles) in B-Prolog.

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        
        N = 5,

        Keith = 1, 
        Libby = 2, 
        Margo = 3, 
        Nora  = 4, 
        Otto  = 5,
        First = [Keith, Libby, Margo, Nora, Otto],

        Last  = [Fell, _Gant, Hall, Ivey, Jule],
        Last :: 1..N,

        length(Age,N),
        Age :: 2..6,

        alldifferent(Last),
        alldifferent(Age),

        % 1. One child is named Libby Jule.
        Libby #= Jule,
        % 2. Keith is one year older than the Ivey child, who is one year 
        %    older than Nora.

        % It's a little boring that this don't work:
        %   Age[Keith] #= Age[Ivey] + 1
        element(Ivey, Age,AgeIvey),
        Age[Keith] #= AgeIvey + 1,
        AgeIvey #= Age[Nora] + 1,

        % 3. The Fell child is three years older than Margo.
        element(Fell, Age, AgeFell),
        AgeFell #= Age[Margo] + 3,

        % 4. Otto is twice as many years old as the Hall child.
        element(Hall,Age,AgeHall),
        Age[Otto] #= AgeHall*2,

        % search
        term_variables([First,Last,Age], Vars),
        labeling(Vars),

        writeln(first:First),
        writeln(last:Last),
        writeln(age:Age),
        nl,

        % print solution
        FirstS = ['Keith', 'Libby', 'Margo', 'Nora', 'Otto'],
        LastS  = ['Fell', 'Gant', 'Hall', 'Ivey', 'Jule'],
        print_all(First,FirstS),
        print_all(Last,LastS),
        foreach(A in Age, format("~d ",[A])),
        nl.


print_all(X,S) :-
        length(X,Len),
        foreach(I in 1..Len, [IX,This],
                (nth1(IX,X,I),
                 nth1(IX,S,This),
                 write(This), write(' ')
                )
        ),nl.
