/*

  Babysitting puzzle (Dell Logic Puzzles) in ECLiPSe.

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


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(listut).
:-lib(propia).


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

        dim(Age,[N]),
        Age :: 2..6,

        alldifferent(Last),
        alldifferent(Age),

        % 1. One child is named Libby Jule.
        Libby #= Jule,

        % 2. Keith is one year older than the Ivey child, who is one year 
        %    older than Nora.
        Age[Keith] #= Age[Ivey] + 1,
        Age[Ivey] #= Age[Nora] + 1,

        % 3. The Fell child is three years older than Margo.
        Age[Fell] #= Age[Margo] + 3,

        % 4. Otto is twice as many years old as the Hall child.
        Age[Otto] #= Age[Hall]*2,

        % search
        term_variables([First,Last,Age], Vars),
        labeling(Vars),

        % print solution
        FirstS = ["Keith", "Libby", "Margo", "Nora", "Otto"],
        LastS  = ["Fell", "Gant", "Hall", "Ivey", "Jule"],
        calc_size([FirstS,LastS], Size, 2),
        print_all(First,FirstS,Size),
        print_all(Last,LastS,Size),
        % print ages
        concat_string(["%",Size, "d"],Format),
        ( foreacharg(A, Age), param(Format) do
              printf(Format, [A])
        ),
        nl.


print_all(X,S,Size) :-
        length(X,Len),
        concat_string(["%",Size, "s"],Format),
        (for(I,1,Len), param(X,S,Format) do            
             nth1(IX,X,I),
             nth1(IX,S,This),
             printf(Format, [This])
        ),nl.


% Size is 2 + the length of the largest string.
calc_size(A, Size, Add) :-
        flatten(A,List),
        ( foreach(String,List), foreach(Len, Lengths) do
              string_length(String,Len)
        ),
        Size is maxlist(Lengths) + Add.
