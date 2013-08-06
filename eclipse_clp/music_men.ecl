/*

  Music men puzzle in ECLiPSe.

  """
  Three friends like different kinds of music.  From the clues given
  below, can you identify them, say how old each is, and work out
  his musical preference?

  Clues: 
  1.      Rob is older than Queen, who likes classical music.
  2.      The pop-music fan, who is not Prince, is not 24.
  3.      Leon, who is not King, is 25.
  4.      Mark's musical preference is not jazz.
  """

  Knowledge: "this is what we know of the world."
  Names           : Leon, Mark, Rob.
  Surnames        : King, Prince, Queen.
  Ages            : 24, 25, 26.
  Music           : Classical, Jazz, Pop.


  Solution:
    Leon Prince, 25, jazz.
    Mark Queen, 24, classical.
    Rob King, 26, pop.


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:- lib(ic), lib(listut).


go :-
        music_men([Age,Names,Surnames,Music]),
        writeln([Age,Names,Surnames,Music]),

        ( foreach(X1,Names) do
              I is X1-23,
              listut:nth1(I,[king,prince,queen],V),
              write([I,V]), write(" ")
        ),
        nl,
        ( foreach(X,Surnames) do
              I is X-23,
              listut:nth1(I,[leon,mark,rob],V),
              write([I,V]), write(" ")
        ),
        nl,
        ( foreach(X,Music) do
              I is X-23,
              listut:nth1(I,[classical,jazz,pop],V),
              write([I,V]), write(" ")
        ),
        nl.


go2 :-
        findall([age:Age,names:Names,surnames:Surnames,music:Music],music_men([Age,Names,Surnames,Music]),L),
        writeln(L).


go3 :-
        music_men([
                      [Age24, Age25, Age26],
                      [King, Prince, Queen],
                      [Leon, Mark, Rob],
                      [Classical, Jazz, Pop]
                  ]),
        writeln([age24=Age24, age25=Age25, age26=Age26]),
        writeln([king=King, prince=Prince, queen=Queen]),
        writeln([leon=Leon, mark=Mark, rob=Rob]),
        writeln([classical=Classical, jazz=Jazz, pop=Pop]).


music_men([Age,Names,Surnames,Music]) :-

        Age      = [Age24, Age25, Age26],
        Names    = [King, Prince, Queen],
        Surnames = [Leon, Mark, Rob],
        Music    = [Classical, Jazz, Pop],

        Age      :: 24..26,
        Names    :: 24..26,
        Surnames :: 24..26,
        Music    :: 24..26,

        alldifferent(Age),
        alldifferent(Names),
        alldifferent(Surnames),
        alldifferent(Music),


        % Age
        Age24 #= 24,
        Age25 #= 25,
        Age26 #= 26,

        % Rob is older than Queen, who likes classical music.
        Rob #> Queen,
        Queen #= Classical,

        % The pop-music fan, who is not Prince, is not 24.
        Pop #\= Prince,
        Pop #\= Age24,

        % Leon, who is not King, is 25.
        Leon #\= King,
        Leon #= Age25,

        %  Mark's musical preference is not jazz.
        Mark #\= Jazz,

        flatten([Age,Names,Surnames,Music],List),

        labeling(List).

