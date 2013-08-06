/*

  Music men puzzle in SICStus Prolog.

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
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        music_men([Age,Names,Surnames,Music]),
        write([Age,Names,Surnames,Music]),nl,
        ( foreach(X1,Names) do
              I is X1-23,
              nth1(I,[king,prince,queen],V),
              write([I,V]), write(" ")
        ),
        nl,
        ( foreach(X,Surnames) do
              I is X-23,
              nth1(I,[leon,mark,rob],V),
              write([I,V]), write(" ")
        ),
        nl,
        ( foreach(X,Music) do
              I is X-23,
              nth1(I,[classical,jazz,pop],V),
              write([I,V]), write(" ")
        ),
        nl.


go2 :-
        findall([age:Age,names:Names,surnames:Surnames,music:Music],music_men([Age,Names,Surnames,Music]),L),
        write(L),nl.


go3 :-
        music_men([
                      [Age24, Age25, Age26],
                      [King, Prince, Queen],
                      [Leon, Mark, Rob],
                      [Classical, Jazz, Pop]
                  ]),
        write([age24=Age24, age25=Age25, age26=Age26]),nl,
        write([king=King, prince=Prince, queen=Queen]),nl,
        write([leon=Leon, mark=Mark, rob=Rob]),nl,
        write([classical=Classical, jazz=Jazz, pop=Pop]),nl.


music_men([Age,Names,Surnames,Music]) :-

        Age      = [Age24, Age25, Age26],
        Names    = [King, Prince, Queen],
        Surnames = [Leon, Mark, Rob],
        Music    = [Classical, Jazz, Pop],

        domain(Age,24,26),
        domain(Names,24,26),
        domain(Surnames,24,26),
        domain(Music,24,26),

        all_different(Age),
        all_different(Names),
        all_different(Surnames),
        all_different(Music),

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

        append(Age,Names,Vars1),
        append(Vars1,Surnames,Vars2),
        append(Vars2,Music,List),

        labeling([], List).

