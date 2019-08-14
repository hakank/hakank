/*

  Music men puzzle in SWI Prolog

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



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   music_men([Age,Names,Surnames,Music]),
   NamesS = ["King","Prince","Queen"],
   SurnamesS = ["Leon","Mark","Rob"],
   MusicS = ["classical","jazz","pop"],
   get_sol(Names,NamesS,NamesSol),
   get_sol(Surnames,SurnamesS,SurnamesSol),
   get_sol(Music,MusicS,MusicSol),
   transpose([SurnamesSol,NamesSol,Age,MusicSol],SolT),
   maplist(format("~w\t~w\t~w\t~w~n"),SolT),
   
   nl.

%%
%% Preparation for pretty print.
%%
get_sol(List,Lookup,Sol) :-
        findall(Val,
                (member(L,List),
                 Ix #= L-23,
                 nth1(Ix,Lookup,Val)
                ),
                Sol).

music_men([Age,Names,Surnames,Music]) :-

        Age      = [Age24, Age25, Age26],
        Names    = [King, Prince, Queen],
        Surnames = [Leon, Mark, Rob],
        Music    = [Classical, Jazz, Pop],
        
        Age      = [24,25,26],
        Names    ins 24..26,
        Surnames ins 24..26,
        Music    ins 24..26,

        all_different(Age),
        all_different(Names),
        all_different(Surnames),
        all_different(Music),

        %% Age
        Age24 #= 24,
        Age25 #= 25,
        Age26 #= 26,
        
        %% Rob is older than Queen, who likes classical music.
        Rob #> Queen,
        Queen #= Classical,

        %% The pop-music fan, who is not Prince, is not 24.
        Pop #\= Prince,
        Pop #\= Age24,

        %% Leon, who is not King, is 25.
        Leon #\= King,
        Leon #= Age25,

        %%  Mark's musical preference is not jazz.
        Mark #\= Jazz,

        flatten([Names,Surnames,Music],Vars),

        labeling([], Vars).

