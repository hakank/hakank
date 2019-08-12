/*

  Secret Santa problem II in SWI Prolog

  From Maple Primes: "Secret Santa Graph Theory"
  http://www.mapleprimes.com/blog/jpmay/secretsantagraphtheory
  """
  Every year my extended family does a "secret santa" gift exchange. 
  Each person draws another person at random and then gets a gift for 
  them. At first, none of my siblings were married, and so the draw was 
  completely random. Then, as people got married, we added the restriction 
  that spouses should not draw each others names. This restriction meant 
  that we moved from using slips of paper on a hat to using a simple 
  computer program to choose names. Then people began to complain when 
  they would get the same person two years in a row, so the program was 
  modified to keep some history and avoid giving anyone a name in their 
  recent history. This year, not everyone was participating, and so after 
  removing names, and limiting the number of exclusions to four per person, 
  I had data something like this:
  
  Name: Spouse, Recent Picks
  
  Noah: Ava. Ella, Evan, Ryan, John
  Ava: Noah, Evan, Mia, John, Ryan
  Ryan: Mia, Ella, Ava, Lily, Evan
  Mia: Ryan, Ava, Ella, Lily, Evan
  Ella: John, Lily, Evan, Mia, Ava
  John: Ella, Noah, Lily, Ryan, Ava
  Lily: Evan, John, Mia, Ava, Ella
  Evan: Lily, Mia, John, Ryan, Noah
  """
  
  Note: I interpret this as the following three constraints:
    1) One cannot be a Secret Santa of one's spouse
    2) One cannot be a Secret Santa for somebody two years in a row
    3) Optimization: maximize the time since the last time 

  This model also handle single persons, something the original
  problem don't mention.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

        %% N = 8, %% Without Single person
        N = 9, %% With a Single person
        Noah = 1,
        Ava  = 2,
        Ryan = 3,
        Mia  = 4,
        Ella = 5,
        John = 6,
        Lily = 7,
        Evan = 8,
        _Single = 9,

        Spouses = 
        [
         Ava,  %% Noa
         Noah, %% Ava
         Mia,  %% Rya
         Ryan, %% Mia
         John, %% Ella
         Ella, %% John
         Evan, %% Lily
         Lily  %% Evan
        , 0    %% Single has no spouse
        ], 

        N1 #= N+1,
        M #= N1, %% "large M" to indicate no earlier history

        %%
        %% The matrix version of earlier rounds.
        %% M means that no earlier Santa.
        %% Note: Ryan and Mia has the same recipient for years 3 and 4,
        %%       and Ella and John has for year 4. 
        %%       This seems to be caused by modification of 
        %%       original data.
        %%
        %%
        %% rounds with a single person (fake data)
        %%
        Rounds =       [
                       %%N  A  R  M  El J  L  Ev S
                        [0, M, 3, M, 1, 4, M, 2, 2], %% Noah
                        [M, 0, 4, 2, M, 3, M, 1, 1], %% Ava 
                        [M, 2, 0, M, 1, M, 3, 4, 4], %% Ryan
                        [M, 1, M, 0, 2, M, 3, 4, 3], %% Mia 
                        [M, 4, M, 3, 0, M, 1, 2, M], %% Ella
                        [1, 4, 3, M, M, 0, 2, M, M], %% John
                        [M, 3, M, 2, 4, 1, 0, M, M], %% Lily
                        [4, M, 3, 1, M, 2, M, 0, M], %% Evan
                        [1, 2, 3, 4, M, 2, M, M, 0]  %% Single
                       ],


        %% decision variables
        length(Santas,N),
        Santas ins 1..N,
        length(Santas2,N),
        Santas2 ins 1..N,
        length(SantaDistance,N),
        SantaDistance ins 1..N1,
        Z in 0..1000, %% total distance (to minimize)

        %% constraints

        %% Everyone gives and receives a Secret Santa
        all_different(Santas),

        %% no Santa for a spouses
        numlist(1,N,Is),
        maplist(no_self_santa(Santas),Is),
        maplist(no_santa_for_spouses,Santas,Spouses),
        
        
        %% Cannot be a Secret Santa for the same person two years in a row.
        maplist(no_santa_two_years_in_a_row_to_same_person(Rounds,Santas,Santas2),Is),

        %% optimize "distance" to earlier rounds:
        maplist(distance_to_earlier_rounds(Santas,SantaDistance,Rounds),Is),

        sum(SantaDistance,#=,Z),

        flatten([Santas,Santas2], Vars),
        
        labeling([max(Z)], Vars),

        writeln(z=Z),
        writeln(santas=Santas),
        writeln(santas2=Santas2),
        writeln(santaDistance=SantaDistance),
        nl.

%%
%% No Self-Santa.
%%
no_self_santa(Santas,I) :-
        element(I,Santas,SaI),
        SaI #\= I.

%%
%% No Santa for spouses.
%%
no_santa_for_spouses(Sa,Sp) :-
        Sp #> 0 #==> Sp #\= Sa.


%%
%% optimize "distance" to earlier rounds:
%%
distance_to_earlier_rounds(Santas,SantaDistance,Rounds,I):-
        element(I,Santas,SantasI),
        element(I,SantaDistance,SantaDistanceI),        
        matrix_element(Rounds,I,SantasI,SantaDistanceI).

%%
%% Cannot be a Secret Santa for the same person two years in a row.
%%
no_santa_two_years_in_a_row_to_same_person(Rounds,Santas,Santas2,I) :-
        element(I,Santas,SantasI),
        element(I,Santas2,Santas2I),
        SantasI #\= Santas2I,
        matrix_element(Rounds,I,Santas2I,1).