/*

  Euler Problem 18 in SWI Prolog

  """
  By starting at the top of the triangle below and moving to adjacent 
  numbers on the row below, the maximum total from top to bottom is 23.

  3
  7 4
  2 4 6
  8 5 9 3

  That is, 3 + 7 + 4 + 9 = 23.

  Find the maximum total from top to bottom of the triangle below:

   75
   95 64
   17 47 82
   18 35 87 10
   20 04 82 47 65
   19 01 23 75 03 34
   88 02 77 73 07 63 67
   99 65 04 28 06 16 70 92
   41 41 26 56 83 40 80 70 33
   41 48 72 33 47 32 37 16 94 29
   53 71 44 65 25 43 91 52 97 51 14
   70 11 33 28 77 73 17 78 39 68 17 57
   91 71 52 38 17 14 91 43 58 50 27 29 48
   63 66 04 68 89 53 67 30 73 16 69 87 40 31
   04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

  NOTE: As there are only 16384 routes, it is possible to solve this problem 
  by trying every route. However, Problem 67, is the same challenge with a 
  triangle containing one-hundred rows; it cannot be solved by brute force, 
  and requires a clever method! ;o)
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).



go :-
        L = [
             %% euler18a,
             euler18b
            ],
        run_problems(L).


p18(Triangle) :- 
        Triangle  = 
        [[75],
         [95,64],
         [17,47,82],
         [18,35,87,10],
         [20, 4,82,47,65],
         [19, 1,23,75, 3,34],
         [88, 2,77,73, 7,63,67],
         [99,65, 4,28, 6,16,70,92],
         [41,41,26,56,83,40,80,70,33],
         [41,48,72,33,47,32,37,16,94,29],
         [53,71,44,65,25,43,91,52,97,51,14],
         [70,11,33,28,77,73,17,78,39,68,17,57],
         [91,71,52,38,17,14,91,43,58,50,27,29,48],
         [63,66, 4,68,89,53,67,30,73,16,69,87,40,31],
         [ 4,62,98,27,23, 9,70,98,73,93,38,53,60, 4,23]].

%%
%% Using nb_setval for the global max value (sorry about that).
%% 0.06s
%%
euler18a :-
        abolish_all_tables,
        p18(Tri),
        nb_setval(maxval, 0),
        matrix_element5(Tri, 1, 1, Tri11),
        pp(1, 1, Tri11, Tri),
        nb_getval(maxval,MaxVal),
        writeln(MaxVal).
        
% :- table pp/4. % It's much slower with tabling: 0.38s
pp(Row, Column, Sum, Tri) :-
        nb_getval(maxval,MaxVal),
        (Sum > MaxVal
        ->
         nb_setval(maxval,Sum)
        ;
         true
        ),
        Row1 is Row + 1,
        length(Tri,TriLen),
        (Row1 =< TriLen
        ->
         findall(_,
                 (between(0,1,I),
                  ColumnI is Column+I,
                  matrix_element5(Tri,Row1,ColumnI,TriRC1),
                  SumT is Sum+TriRC1,
                  pp(Row1,ColumnI, SumT, Tri)
                 ),
                 _)
        ;
        true
        ).
        
%%
%% (Idea from Neng-Fa Zhou.)
%%
%% NOTE: The tabling (+,+,+,max) does not work (as I expect).
%% Perhaps it will be fixed in version 8.1.14.
%%
%% Without tabling
%% ?- make, euler18xxx.
%% 794
%% true ;
%% 794
%% true ;
%% 852
%%
euler18b :-
        abolish_all_tables,
        p18(Tri),
        pp2(1,1,Tri,Sum),
        writeln(Sum).

:- table pp2(+,+,+,max).
pp2(Row,_Column,Tri,Sum) :-
        length(Tri,Len),
        Row > Len,
        Sum = 0.
pp2(Row,Column,Tri,Sum) :-
        length(Tri,Len),
        Row =< Len,
        Row1 is Row+1,
        pp2(Row1,Column,Tri,Sum1),
        matrix_element5(Tri,Row,Column,TriRC),
        Sum is Sum1+TriRC.
pp2(Row,Column,Tri,Sum) :-
        length(Tri,Len),
        Row =< Len,        
        Row1 is Row+1, 
        Column1 is Column+1,
        pp2(Row1,Column1,Tri,Sum1),
        matrix_element5(Tri,Row,Column,TriRC),
        Sum is Sum1+TriRC.

