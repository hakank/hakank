/*

  Calculs d'enfer puzzle in SWI Prolog.

  Problem from Jianyang Zhou "The Manual of NCL version 1.2", page 33
  http://citeseer.ist.psu.edu/161721.html
  
  The solution is the manual is:
  """
  a = -16, b = -14, c = -13, d = -12, e = -10,
  f = 4, g = 13, h = -1, i = -3, j = -11, k = -9,
  l = 16, m = -8, n = 11, o = 0, p = -6, q = -4,
  r = 15, s = 2, t = 9, u = -15, v = 14, w = -7,
  x = 7, y = -2, z = -5.
 
  max_{#1\in [1,26]}{|x_{#1}|} minimized to 16
  """
 
  Also, see the discussion of the Z model:
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/calculs_enfer/calculs_enfer.ps
  (which shows the same solution).


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


go :-
        time(once(calculs_d_enfer(AA,Amax))),
        writeln(amax=Amax),
        writeln(AA).

go2 :-
        time(once(calculs_d_enfer2(AA,Amax))),
        writeln(amax=Amax),
        writeln(AA).


calculs_d_enfer(AA,Amax) :-

        NN #= 26,

        AA = [_A,_B,_C,_D,E,F,G,H,I,_J,_K,L,_M,N,O,_P,_Q,R,S,T,U,V,W,X,_Y,Z],
        AA ins -100..100,
        
        % The objective is to minimize the maximum of the absolute 
        % values of [I]
        length(Aabs,NN),
        Aabs ins 0..100,

        %% Aabs is the abs value of AA
        maplist(abs2,AA,Aabs),
        
        % we want to minimize the maximum value in AA
        Amax in 0..NN,
        max_list_clp(Aabs,Amax),
        
        all_different(AA),

        Z+E+R+O     #= 0,
        O+N+E       #= 1,
        T+W+O       #= 2,
        T+H+R+E+E   #= 3,
        F+O+U+R     #= 4,
        F+I+V+E     #= 5,
        S+I+X       #= 6,
        S+E+V+E+N   #= 7,
        E+I+G+H+T   #= 8,
        N+I+N+E     #= 9,
        T+E+N       #= 10,
        E+L+E+V+E+N #= 11,
        T+W+E+L+F   #= 12,
        
        % search
        labeling([min(Amax),ff,down,bisect], AA).

%%
%% Using sum() instead
%%
calculs_d_enfer2(AA,Amax) :-

        NN #= 26,

        AA = [_A,_B,_C,_D,E,F,G,H,I,_J,_K,L,_M,N,O,_P,_Q,R,S,T,U,V,W,X,_Y,Z],
        AA ins -100..100,
        
        length(Aabs,NN),
        Aabs ins 0..100,

        %% Aabs is the abs value of AA
        maplist(abs2,AA,Aabs),
        
        % we want to minimize the maximum value in AA
        Amax in 0..NN,
        max_list_clp(Aabs,Amax),
       
        all_different(AA),

        sum([Z,E,R,O],     #=,  0),
        sum([O,N,E],       #=,  1),
        sum([T,W,O],       #=,  2),
        sum([T,H,R,E,E],   #=,  3),
        sum([F,O,U,R],     #=,  4),
        sum([F,I,V,E],     #=,  5),
        sum([S,I,X],       #=,  6),
        sum([S,E,V,E,N],   #=,  7),
        sum([E,I,G,H,T],   #=,  8),
        sum([N,I,N,E],     #=,  9),
        sum([T,E,N],       #=, 10),
        sum([E,L,E,V,E,N], #=, 11),
        sum([T,W,E,L,F],   #=, 12),
        
        labeling([min(Amax),ff,down,bisect], AA).

%%
%% Abs is the absolute value of A (for maplist).
%%
abs2(A,Abs) :- Abs #= abs(A).
