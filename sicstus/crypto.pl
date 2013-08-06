/*

  Crypto problem (alphametic problem) in SICStus Prolog.

  This is a standard alphametic problem in mathematical recreations, 
  constraint programming etc.
    
  From GLPK:s model cryto.mod.
 
  """
     This problem comes from the newsgroup rec.puzzle.
     The numbers from 1 to 26 are assigned to the letters of the alphabet.
     The numbers beside each word are the total of the values assigned to
     the letters in the word (e.g. for LYRE: L, Y, R, E might be to equal
     5, 9, 20 and 13, or any other combination that add up to 47).
     Find the value of each letter under the equations:
 
     BALLET  45     GLEE  66     POLKA      59     SONG     61
     CELLO   43     JAZZ  58     QUARTET    50     SOPRANO  82
     CONCERT 74     LYRE  47     SAXOPHONE 134     THEME    72
     FLUTE   30     OBOE  53     SCALE      51     VIOLIN  100
     FUGUE   50     OPERA 65     SOLO       37     WALTZ    34
 
     Solution:
     A, B,C, D, E,F, G, H, I, J, K,L,M, N, O, P,Q, R, S,T,U, V,W, X, Y, Z
     5,13,9,16,20,4,24,21,25,17,23,2,8,12,10,19,7,11,15,3,1,26,6,22,14,18
 
     Reference:
     Koalog Constraint Solver <http://www.koalog.com/php/jcs.php>,
     Simple problems, the crypto-arithmetic puzzle ALPHACIPHER.
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/crypto.mzn
              http://www.hakank.org/minizinc/crypto_ip.mzn
  * Comet   : http://www.hakank.org/comet/crypto.co
  * Gecode/R: http://www.hakank.org/gecode_r/alpha.rb
  * ECLiPSe : http://www.hakank.org/eclipse/crypto.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        BALLET     =  45,
        CELLO      =  43,
        CONCERT    =  74,
        FLUTE      =  30,
        FUGUE      =  50,
        GLEE       =  66,
        JAZZ       =  58,
        LYRE       =  47,
        OBOE       =  53,
        OPERA      =  65,
        POLKA      =  59,
        QUARTET    =  50,
        SAXOPHONE  = 134,
        SCALE      =  51,
        SOLO       =  37,
        SONG       =  61,
        SOPRANO    =  82,
        THEME      =  72,
        VIOLIN     = 100,
        WALTZ      =  34,

        % note: D is not in any constraint
        LD = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z],
        domain(LD,1,26),

        all_different(LD),

            B + A + L + L + E + T #= BALLET,
                C + E + L + L + O #= CELLO,
        C + O + N + C + E + R + T #= CONCERT,
                F + L + U + T + E #= FLUTE,
                F + U + G + U + E #= FUGUE,
                    G + L + E + E #= GLEE,
                    J + A + Z + Z #= JAZZ,
                    L + Y + R + E #= LYRE,
                    O + B + O + E #= OBOE,
                O + P + E + R + A #= OPERA,
                P + O + L + K + A #= POLKA,
        Q + U + A + R + T + E + T #= QUARTET,
S + A + X + O + P + H + O + N + E #= SAXOPHONE,
                S + C + A + L + E #= SCALE,
                    S + O + L + O #= SOLO,
                    S + O + N + G #= SONG,
        S + O + P + R + A + N + O #= SOPRANO,
                T + H + E + M + E #= THEME,
            V + I + O + L + I + N #= VIOLIN,
                W + A + L + T + Z #= WALTZ,


        labeling([ffc,bisect,up],LD),
        Alpha = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],
        ( foreach(Char, Alpha), 
          foreach(Val,LD) do 
              format("~w: ~d\n", [Char,Val])
        ),
        fd_statistics.
