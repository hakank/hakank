/*

  Twin letters problem in ECLiPSe.

  From
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  Twin Letters    

  In the following puzzle, there are ten pairs of
  letters to be assigned to the same digit so that the multiplication
  (including intermediate results) is correct. Can you find out the
  pairs and their values?

          A B C
   *      D E F
   ____________
          G H I
        J K L
      M N O
   ____________
      P Q R S T



Q=R=0, D=H=1, A=B=2, J=M=3, C=P=4, 
K=N=5, I=T=6, E=G=7, L=O=8, F=S=9

224 * 179
_________
      716
     358
    358
_________
    40096 



Min lösning:

[a : 2, b : 2, c : 4, d : 1, e : 7, f : 9, g : 7, h : 1, i : 6, j : 3]
[k : 5, l : 8, m : 3, n : 5, o : 8, p : 4, q : 0, r : 0, s : 9, t : 6]

vilket alltså är:
q=r=0, d=h=1, a=b=2, j=m=3, c=p=4
k=n=5, i=t=6, e=g=7, l=o=8, f=s=9



  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic), lib(ic_global).

go :-

        twin([A, B, C, D, E, F, G, H, I, J, K, 
                      L, M, N, O, P, Q, R, S, T]), 
        writeln([a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J]),
        writeln([k:K,l:L,m:M,n:N,o:O,p:P,q:Q,r:R,s:S,t:T]).


twin(LD) :-
        LD = [A, B, C, D, E, F, G, H, I, J,
              K, L, M, N, O, P, Q, R, S, T],
        LD :: 0..9,
        C1 :: 0..1,
        C2 :: 0..2,
        C3 :: 0..1,

        % exact 2 occurrences of each digit
        ( for(I, 0, 9), param(LD) do
            occurrences(I,LD,2)        
        ), 

        100*G + 10*H + I +
        1000*J + 100*K + 10*L +
        10000*M + 1000*N + 100*O #=
        10000*P + 1000*Q + 100*R + 10*S + T,
        
        (100*D + 10*E + F)*C #= 100*G + 10*H + I,
        (100*D + 10*E + F)*B #= 100*J + 10*K + L,
        (100*D + 10*E + F)*A #= 100*M + 10*N + O,
        
        (100*A + 10*B + C) * (100*D + 10*E + F) #=
        10000*P + 1000*Q + 100*R + 10*S + T,

        % Carry restrictions
        T         #= I,
        S + 10*C1 #= H + L,
        R + 10*C2 #= G + K + O + C1,
        Q + 10*C3 #= J + N + C2,
        P         #= M + C3,

        search(LD, 0, first_fail, indomain, complete, []),
        search([C1,C2,C3], 0, first_fail, indomain, complete, []).
