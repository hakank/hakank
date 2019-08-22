/*

  Fill-in the squares problem (Brainjammer) in SWI Prolog

  This problem is from the ZDC system, available from 
  http://www.bracil.net/CSP/cacp/cacpdemo.html , in the
  file 
     Brainjammer.txt 
  from 2003-01-26, which states:
  """
  Only Solution is:
        1	2	3	4	5
  ====================================================
  A  	 7	11	2	17	1
  B	13	19	23	22	3
  C	9	20	24	14	12
  D	16	21	25	18	10
  E	4	8	15	6	5

  22mins55secs of CPU time to find first solution
  50mins42secs of CPU time with duplicate induced variables removed?
  Maybe this has something to do with the variable ordering...as this might change
  as a result of removing duplicate induced variables.
  1hr:34 mins of CPU time to find a single solution and determine no other solutions
  exist.

  Statistics for finding the first solution:
  (with duplicate induced nodes removed)
  CPU seconds: 		4880.63	(On a Pentium Pro 200Mhz, VC++)
  Node count:			4036162
  Induced node count:	1849214
  Backtracks:			5885311
  """

  Notes:
  - On my 8 core 2.8 Mhz (Linux Ubuntu) it takes about 0.02 seconds 
    and 0 backtracks to solve this problem (include proving the 
    uniqueness of the solution), but the comparison is really 
    not fair considering the difference in machines then and now.

  - The only references to this problem I've found are the following pages:
    http://discuss.fogcreek.com/techInterview/default.asp?cmd=show&ixPost=2787
    http://notdarkandstormy.blogspot.com/2005/05/funky-logic-problem.html
    and especially 
    http://perplexus.info/show.php?pid=2683
    which has a lot of comments about manually solving the problem.

  I've yet to know the original source.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        time(problem(_)),
        fail,
        nl.
go.        

go1 :-
   time(findall(_,problem(_),_)).



problem(ALL) :-

   N = 5,

   N2 #= N^2,
   length(A,N), A ins 1..N2,
   A = [A1,_A2,A3,A4,A5], %% A2 is not used
   length(B,N), B ins 1..N2,
   B = [B1,B2,B3,B4,B5],
   length(C,N), C ins 1..N2,
   C = [C1,C2,C3,C4,C5],
   length(D,N), D ins 1..N2,
   D = [D1,D2,D3,D4,D5],
   length(E,N), E ins 1..N2,
   E = [E1,E2,E3,E4,E5],   

   sum(A,#=,ASum),
   sum(B,#=,BSum),
   sum(C,#=,CSum),
   sum(D,#=,DSum),
   sum(E,#=,ESum),

   % Each number from 1-25, used only once
   flatten([A,B,C,D,E],ALL),
   all_different(ALL),

   
   % 1. Sum of each column is odd
   maplist(odd_sum,A,B,C,D,E),
      
   % 2. Sum of each row, except C, is even
   ASum mod 2 #= 0,
   BSum mod 2 #= 0,
   CSum mod 2 #= 1,
   DSum mod 2 #= 0,
   ESum mod 2 #= 0,
   
   % 3. Sum of row A is not greater than the sum of any other row
   ASum #=< BSum,
   ASum #=< CSum,
   ASum #=< DSum,
   ASum #=< ESum,
  
   % 4. The sum of diagonal A1 to E5 is greater than the sum of
   %  diagonal E1 to A5
   A1 + B2 + C3 + D4 + E5  #> E1 + D2 + C3 + B4 + A5,
  
   % 5. (A4 + B4) is greater than (C4+D4+E4)
   A4 + B4 #> C4 + D4 + E4,
  
   % 6. A1 + B1 = D1 + E1
   A1 + B1 #= D1 + E1,
    
   % 7. A1 > E1
   A1 #> E1,
  
   % 8. A1, A3 and B1 are primes
   is_prime(A1),
   is_prime(A3),
   is_prime(B1),

   % 9. (A3 + E3) is a prime number
   A3E3 #= A3+E3,
   is_prime(A3E3),


   % 10. A5,D1,D3 and E1 are squares
   is_square(A5),
   is_square(D1),
   is_square(D3),
   is_square(E1),

   % 11. B2, C2, and D2 are ascending consecutive numbers
   B2 + 1 #= C2,
   C2 + 1 #= D2,
  
   % 12. B3, C3, and D3 are ascending consecutive numbers
   B3 + 1 #= C3,
   C3 + 1 #= D3,
   
   % 13. B5 + D5 = A5 + C5
   B5 + D5 #= A5 + C5,

   % 14. (c1)^2 + (c5)^2 = (e3)^2
   C1s #= C1*C1,
   C5s #= C5*C5,
   E3s #= E3*E3,
   C1s + C5s #= E3s,

   % 15. C5 is a two-digit number
   C5 #> 9,
   
   % 16. D5 is a multiple of E5
   D5 mod E5 #= 0,
         
   % 17. E1 + E3 = E2 + E4 + E5
   E1 + E3 #= E2 + E4 + E5,
   
   labeling([ffc,enum],ALL),

   writeln(a=A),
   writeln(b=B),
   writeln(c=C),
   writeln(d=D),
   writeln(e=E),
   nl.


is_prime(V) :-
   member(V, [2,3,5,7,11,13,17,19,23]).

is_square(V) :-
   member(V, [1,4,9,16,25]).

%%
%% sum of each column is odd
%%
odd_sum(A,B,C,D,E) :-
        (A+B+C+D+E) mod 2 #= 1.  
