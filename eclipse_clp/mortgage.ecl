/*

  Mortgage experiment in ECLiPSe.
  
  Marriot & Stuckey "Programming with Constraints", page 175f
  And some other sources, see below.


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/


*/

:- lib(ic). 

w(X) :- write(X),nl.

%
% Marriott & Stuckey, page 178
% 
mortgage(P,T,I,R,B) :-
        T  $>= 1.0,
        NP $= P + (P * I) - R,
        NT $= T-1,
        mortgage(NP, NT, I, R, B).

mortgage(P,T,_,_,B):-
        T $= 0,
        B $= P.


go :-
        LD = [P,T,I,R,B],
        LD :: 0.0..10000,
        T $= 3.0,
        I $= 10/100,
        R $= 150.0,
        B $= 0.0,
        mortgage(P,T,I,R,B),
        write([p=P,t=T,i=I,r=R,b=B]), nl.


%
% From and older ECLiPSe example, eclipse/lib_noncom/clpqr/examples/mg.pl
% 
mg(P,T,I,B,MP) :-
        T $= 1, 
        B + MP $= P * (1 + I)
        .

mg(P,T,I,B,MP) :-
        T  $> 1,
        P1 $= P * (1 + I) - MP,
        T1 $= T - 1,
	mg(P1, T1, I, B, MP).



go2 :- 
        LD = [P,T,I,B,MP],
        LD :: 0.0..10000, % ska inte vara med i clpr
        T $= 3.0,
        I $= 10/100,
        B $= 150.0,
        MP = 0.0,
        mg(P,T,I,B,MP),
        write([p=P,t=T,i=I,b=B,mp=MP]), nl.


%
% From Thom Frühwirth
%
% http://www.informatik.uni-ulm.de/pm/fileadmin/pm/home/fruehwirth/Papers/cp-intro.pdf 
% page 7
% D: Amount of Loan, Debt, Principal
% T: Duration of loan in months
% I: Interest rate per month
% R: Rate of payments per month
% S: Balance of debt after T months
%
mortgage3(D, T, I, R, S) :-
        T $= 0,       
        D $= S
        ;
        T $> 0,
        T1 $= T - 1,
        D1 $= D + D*I - R,
        mortgage3(D1, T1, I, R, S).


mortgage3(D, T, I, R, S) :-
        T $= 0, D $= S
        ;
        T $> 0, T1 $= T - 1, D1 $= D + D*I - R,
        mortgage3(D1, T1, I, R, S).


% this seems to work
go3 :-
        mortgage3(100000,360,0.01,1025,S1),
        w(S1),

        mortgage3(D2,360,0.01,1025,0),
        w(D2),

        mortgage3(100000,T3,0.01,1025,S3), S3 $=<0,
        w([T3,S3])%,

        % don't work
        % mortgage3(D4,360,0.01,R4,0),
        % w([D4,R4])
        .


%
% keep T variable
% Don't work: throws stack overflow...
% 
go4 :-
        LD = [P,T,I,R,B],
        LD :: 0.0..10000,
        % T $= 3.0,
        I $= 10/100,
        R $= 150.0,
        B $= 0.0,
        P $= 373.027798647633,
        mortgage(P,T,I,R,B),
        write([p=P,t=T,i=I,r=R,b=B]), nl.


