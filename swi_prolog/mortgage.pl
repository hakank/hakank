/*

  Mortgage experiments (using clpr) in SWI Prolog

  Marriot & Stuckey "Programming with Constraints", page 175f
  And some other sources, see below.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpr)).

%% Marriott & Stuckey
go :-
        T = 3.0,
        I = 0.1, % 10.0/100.0,
        R = 150.0,
        B = 0.0,
        mortgage(P,T,I,R,B),
        writeln([p=P,t=T,i=I,r=R,b=B]),
        nl.

%% An older ECLiPSe example
go2 :- 
        T = 3.0,
        I = 0.1, % 10/100,
        B = 150.0,
        MP = 0.0,
        mg(P,T,I,B,MP),
        writeln([p=P,t=T,i=I,b=B,mp=MP]),
        nl.


%% From Thom Frühwirth
go3 :-
        mortgage3(100000,360,0.01,1025,S1),
        writeln(s1=S1),
                
        mortgage3(D2,360,0.01,1025,0),
        writeln(d2=D2),

        mortgage3(100000,T3,0.01,1025,S3), S3 =<0,
        writeln([t3=T3,s3=S3]),

        % don't work
        % mortgage3(D4,360,0.01,R4,0),
        % writeln([d4=D4,r4=R4]),
        nl.


%
% Marriott & Stuckey, page 178
% 
mortgage(P,T,I,R,B) :-
        {T  >= 1.0,
        NP = P + (P * I) - R,
        NT = T-1},
        mortgage(NP, NT, I, R, B).

mortgage(P,T,_,_,B):-
        {T = 0, B = P}.



%
% From an older ECLiPSe example, eclipse/lib_noncom/clpqr/examples/mg.pl
% 
mg(P,T,I,B,MP) :-
        {T = 1,  B + MP = P * (1 + I)}.


mg(P,T,I,B,MP) :-
        {
        T  > 1,
        P1 = P * (1 + I) - MP,
        T1 = T - 1},
	mg(P1, T1, I, B, MP).




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
        {T = 0,D = S}
        ;
        {T > 0,
         T1 = T - 1,
         D1 = D + D*I - R
        },
        mortgage3(D1, T1, I, R, S).


mortgage3(D, T, I, R, S) :-
        {T = 0, D = S}
        ;
        {T > 0, T1 = T - 1, D1 = D + D*I - R},
        mortgage3(D1, T1, I, R, S).
