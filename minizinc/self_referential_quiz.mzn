% 
% Self Referencing Quiz puzzle in MiniZinc.
% 
% See:
% http://www.lcc.uma.es/~afdez/srq/
%
%
% Model and description from GProlog ExamplesFD/srq.pl
% """
% Name           : srq.pl                                                 
% Title          : Self-Referential Quiz puzzle                           
% Original Source: M. Henz                                                
% Adapted by     : Daniel Diaz for GNU Prolog                             
% Date           : February 1997                                          
%                                                                         
%                                                                         
% Q1 : the first question whose answer is A is                            
%      (A) 4   (B) 3   (C) 2   (D) 1   (E) none of the above          
%    
% Q2 : the only two consecutive questions with identical answers are      
%      (A) 3 and 4   (B) 4 and 5   (C) 5 and 6   (D) 6 and 7   (E) 7 and 8
%
% Q3 : the next question with answer A is                                 
%      (A) 4   (B) 5   (C) 6   (D) 7   (E) 8                              
%
% Q4 : the first even numbered question with answer B is                  
%      (A) 2   (B) 4   (C) 6   (D) 8   (E) 10                             
%
% Q5 : the only odd numbered question with answer C is                    
%      (A) 1   (B) 3   (C) 5   (D) 7   (E) 9                              
%
% Q6 : a question with answer D                                           
%      (A) comes before this one but not after this one                   
%      (B) comes after this one but not before this one                   
%      (C) comes before and after this one                                
%      (D) does not occur at all                                          
%      (E) none of the above                                              
%
% Q7 : the last question whose answer is E is                             
%      (A) 5   (B) 6   (C) 7   (D) 8   (E) 9                              
%
% Q8 : the number of questions whose answers are conconants is            
%      (A) 7   (B) 6   (C) 5   (D) 4   (E) 3                              
%
% Q9 : the number of questions whose answers are vowels is                
%      (A) 0   (B) 1   (C) 2   (D) 3   (E) 4                              
%
% Q10: the answer of this question is                                     
%      (A) A   (B) B   (C) C   (D) D   (E) E                              
%                                                                         
% Solution:                                                               
% [3,1,2,2,1,2,5,2,5,4]                                                   
%  C,A,B,B,A,B,E,B,E,D                                                    

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


set of 1..5: Ans = 1..5;
var Ans: Q1;
var Ans: Q2;
var Ans: Q3;
var Ans: Q4;
var Ans: Q5;
var Ans: Q6;
var Ans: Q7;
var Ans: Q8;
var Ans: Q9;
var Ans: Q10;

var bool: BeforeQ4;
var bool: AfterQ4;
var bool: BCD1;
var bool: BCD2;
var bool: BCD3;
var bool: BCD4;
var bool: BCD5;
var bool: BCD6;
var bool: BCD7;
var bool: BCD8;
var bool: BCD9;
var bool: BCD10;

var 0..10: BCD;
var 0..10: AE;

var bool: AE1;
var bool: AE2;
var bool: AE3;
var bool: AE4;
var bool: AE5;
var bool: AE6;
var bool: AE7;
var bool: AE8;
var bool: AE9;
var bool: AE10;


array[1..10] of var Ans: LD = [Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10];

solve :: int_search(LD, first_fail, indomain_min, complete) satisfy;

constraint

        %
        % The two commented lines was commented in the GProlog source.
        %
	(Q1=1 <-> (Q4=1 /\ Q1 != 1 /\ Q2 != 1 /\ Q3 != 1)) /\
	(Q1=2 <-> (Q3=1 /\ Q1 != 1 /\ Q2 != 1)) /\
	(Q1=3 <-> (Q2=1 /\ Q1 != 1)) /\
	(Q1=4 <-> Q1=1) /\
%	(Q1=5 <-> (Q1 != 1 /\ Q2 != 1 /\ Q3 != 1 /\ Q4 != 1)) /\


	(Q2=1 <-> Q3=Q4) /\
	(Q2=2 <-> Q4=Q5) /\
	(Q2=3 <-> Q5=Q6) /\
	(Q2=4 <-> Q6=Q7) /\
	(Q2=5 <-> Q7=Q8) /\


	(Q3=1 <-> Q4=1) /\
	(Q3=2 <-> (Q5=1 /\ Q4 != 1)) /\
	(Q3=3 <-> (Q6=1 /\ Q4 != 1 /\ Q5 != 1)) /\
	(Q3=4 <-> (Q7=1 /\ Q4 != 1 /\ Q5 != 1 /\ Q6 != 1)) /\
	(Q3=5 <-> (Q8=1 /\ Q4 != 1 /\ Q5 != 1 /\ Q6 != 1 /\ Q7 != 1)) /\


	(Q4=1 <-> Q2=2) /\
	(Q4=2 <-> (Q4=2 /\ Q2 != 2)) /\
	(Q4=3 <-> (Q6=2 /\ Q2 != 2 /\ Q4 != 2)) /\
	(Q4=4 <-> (Q8=2 /\ Q2 != 2 /\ Q4 != 2 /\ Q6 != 2)) /\
	(Q4=5 <-> (Q10=2 /\ Q2 != 2 /\ Q4 != 2 /\ Q6 != 2 /\ Q8 != 2)) /\


	(Q5=1 <-> (Q1=3 /\ Q3 != 3 /\ Q5 != 3 /\ Q7 != 3 /\ Q9 != 3)) /\
	(Q5=2 <-> (Q3=3 /\ Q1 != 3 /\ Q5 != 3 /\ Q7 != 3 /\ Q9 != 3)) /\
	(Q5=3 <-> (Q5=3 /\ Q1 != 3 /\ Q3 != 3 /\ Q7 != 3 /\ Q9 != 3)) /\
	(Q5=4 <-> (Q7=3 /\ Q1 != 3 /\ Q2 != 3 /\ Q5 != 3 /\ Q9 != 3)) /\
	(Q5=5 <-> (Q9=3 /\ Q1 != 3 /\ Q3 != 3 /\ Q5 != 3 /\ Q7 != 3)) /\


	(BeforeQ4 <-> (Q1=4 \/ Q2=4 \/ Q3=4 \/ Q4=4 \/ Q5=4)) /\
	(AfterQ4  <-> (Q7=4 \/ Q8=4 \/ Q9=4 \/ Q10=4)) /\

	(Q6=1 <-> (BeforeQ4 /\ not AfterQ4)) /\
	(Q6=2 <-> (not BeforeQ4 /\ AfterQ4)) /\
	(Q6=3 <-> (BeforeQ4 /\ AfterQ4)) /\
	(Q6=4 <-> (Q1 != 4 /\ Q2 != 4 /\ Q3 != 4 /\ Q4 != 4 /\ Q5 != 4 /\ 
                   Q6 != 4 /\ Q7 != 4 /\ Q8 != 4 /\ Q9 != 4 /\ Q10 != 4)) /\
%	(Q6=5 <-> Q6=4) /\


	(Q7=1 <-> (Q5=5 /\ Q6 != 5 /\ Q7 != 5 /\ Q8 != 5 /\ Q9 != 5 /\ 
                   Q10 != 5)) /\
	(Q7=2 <-> (Q6=5 /\ Q7 != 5 /\ Q8 != 5 /\ Q9 != 5 /\ Q10 != 5)) /\
	(Q7=3 <-> (Q7=5 /\ Q8 != 5 /\ Q9 != 5 /\ Q10 != 5)) /\
	(Q7=4 <-> (Q8=5 /\ Q9 != 5 /\ Q10 != 5)) /\
        (Q7=5 <-> (Q9=5 /\ Q10 != 5)) /\

        % note the extensive grouping...
        ((BCD1 <-> (Q1 >= 2 /\ Q1 <= 4)) /\ (AE1 <-> not BCD1)) /\
        ((BCD2 <-> (Q2 >= 2 /\ Q2 <= 4)) /\ (AE2 <-> not BCD2)) /\
        ((BCD3 <-> (Q3 >= 2 /\ Q3 <= 4)) /\ (AE3 <-> not BCD3)) /\
        ((BCD4 <-> (Q4 >= 2 /\ Q4 <= 4)) /\ (AE4 <-> not BCD4)) /\
        ((BCD5 <-> (Q5 >= 2 /\ Q5 <= 4)) /\ (AE5 <-> not BCD5)) /\
        ((BCD6 <-> (Q6 >= 2 /\ Q6 <= 4)) /\ (AE6 <-> not BCD6)) /\
        ((BCD7 <-> (Q7 >= 2 /\ Q7 <= 4)) /\ (AE7 <-> not BCD7)) /\
        ((BCD8 <-> (Q8 >= 2 /\ Q8 <= 4)) /\ (AE8 <-> not BCD8)) /\
        ((BCD9 <-> (Q9 >= 2 /\ Q9 <= 4)) /\ (AE9 <-> not BCD9)) /\
        ((BCD10<-> (Q10 >= 2 /\ Q10 <= 4)) /\ (AE10 <-> not BCD10)) /\

	(BCD=bool2int(BCD1)+bool2int(BCD2)+bool2int(BCD3)+bool2int(BCD4)+bool2int(BCD5)+bool2int(BCD6)+bool2int(BCD7)+bool2int(BCD8)+bool2int(BCD9)+bool2int(BCD10)) /\

	(AE=bool2int(AE1)+bool2int(AE2)+bool2int(AE3)+bool2int(AE4)+bool2int(AE5)+bool2int(AE6)+bool2int(AE7)+bool2int(AE8)+bool2int(AE9)+bool2int(AE10)) /\


	(Q8=1 <-> BCD=7) /\
	(Q8=2 <-> BCD=6) /\
	(Q8=3 <-> BCD=5) /\
	(Q8=4 <-> BCD=4) /\
        (Q8=5 <-> BCD=3) /\

	(Q9=1 <-> AE=0) /\
	(Q9=2 <-> AE=1) /\
	(Q9=3 <-> AE=2) /\
	(Q9=4 <-> AE=3) /\
	(Q9=5 <-> AE=4)

;


output [
 "Answers: ", show(LD), "\n"
]
;