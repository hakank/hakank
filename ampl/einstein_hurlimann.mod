/*
   From Hurlimann

   LPL model einstein ;
  set a := /Norwegian, Dane, Briton, Swede, German/;
      b := /yellow blue red green white/;
      c := /cat bird dog horse fish/;
      d := /coffee tea milk juice water/;
      e := /Dunhill Marlboro Pall-Mall Bluemaster Prince/;
  distinct variable
     A{a}[1,5]; B{b}[1,5]; C{c}[1,5]; D{d}[1,5]; E{e}[1,5];
  constraint
     R1:  A(3)=B(3);
     R2:  A(4)=C(3);
     R3:  A(2)=D(2);
     R4:  B(4)+1=B(5);
     R5:  B(4)=D(1);
     R6:  E(3)=C(2);
     R7:  D(3)=3;
     R8:  B(1)=E(1);
     R9:  A(1)=1;
     R10: E(2)=C(1)+1 or E(2)=C(1)-1;
     R11: C(4)=E(1)+1 or C(4)=E(1)-1;
     R12: E(4)=D(4);
     R13: B(2)=2;
     R14: A(5)=E(5);
     R15: E(2)=D(5)+1 or E(2)=D(5)-1;
  minimize obj: A(1);
  for{a} do
   if A[a]=C[5] then write 'The %s owns the fish\n':a; end;
  end;
end

Problem formulation (talk version):

   1. There are 5 houses in five different colors.
   2. In each house lives a person with a different nationality.
   3. These 5 owners drink a certain drink, smoke a certain brand of to-
      bacco and keep a certain pet.
   4. No owner has the same pet, smokes the same tobacco, or drinks the
      same drink.

The question is: Who owns the fish supposing the following 15 conditions
hold?

   1. The Briton lives in the red house
   2. The Swede keeps dogs as pets
   3. The Dane drinks tea
   4. The green house is adjacent on the left of the white house
   5. The green house owner drinks coffee
   6. The person who smokes Pall-Mall raises birds
   7. The owner of the yellow house smokes Dunhill
   8. The man living in the house right in the center drinks milk
   9. The Norwegian lives in the first house
 10. The man who smokes Marlboros [Blend] lives next to the one who keeps cats
 11. The man who keeps horses lives next to the one who smokes Dunhill
 12. The owner who smokes Bluemaster drinks juice [beer]
 13. The German smokes Prince
 14. The Norwegian lives next to the blue house
 15. The man who smokes Marlboro [Blend] has a neighbor who drinks water.


Solution:
The German owns the fish.

According to http://www.naute.com/puzzles/puzzle13.phtml
this should be the solution matrix:
house        1              2          3            4          5
color        yellow(1)      blue(2)    red(3)       green(4)   white(5)
nationality  norweigian(1)  dane(2)    brit(3)      german(5)  swede(4)
drink        water(5)       tea(2)     milk(3)      coffee(1)  beer(4)
smoke        dunhill(1)     blend(2)   pall-mall(3) prince(5)  bluemaster(4)
pet          cats(1)        horse(4)   birds(2)     <fish>(5)  dogs(3)

i.e.
color: 1 2 3 4 5
nat  : 1 2 3 5 4
drink: 5 2 3 1 4
smoke: 1 2 3 5 4
pet  : 1 4 2 5 3

Comments:
 * The alldiff-things was quite easy
 * det som stökar är OR-konstruktionen i de tre constrainterna
 * the more troublesome thing is the OR construct with the three
   constraint
 * and the display of the matrix was also somewhat tricky. I would like 
   to use
      i in 1..5
   and the print what's in a[A[i]].

My matrix is:

The German owns the fish (4)
nationality:    Norwegian(1)  Dane(2)  Briton(3)  Swede(5)  German(4)
color:          yellow(1)  blue(2)  red(3)  green(4)  white(5)
pet:            cat(1)  bird(3)  dog(5)  horse(2)  fish(4)
drinks:         coffee(4)  tea(2)  milk(3)  juice(5)  water(1)
smokes          Dunhill(1)  Marlboro(2)  Pall-Mall(3)  Bluemaster(5)  Prince(4)

-> 
House             1 2 3 4 5
nationality     Norweigan Dane     Briton    German Swede
color           yellow    blue     red       green  white 
pet             cat       horse    bird      fish   dog
drinks          water     tea      milk      coffee juice
smokes          dunhill   marlboro pall-mall prince bluemaster

where beer  <-> juice
      blend <-> marlboro

OK, this seems to be correct.

This model can be compared to zebra.mod (which is very different).

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

set a := {'Norwegian', 'Dane', 'Briton', 'Swede', 'German'};
set b := {'yellow', 'blue', 'red', 'green', 'white'};
set c := {'cat', 'bird', 'dog', 'horse', 'fish'};
set d := {'coffee', 'tea', 'milk', 'juice', 'water'};
set e := {'Dunhill', 'Marlboro','Pall-Mall', 'Bluemaster', 'Prince'};

#  distinct variable!
var A{a} integer >= 1 <= 5; 
var B{b} integer >= 1 <= 5; 
var C{c} integer >= 1 <= 5; 
var D{d} integer >= 1 <= 5; 
var E{e} integer >= 1 <= 5;

# for alldiff of A..E
var yA{a,a} binary;
var yB{b,b} binary;
var yC{c,c} binary;
var yD{d,d} binary;
var yE{e,e} binary;

# for the OR constraints
var yR10 binary;
var yR11 binary;
var yR15 binary;

param M := 6;  # for alldifferent variants
param M2 := 6; # for the OR construction of constraints
param matrix{1..5, 1..5} symbolic;

s.t.
     R1: A['Briton']    = B['red'];     #A[3]=B[3];
     R2: A['Swede']     = C['dog'];     #A[4]=C[3];
     R3: A['Dane']      = D['tea'];     #A[2]=D[2];
     R4: B['green']+1   = B['white'];   #B[4]+1=B[5];
     R5: B['green']     = D['coffee'];  #B[4]=D[1];
     R6: E['Pall-Mall'] = C['bird'];    #E[3]=C[2];
     R7: D['milk']      = 3;            #D[3]=3;
     R8: B['yellow']    = E['Dunhill']; #B[1]=E[1];
     R9: A['Norwegian'] = 1;            #A[1]=1;

     # an or!
     ### R10: E['Marlboro'] = C['cat'] + 1;# or E['Marlboro'] = C['cat'] - 1; #E[2]=C[1]+1 or E[2]=C[1]-1;
     R10a: E['Marlboro'] - C['cat']  <= 1 +  M2 * yR10; 
     R10b: E['Marlboro'] - C['cat']  <=      M2 * (1-yR10); 

     # an or!
     ###R11: C['horse']    = E['Dunhill'] + 1;# or C['horse'] = E['Dunhill'] - 1; #C[4]=E[1]+1 or C[4]=E[1]-1;
     R11a: C['horse']  - E['Dunhill'] <= 1 + M2 * yR11;
     R11b: C['horse']  - E['Dunhill'] <=     M2 * (1-yR11);
   
     R12: E['Bluemaster'] = D['juice']; #E[4]=D[4];
     R13: B['blue']     = 2;            #B[2]=2;
     R14: A['German']   = E['Prince'];  #A[5]=E[5];

     # an or!   
     ####R15: E['Marlboro'] = D['water']+1;# or E['Marlboro'] = D['water']-1; #E[2]=D[5]+1 or E[2]=D[5]-1;
     R15a: E['Marlboro'] - D['water'] <= 1 + M2 * yR15;
     R15b: E['Marlboro'] - D['water'] <=     M2 * (1-yR15);

# and some 'school book approach' to alldifferent
all_differentA1{i in a, j in a}: 
   M*yA[i,j]       + (A[i] - A[j]) >= (if i <> j then 1);
all_differentA2{i in a, j in a}: 
   M*(1 - yA[i,j]) + (A[j] - A[i]) >= (if i <> j then 1);

all_differentB1{i in b, j in b}: 
   M*yB[i,j]       + (B[i] - B[j]) >= (if i <> j then 1);
all_differentB2{i in b, j in b}: 
   M*(1 - yB[i,j]) + (B[j] - B[i]) >= (if i <> j then 1);

all_differentC1{i in c, j in c}: 
   M*yC[i,j]       + (C[i] - C[j]) >= (if i <> j then 1);
all_differentC2{i in c, j in c}: 
   M*(1 - yC[i,j]) + (C[j] - C[i]) >= (if i <> j then 1);

all_differentD1{i in d, j in d}: 
   M*yD[i,j]       + (D[i] - D[j]) >= (if i <> j then 1);
all_differentD2{i in d, j in d}: 
   M*(1 - yD[i,j]) + (D[j] - D[i]) >= (if i <> j then 1);

all_differentE1{i in e, j in e}: 
   M*yE[i,j]       + (E[i] - E[j]) >= (if i <> j then 1);
all_differentE2{i in e, j in e}: 
   M*(1 - yE[i,j]) + (E[j] - E[i]) >= (if i <> j then 1);

data;

param matrix: 1 2 3 4 5 := 
 1 'Norwegian'  'Dane'  'Briton'  'Swede'  'German'
 2 'yellow'  'blue'  'red'  'green'  'white'
 3 'cat'  'bird'  'dog'  'horse'  'fish'
 4 'coffee'  'tea'  'milk'  'juice'  'water'
 5 'Dunhill'  'Marlboro' 'Pall-Mall'  'Bluemaster'  'Prince'
;



# or whatever...
maximize obj: A['Norwegian'];

#option presolve 0;
option show_stats 1;
option solver cplex;
#option solver lpsolve;
#option solver bonmin;

solve;

display A,B,C,D,E;
#display A;
#display B;
#display C;
#display D;
#display E;

display a,b,c,d,e;
#printf "C['fish']: %s\n", C['fish'];

for{i in a} {
    if A[i]=C['fish'] then printf "The %s owns the fish (%d)\n", i, A[i];
}


printf "nationality:\t";
for{i in a} {
        printf "%s(%d)  ", i, A[i];
}
printf "\ncolor:\t\t";

for{i in b} {
        printf "%s(%d)  ", i, B[i];

}
printf "\npet:\t\t";
for{i in c} {
        printf "%s(%d)  ", i, C[i];

}
printf "\ndrinks:\t\t";
for{i in d} {
        printf "%s(%d)  ", i, D[i];

}
printf "\nsmokes\t\t";
for{i in e} {
        printf "%s(%d)  ", i, E[i];

}
printf "\n";

display yR10,yR11,yR15;



