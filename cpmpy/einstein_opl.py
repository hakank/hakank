"""
Einstein puzzle in cpmpy.

This is a cpmpy version of the OPL code presented in 
Daniel Selman's 'Einstein's Puzzle - Or, 'The Right Tool for the Job''
http://blogs.ilog.com/brms/2009/02/16/einsteins-puzzle/
'''
* Norwegian cats water Dunhill yellow
* Dane horses tea Blends blue
* Brit birds milk Pall Mall red
* German fish coffee Prince green
* Sweed dogs beer Blue Master white
'''
  
Note: I let the 'Sweed' stand, it should - of course - be 'Swede'.

See also:
http://www.stanford.edu/~laurik/fsmbook/examples/Einstein%27sPuzzle.html

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


# Util for printing the solution:
def p(i,x,s):
  for j in range(len(x)):
    if x[j].value() == i:
      return s[j]

def einstein_puzzle():

  N = 5
  # nationalities
  Brit = 0;Sweed = 1;Dane = 2;Norwegian = 3;German = 4;
  NationalitiesS = ["Brit","Sweed","Dane","Norwegian","German"]

  # animals
  Dogs = 0;Fish = 1;Birds = 2;Cats = 3;Horses = 4;
  AnimalsS = ["Dogs","Fish","Birds","Cats","Horses"]

  # drinks
  Tea = 0;Coffee = 1;Milk = 2;Beer = 3;Water = 4;
  DrinksS = ["Tea","Coffee","Milk","Beer","Water"]

  # smokes
  PallMall = 0;Dunhill = 1;Blends = 2;BlueMaster = 3;Prince = 4;
  SmokesS = ["Pall Mall","Dunhill","Blends","Blue Master","Prince"]

  # color
  Red = 0;Green = 1;White = 2;Yellow = 3;Blue = 4;
  ColorsS = ["Red","Green","White","Yellow","Blue"]
   

  # decision variables
  S = intvar(0,N-1,shape=N,name="S")
  A = intvar(0,N-1,shape=N,name="A")
  D = intvar(0,N-1,shape=N,name="D")
  K = intvar(0,N-1,shape=N,name="K")
  C = intvar(0,N-1,shape=N,name="C")


  model = Model([AllDifferent(S),
                 AllDifferent(A),
                 AllDifferent(D),
                 AllDifferent(K),
                 AllDifferent(C),
   
                 # 1. The Brit lives in a red house.
                 S[Brit] == C[Red],

                 # 2. The Swede keeps dogs as pets.
                 S[Sweed] == A[Dogs],

                 # 3. The Dane drinks tea.
                 S[Dane] == D[Tea],

                 # 5. The owner of the Green house drinks coffee.
                 C[Green] == D[Coffee],

                 # 6. The person who smokes Pall Mall rears birds.
                 K[PallMall] == A[Birds],

                 # 7. The owner of the Yellow house smokes Dunhill.
                 C[Yellow] == K[Dunhill],

                 # 8. The man living in the centre house drinks milk.
                 D[Milk] == 2,

                 # 9. The Norwegian lives in the first house.
                 S[Norwegian] == 0,

                 # 12. The man who smokes Blue Master drinks beer.
                 K[BlueMaster] == D[Beer],

                 # 13. The German smokes Prince.
                 S[German] == K[Prince],
  
                 # 4. The Green house is on the left of the White house.
                 C[Green] == C[White]-1,
  
                 # 10. The man who smokes Blends lives next to the one who keeps cats.
                 abs(K[Blends] - A[Cats]) == 1,

                 # 11. The man who keeps horses lives next to the man who smokes Dunhill.
                 abs(A[Horses] - K[Dunhill]) == 1,

                 # 14. The Norwegian lives next to the blue house.
                 abs(S[Norwegian] - C[Blue]) == 1,

                 # 15. The man who smokes Blends has a neighbour who drinks water.
                 abs(K[Blends] - D[Water]) == 1,
                 ])

  def print_sol():
    print("S:", S.value())
    print("A:", A.value())
    print("D:", D.value())
    print("K:", K.value())
    print("C:", C.value())
    for i in range(N):
      SS = p(i,S,NationalitiesS)
      AA = p(i,A,AnimalsS)
      DD = p(i,D,DrinksS)
      KK = p(i,K,SmokesS)
      CC = p(i,C,ColorsS)
      print("House:",i,SS,AA,DD,KK,CC)
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)

einstein_puzzle()
