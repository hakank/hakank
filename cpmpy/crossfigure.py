"""
Crossfigure in cpmpy.

CSPLib problem 21
http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob021/index.html
'''
Crossfigures are the numerical equivalent of crosswords. You have a grid and some 
clues with numerical answers to place on this grid. Clues come in several different 
forms (for example: Across 1. 25 across times two, 2. five dozen, 5. a square number, 
10. prime, 14. 29 across times 21 down ...). 
'''

Also, see 
http://en.wikipedia.org/wiki/Cross-figure

William Y. Sit: 'On Crossnumber Puzzles and The Lucas-Bonaccio Farm 1998'
http://scisun.sci.ccny.cuny.edu/~wyscc/CrossNumber.pdf

Bill Williams: Crossnumber Puzzle, The Little Pigley Farm
http://jig.joelpomerantz.com/fun/dogsmead.html

This model was inspired by the ECLiPSe model written by Warwick Harvey:
http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob021/code.html
 
 
Data from 
http://thinks.com/crosswords/xfig.htm.
 
  This problem is 001 from http://thinks.com/crosswords/xfig.htm 
  ('X' is the blackbox and is fixed to the value of 0)
 
  1  2  3  4  5  6  7  8  9
  ---------------------------
  1  2  _  3  X  4  _  5  6    1
  7  _  X  8  _  _  X  9  _    2
  _  X  10 _  X  11 12 X  _    3
  13 14 _  _  X  15 _  16 _    4 
  X  _  X  X  X  X  X  _  X    5 
  17 _  18 19 X  20 21 _ 22    6
  _  X  23 _  X  24 _  X  _    7
  25 26 X  27 _  _  X  28 _    8
  29 _  _  _  X  30 _  _  _    9

 
  The answer is
   1608 9183
   60 201 42
   3 72 14 1
   5360 2866
    3     4
   4556 1156
   9 67 16 8
   68 804 48
   1008 7332


Note: This model is a port of my Comet model which use base 1, so
here we adjust to Python's base 0.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


#
# across(Matrix, Across, Len, Row, Col)
#        Constrains 'Across' to be equal to the number represented by the
#        'Len' digits starting at position (Row, Col) of the array 'Matrix'
#        and proceeding across.
#
def across(Matrix, Across, Len, Row, Col):
    Row -= 1
    Col -= 1
    tmp = intvar(0,9999,shape=Len)
    constraints = []
    constraints += [to_num(tmp, Across, 10)]
    for i in range(Len):
        constraints += [Matrix[Row,Col+i] == tmp[i]]
    return constraints



#
# down(Matrix, Down, Len, Row, Col):
#	Constrains 'Down' to be equal to the number represented by the
#	'Len' digits starting at position (Row, Col) of the array 'Matrix'
#	and proceeding down.
#
def down(Matrix, Down, Len, Row, Col):
    Row -= 1
    Col -= 1
    tmp = intvar(0,9999,shape=Len)
    constraints = []
    constraints += [to_num(tmp, Down, 10)]
    for i in range(Len):
        constraints += [Matrix[Row+i,Col] == tmp[i]]
    return constraints


def crossfigure():

    model = Model()
    n = 9

    D = 9999 # the max length of the numbers in this problem is 4
    primes = [i for i in range(2,D+1) if is_prime(i)]
    squares = [i**2 for i in range(1,1+math.ceil(math.sqrt(D)))]

    Z = -1
    B = -2 # Black box
    # The valid squares (or rather the invalid are marked as B)
    Valid = [[ Z,  Z,  Z,  Z,  B, Z, Z, Z, Z],
             [ Z,  Z,  B,  Z,  Z, Z, B, Z, Z],
             [ Z,  B,  Z,  Z,  B, Z, Z, B, Z],
             [ Z,  Z,  Z,  Z,  B, Z, Z, Z, Z],
             [ B,  Z,  B,  B,  B, B, B, Z, B],
             [ Z,  Z,  Z,  Z,  B, Z, Z, Z, Z],
             [ Z,  B,  Z,  Z,  B, Z, Z, B, Z],
             [ Z,  Z,  B,  Z,  Z, Z, B, Z, Z],
             [ Z,  Z,  Z,  Z,  B, Z, Z, Z, Z]]
    

    M = intvar(0,9,shape=(n,n),name="M")

    for i in range(n):
        for j in range(n):
            if Valid[i][j] == B:
                model += (M[i,j] == 0)

    A1 = intvar(0,D,name="A1")
    A4 = intvar(0,D,name="A4")
    A7 = intvar(0,D,name="A7")
    A8 = intvar(0,D,name="A8")
    A9 = intvar(0,D,name="A9")
    A10 = intvar(0,D,name="A10")
    A11 = intvar(0,D,name="A11")
    A13 = intvar(0,D,name="A13")
    A15 = intvar(0,D,name="A15")
    A17 = intvar(0,D,name="A17")
    A20 = intvar(0,D,name="A20")
    A23 = intvar(0,D,name="A23")
    A24 = intvar(0,D,name="A24")
    A25 = intvar(0,D,name="A25")
    A27 = intvar(0,D,name="A27")
    A28 = intvar(0,D,name="A28")
    A29 = intvar(0,D,name="A29")
    A30 = intvar(0,D,name="A30")

    AList = [A1,A4,A7,A8,A9,A10,A11,A13,A15,A17,A20,A23,A24,A25,A27,A28,A29,A30]

    D1 = intvar(0,D,name="D1")
    D2 = intvar(0,D,name="D2")
    D3 = intvar(0,D,name="D3")
    D4 = intvar(0,D,name="D4")
    D5 = intvar(0,D,name="D5")
    D6 = intvar(0,D,name="D6")
    D10 = intvar(0,D,name="D10")
    D12 = intvar(0,D,name="D12")
    D14 = intvar(0,D,name="D14")
    D16 = intvar(0,D,name="D16")
    D17 = intvar(0,D,name="D17")
    D18 = intvar(0,D,name="D18")
    D19 = intvar(0,D,name="D19")
    D20 = intvar(0,D,name="D20")
    D21 = intvar(0,D,name="D21")
    D22 = intvar(0,D,name="D22")
    D26 = intvar(0,D,name="D26")
    D28 = intvar(0,D,name="D28")
    DList = [D1,D2,D3,D4,D5,D6,D10,D12,D14,D17,D18,D19,D20,D21,D22,D26,D28]


    # Set up the constraints between the matrix elements and the
    # clue numbers.
    #
    # Note: Row/Col are adjusted to base-0 in the
    #       across and down methods.
    #
    model += (across(M, A1, 4, 1, 1))
    model += (across(M, A4, 4, 1, 6))
    model += (across(M, A7, 2, 2, 1))
    model += (across(M, A8, 3, 2, 4))
    model += (across(M, A9, 2, 2, 8))
    model += (across(M, A10, 2, 3, 3)) 
    model += (across(M, A11, 2, 3, 6))
    model += (across(M, A13, 4, 4, 1))
    model += (across(M, A15, 4, 4, 6))
    model += (across(M, A17, 4, 6, 1))
    model += (across(M, A20, 4, 6, 6))
    model += (across(M, A23, 2, 7, 3))
    model += (across(M, A24, 2, 7, 6))
    model += (across(M, A25, 2, 8, 1))
    model += (across(M, A27, 3, 8, 4))
    model += (across(M, A28, 2, 8, 8))
    model += (across(M, A29, 4, 9, 1))
    model += (across(M, A30, 4, 9, 6))

    model += (down(M, D1, 4, 1, 1))
    model += (down(M, D2, 2, 1, 2))
    model += (down(M, D3, 4, 1, 4))
    model += (down(M, D4, 4, 1, 6))
    model += (down(M, D5, 2, 1, 8))
    model += (down(M, D6, 4, 1, 9))
    model += (down(M, D10, 2, 3, 3)) 
    model += (down(M, D12, 2, 3, 7))
    model += (down(M, D14, 3, 4, 2))
    model += (down(M, D16, 3, 4, 8))
    model += (down(M, D17, 4, 6, 1))
    model += (down(M, D18, 2, 6, 3))
    model += (down(M, D19, 4, 6, 4))
    model += (down(M, D20, 4, 6, 6))
    model += (down(M, D21, 2, 6, 7))
    model += (down(M, D22, 4, 6, 9))
    model += (down(M, D26, 2, 8, 2))
    model += (down(M, D28, 2, 8, 8))


    # Set up the clue constraints.
    #  Across
    #  1 27 across times two
    #  4 4 down plus seventy-one
    #  7 18 down plus four
    #  8 6 down divided by sixteen
    #  9 2 down minus eighteen
    # 10 Dozen in six gross
    # 11 5 down minus seventy
    # 13 26 down times 23 across
    # 15 6 down minus 350
    # 17 25 across times 23 across
    # 20 A square number
    # 23 A prime number
    # 24 A square number
    # 25 20 across divided by seventeen
    # 27 6 down divided by four
    # 28 Four dozen
    # 29 Seven gross
    # 30 22 down plus 450 

    model += (A1 == 2 * A27)
    model += (A4 == D4 + 71)
    model += (A7 == D18 + 4)
    # model += (A8 == D6 / 16)
    model += (16*A8 == D6)    
    model += (A9 == D2 - 18)
    # model += (A10 == 6 * 144 / 12)
    model += (12*A10 == 6 * 144)    
    model += (A11 == D5 - 70)
    model += (A13 == D26 * A23)
    model += (A15 == D6 - 350)
    model += (A17 == A25 * A23)
    # model += (square(A20))
    model += (member_of(squares,A20))
    # model += (is_prime(A23))
    model += (member_of(primes,A23))
    # model += (square(A24))
    model += (member_of(squares,A24))    
    # model += (A25 == A20 / 17)
    model += (17*A25 == A20)    
    # model += (A27 == D6 / 4)
    model += (4*A27 == D6)    
    model += (A28 == 4 * 12)
    model += (A29 == 7 * 144)
    model += (A30 == D22 + 450)

    # Down
    #
    #  1 1 across plus twenty-seven
    #  2 Five dozen
    #  3 30 across plus 888
    #  4 Two times 17 across
    #  5 29 across divided by twelve
    #  6 28 across times 23 across
    # 10 10 across plus four
    # 12 Three times 24 across
    # 14 13 across divided by sixteen
    # 16 28 down times fifteen
    # 17 13 across minus 399
    # 18 29 across divided by eighteen
    # 19 22 down minus ninety-four
    # 20 20 across minus nine
    # 21 25 across minus fifty-two
    # 22 20 down times six
    # 26 Five times 24 across
    # 28 21 down plus twenty-seven 

    model += (D1 == A1 + 27)
    model += (D2 == 5 * 12)
    model += (D3 == A30 + 888)
    model += (D4 == 2 * A17)
    # model += (D5 == A29 / 12)
    model += (12*D5 == A29)    
    model += (D6 == A28 * A23)
    model += (D10 == A10 + 4)
    model += (D12 == A24 * 3)
    # model += (D14 == A13 / 16)
    model += (16*D14 == A13)    
    model += (D16 == 15 * D28)
    model += (D17 == A13 - 399)
    # model += (D18 == A29 / 18)
    model += (18*D18 == A29)    
    model += (D19 == D22 - 94)
    model += (D20 == A20 - 9)
    model += (D21 == A25 - 52)
    model += (D22 == 6 * D20)
    model += (D26 == 5 * A24)
    model += (D28 == D21 + 27)

    def print_sol():
        Mval = M.value()
        print(Mval)
        for i in range(n):
            for j in range(n):
                if Valid[i][j] == B:
                    print("_", end="")
                else:
                    print(Mval[i,j],end="")
            print()
        print()
        print("AList:",[AList[i].value() for i in range(len(AList))])
        print("DList:",[DList[i].value() for i in range(len(DList))])
   
    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 1
    ss.ort_solver.parameters.cp_model_probing_level = 0
    
    num_solutions = ss.solveAll(display=print_sol)
    print()
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    print()    


crossfigure()
