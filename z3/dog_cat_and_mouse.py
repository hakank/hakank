#
# Dog, Cat and Mouse problem in z3.
#
# From https://ericpony.github.io/z3py-tutorial/guide-examples.htm
# """
# Dog, Cat and Mouse
# Consider the following puzzle. Spend exactly 100 dollars and buy exactly 100 animals.
# Dogs cost 15 dollars, cats cost 1 dollar, and mice cost 25 cents each. You have to
# buy at least one of each.
#
# How many of each should you buy?
# """
#
# The standard approach on these puzzles is to convert everything to integers
# and multiply by a constant (here 100).
#
# Is is possible to use the given proper (real) coefficients instead of this
# multiplication hack? Yes, but one have to be careful to use z3's Q(.,.) instead of
# (Python's) 1/4 or 0.25.
#
# Solutions:
#  Int approach
#  [mouse = 56, cat = 41, dog = 3]
#
#  Real approach
#  [mouse = 56, cat = 41, dog = 3]
#

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#

from z3 import *

#
# This is the model from the site mentioned above.
#
def dog_cat_mouse_int():
    # Create 3 integer variables
    dog, cat, mouse = Ints('dog cat mouse')
    solve(dog >= 1,   # at least one dog
          cat >= 1,   # at least one cat
          mouse >= 1, # at least one mouse
          # we want to buy 100 animals
          dog + cat + mouse == 100,
          # We have 100 dollars (10000 cents):
          #   dogs cost 15 dollars (1500 cents), 
          #   cats cost 1 dollar (100 cents), and 
          #   mice cost 25 cents 
          1500 * dog + 100 * cat + 25 * mouse == 10000)


#
# Here is a version using the original (real!) values.
# Though we have to be careful to use Q(.,.) and
# not Python's rationals for representing 25 cents: Q(1,4)
#
def dog_cat_mouse_real():
    dog, cat, mouse = Ints('dog cat mouse')
    solve(dog >= 1,   # at least one dog
          cat >= 1,   # at least one cat
          mouse >= 1, # at least one mouse
          # we want to buy 100 animals
          dog + cat + mouse == 100,
          # We have 100 dollars:
          #   dogs cost 15 dollars, 
          #   cats cost 1 dollar, and 
          #   mice cost 25 cents

          # This doesn't give the correct answer: [dog = 1, mouse = 14, cat = 85]
          # 15 * dog + 1 * cat + 1/4 * mouse == 100)
          # And this doesn't either: [dog = 1, mouse = 14, cat = 85]
          # 15 * dog + 1 * cat + 0.25 * mouse == 100)
          
          # But these two variants work, i.e. using Q()
          # 15 * dog + 1 * cat + Q(1,4) * mouse == 100
          15 * dog + 1 * cat + Q(25,100) * mouse == 100          
      )

print("Int approach")
dog_cat_mouse_int()

print("\nReal approach")
dog_cat_mouse_real()
