#!/usr/bin/python
"""
Who killed agatha? (The Dreadsbury Mansion Murder Mystery) in Numberjack.

This is a standard benchmark for theorem proving.

http://www.lsv.ens-cachan.fr/~goubault/H1.dist/H1.1/Doc/h1003.html
'''
Someone in Dreadsbury Mansion killed Aunt Agatha. 
Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
are the only ones to live there. A killer always hates, and is no 
richer than his victim. Charles hates noone that Agatha hates. Agatha 
hates everybody except the butler. The butler hates everyone not richer 
than Aunt Agatha. The butler hates everyone whom Agatha hates. 
Noone hates everyone. Who killed Agatha? 
'''

Originally from F. J. Pelletier: 
Seventy-five problems for testing automatic theorem provers. 
Journal of Automated Reasoning, 2: 216, 1986.

Note: There are 8 solutions, all stating that Agatha killed herself.
See my blog post "Decision Management Community November 2014 Challenge: Who killed Agatha?" for
a general discussion of this model (and why there are 8 solutions):
http://www.hakank.org/constraint_programming_blog/2014/11/decision_management_community_november_2014_challenge_who_killed_agath.html


This model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""
import sys
from Numberjack import *

#
# Note: Numberjack don't have a built-in implication syntax.
# Here's a simple version (using Boolean algebra)
#
# (equivalence is done with c1 == c2, etc)
#
def imply(b1,b2):
    return (b1 <= b2)


def agatha(libs):

    n = 3
    agatha  = 0
    butler  = 1
    charles = 2

    people = ["agatha","butler","charles"]
    
    the_killer = Variable(0, 2,"the_killer")
    # the_victim = Variable(0, 2)    

    hates = Matrix(n, n, 0, 1,"hates")
    richer = Matrix(n, n, 0, 1,"richer")

    model = Model()

    # Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
    # are the only ones to live there. 

    # A killer always hates, and is no richer than his victim. 
    model.add(hates[the_killer, agatha] == 1)
    model.add(richer[the_killer, agatha] == 0)

    # define the concept of richer: no one is richer than him-/herself
    model.add([richer[i,i] == 0 for i in range(n)])

    # (contd...) if i is richer than j then j is not richer than 
    model.add(
        [ (richer[i,j] == 1) == (richer[j,i] == 0) for i in range(n) for j in range(n) if i != j]
        )

    # Charles hates noone that Agatha hates. 
    for i in range(n):
        model.add(imply(hates[agatha, i] == 1, hates[charles, i] == 0) )

    # Agatha hates everybody except the butler. 
    model.add(hates[agatha, charles] == 1)
    model.add(hates[agatha, agatha] == 1)
    model.add(hates[agatha, butler] == 0)

    # The butler hates everyone not richer than Aunt Agatha. 
    for i in range(n):
        model.add(imply(richer[i, agatha] == 0,hates[butler, i] == 1 ))

    # The butler hates everyone whom Agatha hates. 
    for i in range(n):
        model.add(imply( hates[agatha, i] == 1, hates[butler, i] == 1) )

    # Noone hates everyone. 
    for i in range(n): 
      model.add(sum([ hates[i,j] for j in range(n)]) <= 2)
     
    # Who killed Agatha? 

    print model

    for library in libs:
        solver = model.load(library)
        if solver.solve():
            solver.printStatistics()
            num_solutions = 1
            while library == 'Mistral' and solver.getNextSolution():
                print "killer: ", people[the_killer.get_value()]
                num_solutions += 1
            print "number of solutions:", num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
        else:
            print "No solution"
        print ''


agatha(['Mistral'])
