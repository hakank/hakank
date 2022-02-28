#
# Read test in z3.
#
# This is one of my standard test when learning a new programming
# language: read a word file and filters the words that match 
# the regular expression
#     a.*b.*c..., b.*c.*d.., c.*d.*e..., etc
#
# Here we use z3's Re functions, e.g.
#
#    alpha = Range("a","z")
#    Concat(Star(alpha),
#           Re("a"),
#           Star(alpha),
#           Re("b"),
#           Star(alpha),
#           Re("c"),
#           Star(alpha)
#          )
#
# Some examples using /usr/share/dict/words:
# * .*e.*f.*g.*h.* (4 characters)
#  ['prefight', 'prizefight', 'prizefighter', 'prizefighters', 'prizefighting', 'prizefightings',
#   'prizefights', 'refight', 'refighting', 'refights', 'refought', 'spaceflight', 'spaceflights']
#
# * .*h.*i.*j.*k.* (4 characters)
#   ['antihijack', 'highjack', 'highjacked', 'highjacking', 'highjacks', 'hijack', 'hijacked',
#    'hijacker', 'hijackers', 'hijacking', 'hijacks', 'hijinks']
#
# * .*l.*m.*n.*o.*p.* (5 characters)
# ['albuminoscope', 'albuminurophobia', 'alimentotherapy', 'aluminographic', 'aluminography',
#  'aluminotype', 'helminthophobia', 'helminthosporiose', 'helminthosporium' ,'helminthosporoid',
#  'limnograph', 'limnophil', 'limnophile' ,'limnophilid', 'limnophilidae', 'limnophilous',
#  'limnophobia', 'limnoplankton', 'luminophor', 'luminophore', 'lymhpangiophlebitis',
#  'lymphadenopathy', 'lymphangioplasty']
#
#
# Using a Swedish wordlist we find these words matching .*k.*l.*m.*n.*o.*p.*
# (6 characters) which is the longest sequence found.
# ['alkoholmonopol', 'kaliumtetracyanokuprat', 'kaliumtetracyanoplatinat', 'komplementoperation',
#  'kulminationspunkt', 'vinkelmätningsmikroskop']
# Of these words, the first (alkoholmonopol) is probably the only one used in common parlance.
# It means monopoly of alcohol sales.
#
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# """
import time
from z3 import *

#
# The regex is (for the chars "abc")
#
# regex = Concat([whatever,
#                Re("a"),
#                whatever,
#                Re("b"),
#                whatever,
#                Re("c"),
#                whatever]
#                )
# 
# (This could probably be written a little neater...)
#
def gen_regex(chars):
    n = len(chars)
    alpha = Range("a","z")
    whatever = Star(alpha)
    return Concat([whatever] +
                  [Concat(a,b) for (a,b) in list(zip([Re(c) for c in chars],
                                                     [whatever for i in range(n)]))])

def read_test(words,n):
    """
    Generate all possible string give the
    regular expression regex.
    """

    # s = Solver()
    # s = SolverFor("QF_S")
    # s = SolverFor("ALL")
    s = SolverFor("QF_SLIA")
    s.set("smt.string_solver","z3str3")
    # s.set("smt.string_solver","auto")
    
    # Some constants
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    alphabet_len = len(alphabet)
    for chars in[alphabet[m:m+n] for m in range(alphabet_len - n+1)]:
        print("chars:", ".*" + ".*".join(chars) + ".*")
        regex = gen_regex(chars)
    
        matches = []
        t0 = time.time()
        for word in words:
            s.push()
            s.add(InRe(word,regex))
            if s.check() == sat:
                matches.append(word)
            s.pop()
        t1 = time.time()

        print(matches)
        print("len:",len(matches))
        print("Time: ", t1-t0)
        print()

# word_list = "words_lower.txt"
# word_list = "eng_dict.txt"
word_list = "/usr/share/dict/words"

words = [word.rstrip() for word in open(word_list).readlines()]
n = 4
print("n:", n)
read_test(words,n)
