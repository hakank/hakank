#
# Generate all spellings of Mankell and Kjellerstrand in z3.
#
# This is a recurring problem for me: Generating 'all' possible
# spellings of Henning Mankell and Kjellerstrand given a grammar 
# or regular expression.
#
# I have written about this before:
# - Regular expressions in Gecode
#   http://www.hakank.org/constraint_programming_blog/2009/04/regular_expressions_in_gecode.html
#   This blog post contains further links and references.
#
# - Icon program for the Henning Mankell problem:
#   http://www.hakank.org/unicon/pattern_generation.icn
#
# - Picat program
#   http://www.hakank.org/picat/mankell_v3.pi
#
# This regular expression is from misspelling of
# the last name of Henning Mankell:
#
#   [hm][ea](nk|n|nn)(ing|ell|all)
 # 
# The generated strings are:
# ["menall", "hanall", "hening", "henell", "henall", "hanell", "haning", "maning", "mening",
#  "menell", "manell", "manall", "menning", "henning", "henking", "menking", "manking", "manning",
#  "hanning", "hanking", "hankell", "hannell", "mannell", "mankell", "menkell", "mennell", "hennell",
#  "henkell", "henkall", "menkall", "mankall", "hankall", "mannall", "hannall", "hennall", "mennall"]
# len: 36
#
# As you can see both "henning" and "mankell" are generated.
#
# This regular expression is for (most of) the misspellings
# of my last name, which actually is Kjellerstrand:
#
#    k(je|ä)ll(er|ar)?(st|b)r?an?d
#
# The generated strings are:
# ["källbad", "kjellerbad", "källerband", "källarband", "källstrand", "källerbrad", "källerstad",
#  "källarstad", "källarbrad", "kjellarbad", "kjellstrad", "kjellstand", "kjellbrand", "källstand",
#  "källbrand", "källstrad", "källarbad", "källerbad", "källband", "källstad", "källbrad", "kjellstad",
#  "kjellbrad", "kjellband", "kjellbad", "kjellarstrad", "kjellarstrand", "kjellarbrand", "kjellarstand",
#  "kjellerbrand", "kjellerstrad", "kjellerstand", "kjellerstrand", "källarstrand", "källerstrand",
#  "källerstand", "källerbrand", "källarbrand", "källarstand", "källarstrad", "källerstrad", "kjellarstad",
#  "kjellerstad", "kjellerbrad", "kjellarbrad", "kjellstrand", "kjellarband", "kjellerband"]
# len: 48
#
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# """
from z3 import *


# [hm][ea](nk|n|nn)(ing|ell|all)
mankell = Concat(
    Union(Re("h"),Re("m")),              # h|e
    Union(Re("e"),Re("a")),              # e|a
    Union(Re("nk"),Re("n"),Re("nn")),    # nk|n|nn 
    Union(Re("ing"),Re("ell"),Re("all")) # ing|ell|all
    )

# k(je|ä)ll(er|ar)?(st|b)r?an?d
kjellerstrand = Concat(
    Re("k"),                             # k
    Union(Re("je"),Re("ä")),             # je|ä
    Re("ll"),                            # ll
    Option(Union(Re("er"),Re("ar"))),    # (er|ar)?
    Union(Re("st"),Re("b")),             # (st|b)?
    Option(Re("r")),                     # r?
    Re("a"),                             # a
    Option(Re("n")),                     # n?
    Re("d")                              # d
    )


def generate_strings(regex):
    """
    Generate all possible string give the
    regular expression regex.
    """

    s = Solver()
    x = String("x")
    s.add(InRe(x,regex))

    sols = []
    while s.check() == sat:
        mod = s.model()
        x_val = mod[x]
        sols.append(x_val)
        s.add(x_val != x)
    return sols

print("\nmankell:")
s1 = generate_strings(mankell)
print(s1)
print("len:", len(s1))
print()

print("kjellerstrand:")
s2 = generate_strings(kjellerstrand)
print(s2)
print("len:", len(s2))

    



