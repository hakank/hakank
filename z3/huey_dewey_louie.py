#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Huey, Dewey and Louie problem in Z3
#
# From Marriott & Stucket, Programming with Constraints, page 42
# """
# Huey, Dewey and Louie are being questioned by their uncle. These are the 
# statements the make:
#   Huey: Dewey and Louie has equal share in it; if one is quitly, so is the other.
#   Dewey: If Huey is guilty, then so am I.
#   Louie: Dewey and I are not both quilty.
# 
# Their uncle, knowing that they are cub scouts, realises that they cannot tell a lie.
# Has he got sufficient information to decide who (if any) are quilty?
# """
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

huey, dewey, louie = Bools("huey dewey louie")
solve(
    # Huey: Dewey and Louie has equal share in it; if one is quitly, so is the other.
     dewey == louie,

    # Dewey: If Huey is guilty, then so am I.
    Implies(huey,dewey),

    # Louie: Dewey and I are not both quilty.
    Not(dewey, louie)
    )
