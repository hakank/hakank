#
# Cracking weak hashes in z3/python.
# 
# This is a more general approach than the z3 code in
# https://github.com/TechSecCTF/z3_splash_class/blob/master/examples/java_hash.ipynb
#
# Description from https://github.com/TechSecCTF/z3_splash_class
# """
# Example: Weak hash function
#
# In computer science, a hash function is a function that takes in a string of characters
# (or any object really), and produces a fixed-size number (the 'hash') that corresponds
# to that string. Importantly, the hashes of two related strings should be different.
# Hash functions are useful for all sorts of things in computer science (like hashtables).
# Cryptographic hash functions are a special type of hash function which also satisfies a
# number of properties, one of the most important of which is that given the hash of a
# string, it should be difficult to reconstruct the string.
#
# One area where hash functions are very useful are in password checking. When a user
# registers with a site, they specify a password. Suppose that the webiste records the
# password in their database. Later when the user logs in, they enter that password and
# the website checks that it matches the password in their database. But now, if the
# website's database gets leaked, every user's account is compromised.
#
# Suppose instead the website computes a hash of the password when a user registers and
# stores the hash in their database instead of the password itself. Then, whenever the
# user logs in, the website can just compute the hash of whatever password is entered and
# check the hash against whatever is stored in their database. If the database leaks (and
# the hash is cryptographically secure, and the passwords themselves are sufficiently
# complex) it should be difficult to easily reverse the hashes and compromise the user
# accounts.
#
# The problem is, many people frequently use non-cryptographically secure hash functions
# for password-checking. In this example, we'll reverse the hash for a website using the
# same hash function that Java uses for its hash tables.
# Check out examples/java_hash.ipynb and examples/java_hash.html.
# """

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 


from z3 import *
from z3_utils_hakank import *

# It's probably best if this is a prime,
# not too small and not too large
the_constant = 311 # 31

#
# hakank: I changed the constant from 31 to a larger value (<the_constant>)
#         to be a little more safe from collisions.
#
def java_hash(s):
    m = 0
    for c in s:
        m *= the_constant # orig: 31
        m += ord(c)
    return m

#
# This is quite elegant!
# No need to split up in a temporary array etc.
# And I like that the constraint can handle
# return values.
# hakank: I changed the constant from 31 to <the_constant>
#         to be a little more safe from collisions
def z3_java_hash(s):
    m = 0
    for c in s:
        m *= the_constant # orig: 31
        m += c
    return m

#
# Note: For longer string it finds the first solution quite quickly,
#       but might that long time to find all solutions/prove unicity.
#
# hash_value = java_hash("reindeer")

# first solution: 1.35s
# proving unicity  6min:05.7s
# hashvalue: 4753331546610330079331168487882425380519499671263955494021868514294220721451700916295554945642778480511310492517615453824272651
# hash_value = java_hash("mysupersecretpasswordthatislongersinceiwanttotestit")

# 0.56s (first solution) 47.3s (unicity)
hash_value = java_hash("mysupersecretpasswordtesting")

# hash_value = java_hash("mysupersecretpassword") # 10.3s (unique)
# hash_value = java_hash("p455w0rd") # 0.26s (unique)
# hash_value = java_hash("s3cR3t_p455w0rd!!#!") # 7.1s (unique)
# hash_value = java_hash("s3cR3t_p455w0rd!!#!~") # 3,698s (unique)
# hash_value = java_hash("p455")
# hash_value = java_hash("attack at dawn")

print("hash_value:", hash_value)
print("hash_value len:", len(str(hash_value)))

# This is the original model (we have to know that it's a 8
# letter password).
#
# a, b, c, d, e, f, g, h = Ints('a b c d e f g h')
# s = Solver()
# s.add(And(a <= ord('z'), ord('a') <= a))
# s.add(And(b <= ord('z'), ord('a') <= b))
# s.add(And(c <= ord('z'), ord('a') <= c))
# s.add(And(d <= ord('z'), ord('a') <= d))
# s.add(And(e <= ord('z'), ord('a') <= e))
# s.add(And(f <= ord('z'), ord('a') <= f))
# s.add(And(g <= ord('z'), ord('a') <= g))
# s.add(And(h <= ord('z'), ord('a') <= h))
# s.add(z3_java_hash([a,b,c,d,e,f,g,h]) == hash_value)
# 
# password = chr(m[a].as_long()) + chr(m[b].as_long()) + chr(m[c].as_long()) + chr(m[d].as_long()) + chr(m[e].as_long()) + chr(m[f].as_long()) + chr(m[g].as_long()) + chr(m[h].as_long())

#
# Here is a more general approach using brute force to find the password length
#
#
# Note: With domain as a..z this generate unique solutions
#       given the java_hash_funcion.
#       But if we allow more characters then there's a lot
#       of solutions.
#
# For example, for the_constant = 31 (the original) then the password "p455"
# and allowing chars 32..127 there's 27 solutions:
#   passwords:
#    ['p43s', 'oQs5', 'nr55', 'nqT5', 'oS55', 'p455', 'oRT5', 'p2s5', 'p3T5', 'nps5',
#     'nqST', 'nqRs', 'nr4T', 'nr3s', 'nprT', 'npqs', 'oRST', 'p3ST', 'oQrT', 'p2rT',
#     'oS4T', 'p44T', 'oQqs', 'oRRs', 'oS3s', 'p3Rs', 'p2qs']
# len: 27
# 
# However, if we change the constant 31 to 311 it seems to better:
# It finds the unique solution for this nice password
#   s3cR3t_p455w0rd!!#!
# (Oh, and now I have to change that. :-))
#
# The hashvalue is 85311497448028982295656856511963563079844261364
# The drawback is that it takes a little longer to check for all
# solutions...
#
# This password is found in 1.5s when num_sols=1
#   mysupersecretpasswordthatislongersinceiwanttotestit
# with the hash value of:
#   4753331546610330079331168487882425380519499671263955494021868514294220721451700916295554945642778480511310492517615453824272651
#
def crack_password(hash_value,n,num_sols=0):
    p = [Int(f"p_{i}") for i in range(n)]
    s = Solver()
    for i in range(n):
        # original range
        # s.add(And(p[i] >= ord('a'), p[i] <= ord('z')))
        # A better range " " .. "~"
        s.add(And(p[i] >= 32, p[i] <= 126))

    s.add(z3_java_hash(p) == hash_value)

    passwords = []
    if s.check() == sat:
        found_sols = 0
        while s.check() == sat:
            mod = s.model()
            # print([mod[t].as_long() for t in p])
            password = "".join([chr(mod[t].as_long()) for t in p])
            print("password:",password, flush=True)
            passwords.append(password)
            if num_sols > 0 and found_sols < num_sols:
                break
            getDifferentSolution(s,mod, p)
            found_sols += 1
        return passwords
    else:
        return None

# num_sols = 0 # 0: find all solutions
num_sols = 0
for n in range(1,126):
    print(".", sep=" ")
    passwords = crack_password(hash_value,n,num_sols)
    if passwords != None:
        print("\nn:",n)
        print("passwords:", passwords)
        print("len:",len(passwords))
        break
