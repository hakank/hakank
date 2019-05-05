use v6;

say qq/
n! means n (n 1) ... 3 2 1

Find the sum of the digits in the number 100!
/;

# 0.1s
say [+] ([*] 1..100).comb;

# 0.4s
# sub postfix:<!> { [*] 1..$^a }
# say [+] (100!).comb;

