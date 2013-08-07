use v6;

say qq/
Problem 13

Work out the first ten digits of the sum of the following 
one-hundred 50-digit numbers.
  37107287533902102798797998220837590246510135740250
  ....
  20849603980134001723930671666823555245252804609722
  53503534226472524250874054075591789781264330331690")
/;

# 5537376230

# Rakudo returns - as of writing - a double as result (5.53737623039088e+51)
# say ([+] open("euler13.txt").lines()).substr(0, 10);
say ([+] open("euler13.txt").lines()).subst(".","").substr(0, 10);




