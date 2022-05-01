/ Using the ngn k-version
/ https://codeberg.org/ngn/k

/ Read the word file
w:0:"wordlist.txt"

p:".l..."                           / chars in correct positions
pn:(#?p)-1                          / length of pattern
cs:("";"";"a";"";"t")               / correct chars but wrong position
niw:"sn"                            / chars not in word

/ Now let's crunch the word list
c:w@&{pn=+/p=x}'w                   / correct position
c:c@&{0=+/{+/x[0]=x[1]}'+(cs;x)}'c  / correct chars 1
c:c@&{0=+/0N=x?,/cs}'c              / correct chars 2
c:c@&{0=+/~0N=x?niw}'c              / not in words

/ Prepare the frequency table for the
/ characters in each position in the wordlist words
/ (See the table below.)
a:{`c$(x+97)}'!26                   / alphabet a..z
f:{c:a@&0N=(t:>#'=x)?a;|(t,c)}'+w   / calculate the frequency table

/ Calculate points and sort according to frequency tables
/ The points are
/  total of the word's characters in the frequency table
/  + 100 if the word has unique characters
$|<({`$x}'c)!{{((5=#?x)*100)++/{x[0]?x[1]}'+(f;x)}x}'c


/ Here's the frequency table, but that was to long to
/ fit into a tweet :-)
/ ("xzykjquinovhewlrmdgfaptbcs"
/  "jzqfkgxvsbdymcwptnhulieroa"
/  "qjhzxkfwyvcbpmgdstlnrueoia"
/  "qjyxzbwhfvpkmdguotrcilasne"
/  "vqjuzxbiwfcsgmpoakdnhlrtye")
