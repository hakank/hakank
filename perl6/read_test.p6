use v6;

# Check a word list for all word that matches the patterns
# (here for n = 3):
#   a .* b .* c
#   b .* c .* d
#   c .* d .* e
#   ...
#   z .* y .* z
#

my $n = 6; # number of chars to use
say "N: $n";

# my $file = "unixdict.txt";
my $file = "/usr/share/dict/words";

my @words = open($file).lines;
say "{+@words} words\n";

for ((0..26-$n).map: {("a".."z")[($^a...*)[^$n]].join(".*")}) {
  say "$^r: "; 
  .say for @words.grep(/<$^r>/);
  say();
}

# masak's take. Nice!
# say "\nmasaks's take";
# sub letter-regex($n) { 
#   my $r = "(.)"; 
#   for 1 ..^ $n { 
#       my  $l = $_ - 1; 
#       $r ~= " .* (.) <?\{ \${$_} eq \${$l}.Str.succ \}>" 
#   };
#   return rx/$r/;
# };

# .say for @words.grep(letter-regex(3));

