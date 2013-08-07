use v6;

# Check a word list for all word that matches the patterns
# (here for n = 3):
#   a .* b .* c
#   b .* c .* d
#   c .* d .* e
#   ...
#   z .* y .* z
#

# my @words = open("words").lines; 
my @words = <anna erica peter hakank abcdef dijkstra misconceptions xenophobia>;
my $n = 3;
for ((0..22).map: {("a".."z")[($^a...*)[^$n]].join(".*")}) {
                                                         say "$^r: "; 
                                                        .say for @words.grep(/<$^r>/);
}

# masak's take. Nice!
say "\nmasaks's take";
sub letter-regex($n) { 
         my $r = "(.)"; 
         for 1 ..^ $n { 
            my  $l = $_ - 1; 
            $r ~= " .* (.) <?\{ \${$_} eq \${$l}.Str.succ \}>" 
         };
         return eval "rx/$r/" 
}; 
.say for @words.grep(letter-regex(3));

