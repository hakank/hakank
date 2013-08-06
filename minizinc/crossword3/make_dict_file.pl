#!/usr/local/bin/perl
# 
# Wed Sep 21 18:55:23 2011/hakank@bonetmail.com# 
# 
# Makes a wordlist .dzn file
# 
$|=1;
use strict;
use warnings;
# use diagnostics;

# "/usr/share/dict/words";
my $dict = $ARGV[0] || die "Syntax: $0 wordlist";

print <<EOT;
%
% Words for crosswords3.mzn
%
% This is words extracted from the wordlist file 
%   $dict
%
EOT


open my $fh, "<", $dict or die "Cannot open $dict: $!";
my %all = ();
for (split //, "abcdefghijklmnopqrstuvwxyz") {
    push @{$all{1}}, $_;
}

my $total_words = 0;
my @words = ();
while (<$fh>) {
  chomp;
  s/\r//;
  $_ = lc $_;
  next if !/^[a-z]+$/;
  # next if length $_ < 2;
  # push @words, $_;
  my $len = length $_;
  next if $len == 1; # we handle this above
  push @{$all{$len}}, $_;
  $total_words++;
}



for my $size (sort {$a <=> $b} keys %all) {

    my @words = @{$all{$size}};
    my $num_words = scalar @words;

    print <<EOT;

int: num_words$size = $num_words;
array[1..num_words$size, 1..$size] of int: words$size = array2d(1..num_words$size, 1..$size,
[
EOT
  for (@words) {
    my @word = split //;
    print join ",", @word;
    print ",\n";

  }
  print "]);\n\n";

}

print "% Total words: $total_words\n";
warn "% Total words: $total_words\n";
