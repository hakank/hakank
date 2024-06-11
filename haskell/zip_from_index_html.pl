#!/usr/local/bin/perl
# 
# Sat Jun  9 22:16:24 2012/hakank@bonetmail.com
# 
# Zip all files that are linked in index.html
# 
$|=1;
use strict;
use Data::Dumper;


my $file = "index.html";
open my $fh, "<", $file or die "Cannot open $file: $!";

my $all = join "", <$fh>;
close $fh;

my @files = ();
while ($all =~ /<a href="([^\s]+?.(hs|txt))">/gsm) {
  my $f = $1;
  next if $f !~ /\.(hs|txt)$/;
  push @files, $f;
}

push @files, "index.html";


# print "$_\n" for @files;

print `zip all_public @files`;
print "\n";
