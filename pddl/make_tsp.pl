#!/usr/local/bin/perl
# 
# Tue Jan 17 18:15:20 2012/hakank@bonetmail.com
# 
# 
$|=1;
use strict;
use Data::Dumper;

my $num_nodes = $ARGV[0] || 20;
my $p = $ARGV[1] || 0.5; # probability of connection between two nodes

my @nodes = (1..$num_nodes);
my $objects = join " ", map {"n$_" } @nodes;
my $graph = ();
my $num_connections = 0;
my $start_node = "";
for my $a (@nodes) {
  for my $b (@nodes) {
    if ($a < $b) {
      if (rand 1 < $p) {
        # We assume undirected graph
        $graph .= "    (connected n$a n$b)\n";
        $graph .= "    (connected n$b n$a)\n";
        $num_connections++;
        $start_node = "n$a" if !$start_node;
      }
    }
  }
}

my $date = scalar localtime;

print<<EOT;
;;;
;;; Random graph generated ($date):
;;;   \$ perl make_tsp.pl $num_nodes $p
;;; ($num_connections connections) 
;;;
(define (problem tsp-03)
   (:domain tsp)
   (:objects $objects - node)
   (:init

    ;;; Directed graph
    $graph

    ;;; start
    (at $start_node)
   )

   (:goal 
       (and 
        (at $start_node)
        (forall (?node - node) (visited ?node))
        )

       )
)


EOT
