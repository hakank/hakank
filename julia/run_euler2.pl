#!/usr/bin/perl
#
# 2020-11-15/hakank@gmail.com
#
$|=1;
use strict;
use Data::Dumper;

# Note: Those in %answers are the only one we test.
my %answers = (
"1"=>233168,
"2"=>4613732,
"3"=>6857,
"4"=>906609,
"5"=>232792560,
"6"=>25164150,
"7"=>104743,
"8"=>=>40824,
"9"=>31875000,
"10"=>142913828922,
"11"=>70600674,
"12"=>76576500,
"13"=>5537376230,
"14"=>837799,
"15"=>137846528820,
"16"=>1366,
"17"=>21124,
"18"=>1074,
"19"=>171,
"20"=>648,
"21"=>31626,
"22"=>871198282,
"23"=>4179871,
"24"=>2783915460,
"25"=>4782,
"26"=>983,
"27"=>-59231,
"28"=>669171001,
"29"=>9183,
"30"=>443839,
"31"=>73682,
"32"=>45228,
"33"=>100,
"34"=>40730,
"35"=>55,
"36"=>872187,
"37"=>748317,
"38"=>932718654,
"39"=>840,
"40"=>210,
"41"=>7652413,
"42"=>162,
"43"=>16695334890,
"44"=>5482660,
"45"=>1533776805,
"46"=>5777,
"47"=>134043,
"48"=>9110846700,
"49"=>296962999629,
"50"=>997651,
    );

my $res = `time julia run_euler.jl`;
# print "RES: $res";

my %cpu_times = ();
foreach (split/\n/, $res) {
  next if !/^euler/;
  my ($p, $r, $t) = ($1,$2,$3) if m!^euler(\d+).+?:\s*(.+?):\s*([\d.]+?)s\s*$!;
  my $ans = $answers{$p};
  print "euler$p time:${t}s res:$r  (correct answer:$ans)\n";

  if ($r != $ans) {
    print "WRONG should be: $ans was $r";
  }
  $cpu_times{$p} = $t;
}

my @fails = ();
my $total_cpu_time = 0;
print "\n\nTimes:\n";
foreach (sort {$a <=> $b} keys %cpu_times) {
    my $t = $cpu_times{$_};
    $total_cpu_time += $t;
    my $warn = "";
    if ($t > 10) {
        $warn = "!!!!!";
    } elsif ($t > 5) {
        $warn = "!!!!";
    } elsif ($t > 3) {
        $warn = "!!!";
    } elsif ($t > 2) {
        $warn = "!!";
    } elsif ($t > 1) {
        $warn = "!";
    }
    print "$_: $t $warn\n";
}

print "\nOrder by time (decreasing)\n";
foreach (sort {$cpu_times{$b} <=> $cpu_times{$a}} keys %cpu_times) {
    print "$_: $cpu_times{$_}\n";
}

print "\n";
print "\nTotal CPU time (from julia's \@timed): $total_cpu_time seconds\n";
print "\n";

if (@fails) {
    print "FAILURES: @fails\n\n";
} else {
    print "All OK!\n\n";
}
