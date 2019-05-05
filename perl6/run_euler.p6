use v6;

# Run euler*.p6 programs.

my %answers = (
  1  => 233168,
  2  => 4613732,
  3  => 6857,
  4  => 906609,
  5  => 232792560,
  6  => 25164150,
  7  => 104743,
  8  =>  40824,
  9  => 31875000,
  10  => 142913828922,
  11  => 70600674,
  12  => 76576500,
  13  => 5537376230,
  14  => 837799,
  15  => 137846528820,
  16  => 1366,
  17  => 21124,
  18  => 1074,
  19  => 171,
  20  => 648,
  21  => 31626,
  22  => 871198282,
  23  => 4179871,
  24  => 2783915460,
  25  => 4782,
  26  => 983,
  27  => -59231,
  28  => 669171001,
  29  => 9183,
  30  => 443839,
  31  => 73682,
  32  => 45228,
  33  => 100,
  34  => 40730,
  35  => 55,
  36  => 872187,
  37  => 748317,
  38  => 932718654,
  39  => 840,
  40  => 210,
  41  => 7652413,
  42  => 162,
  43  => 16695334890,
  44  => 5482660,
  45  => 1533776805,
  46  => 5777,
  47  => 134043,
  48  => 9110846700,
  49  => 296962999629,
  50  => 997651,
);

# .say for %answers.sort: { $^a.key <=> $^b.key };

my %times = ();
for 1..50 -> $problem {
  my $prog = "euler$problem.p6";
  # say $prog;
  if !$prog.IO.e {
    say "Problem $prog don't exist!";
    %times{$problem} = 9999;
    next;
  }
  my $p;
  my $answer;
  my $time = timeit {
    $p = run "perl6", $prog, :out;
    $answer = $p.out.slurp: :close;
  }
  %times{$problem} = $time;
  my $check = %answers{$problem};
  # say "CHECK: $check\nANSWER: $answer";
  if $answer ~~ /$check/ {
    say "Problem $problem $check: OK! (time: $time)";
  } else {
    say "Problem $problem is not correct! Should be $check! (time: $time)";
  }

}

.say for %times.sort: { $^a.key <=> $^b.key };;

sub timeit(&code) is export {
  my $t0 = now;
  code();
  my $t1 = now;
  return $t1 - $t0;
};
