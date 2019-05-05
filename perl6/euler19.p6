use v6;

say qq/
Problem 19

You are given the following information, but you may prefer 
to do some research for yourself.

* 1 Jan 1900 was a Monday.
* Thirty days has September,
  April, June and November.
  All the rest have thirty-one,
  Saving February alone,
  Which has twenty-eight, rain or shine.
  And on leap years, twenty-nine.
* A leap year occurs on any year evenly divisible by 4, but not 
  on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the 
twentieth century (1 Jan 1901 to 31 Dec 2000)?

/;

euler19();

# 0.3s
sub euler19() {
  my $count = 0; 
  for 1901..2000 X 1..12  {
      my ($year, $month) = @^x;
      my $month2 =  $month < 10 ?? "0$month" !! $month;
      my $t = Date.new("$year-$month2-01");       
      if $t.day-of-week() == 7 { 
          $count++; 
          # say "$t $count" 
      }
  }
  say "count: $count";
}


# More brutal: 10.2s
sub euler19b() {
  my $date_20001231 = DateTime.new("2000-12-31T00:00:00");
  my $count = 0;  
  for 1...* { 
      my $t = DateTime.new(DateTime.new("1901-01-01T00:00:00").Instant()+24*60*60*$_);       
      # say "T: $t t.day:{$t.day()} t.day-of.week(); {$t.day-of-week()} ";
      if $t.day() == 1 and $t.day-of-week() == 7 { 
          $count++; 
          # say "$t $count" 
      }
      last if $t >= $date_20001231
  }
  say "count: $count";
}