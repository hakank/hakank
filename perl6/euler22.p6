use v6;

say qq{
Project Euler problem 22

Using names.txt (right click and 'Save Link/Target As...'), a 46K 
text file containing over five-thousand first names, begin by sorting 
it into alphabetical order. Then working out the alphabetical value 
for each name, multiply this value by its alphabetical position in the 
list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, 
which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in 
the list. So, COLIN would obtain a score of 938 53 = 49714.

What is the total of all the name scores in the file?")
};

my @names = open("euler22_names.txt").lines().split(",");
@names = (@names.map: { .trans('"' => '')}).sort();
my %alpha = ("A".."Z") Z=> (1...*);
my $total = 0;
my $c = 1; 
for @names -> $name {
    $total += ([+] %alpha{$name.comb}) * $c;
    $c++;
}
say "total: $total";