#!/usr/bin/perl
# 
# Wordle solver in Perl.
#
# The basic idea of the scoring is to score the words
# according the the frequency of each characters position.
# The frequency table is created in create_frequenct_table(words).
#
# See comments below for example and how the scoring works.
#
# 
# Created by Hakan Kjellerstrand (hakank@gmail.com)
# Also, see my Perl page: http://hakank.org/perl/
# 
$|=1;
use strict;
use Data::Dumper;

print "Frequency table:\n";
print join "\n", create_freq("wordle_small.txt");
print "\n\n";


# wordle(correct_pos,correct_chars, not_in_word, words)

wordle(".r.n.",["","","","",""],"slatcoe");
# -> briny brink grind bring drink drunk wring wrung

wordle(".r.n.", ["","n","","","br"], "slatcoe");
# -> briny brink bring

wordle(".....",["","r","","",""],"slantcied");
# -> rough forum murky morph humor rugby furry worry roomy furor forgo hurry rumor myrrh juror


# wordle(".....", ["","","","",""], "");
# -> All words

print "\n";

#
# The Wordle solver.
#
# Syntax: wordle(correct_pos,correct_chars, not_in_word)
#
# - correct_pos: characters in correct position (regular expression)
#   Example: <.r.n.>
#     - "r" is in the second position, "n" is in the fourth position
#     - The . means that we don't know that character.
# 
# - correct_chars: characters in the word but not in correct position
#   Example: ["","l","la","","r"]
#     - "l" is in the word but not in second or third position
#     - "r" is in the word but not in the last position
#     - "" means that there are no relevant character in this
#       position
#    
# - not_in_word: characters not in the word
#   Example: "slatcoe"
#    - There is no "s", "l", "a", "t", "c", "o", nor "e" in the word
#
sub wordle {
    my ($correct_pos, $correct_chars, $not_in_word) = @_;

    my @correct_chars_a = map {$_ ne "" ? split // : ""} @$correct_chars;
    my @not_in_word = split //, $not_in_word;

    my $wordlist = "wordle_small.txt";
    my @words = get_words($wordlist);

    my @candidates = ();    
    WORD:
    foreach my $word (@words) {
        my @word = split//,$word;
        
        # Correct position
        next if $word !~ m!$correct_pos!;
        
        # Correct characters but not in correct position
        foreach my $i (0..4) {
            if ($correct_chars->[$i] ne "") {
                foreach my $c ($correct_chars_a[$i]) {
                    next WORD if $word[$i] eq $c or $word !~ m!$c!;
                }
            } 
        }
        
        # Not in word
        next WORD if map {$word =~ m!$_!} @not_in_word;
        
        push @candidates, $word;
    }
    
    print join " ", score_words(@candidates);
    print "\n";

}

#
# Score the candidate words
# 
sub score_words {
    my (@words) = @_;

    my %scores = ();
    for my $word (@words) {
        $scores{$word} = get_score($word);
    }
 
    return (sort {$scores{$b} <=> $scores{$a}} keys %scores)
}


#
# Score a word.
#
# The score is the sum the positions of each character
# in the word according to the position in the frequency
# table.
#
# We add 100 points if this is a word with 5 unique
# characters.
# 
sub get_score {
    my ($word) = @_;
    my @word = split //, $word;
    my @freq = ("xzyjkquinovhewlrmdgfaptbcs",
                "zjqfkgxvbsdymcwptnhulieroa",
                "qjhzxkfwyvcbpmgdstlnrueoia",
                "qjxyzbwhfvpkmdguotrcilasne",
                "vqjuzxbiwfcsgmpoakdnhlrtye");
    
    my $score = 0;
    for my $pos (0..4) {
        # Find the position for this character in the word
        for my $i (0..25) {
            $score += $i if substr($freq[$pos],$i,1) eq $word[$pos];         
        }
    }
    # Bonus: +100 if word has unique characters
    $score += 100 if scalar(uniq(@word)) == 5;
    
    return $score;
}


#
# Create the frequencty table for the wordlist.
#
# Each position of the word has a separate table.
#
sub create_freq {
    my ($wordlist) = @_;
    my @words = get_words($wordlist);

    # Count the number of characters in the specific
    # positions
    my @freq = ({},{},{},{},{});
    foreach my $word (@words) {
        my @word = split //, $word;
        foreach my $pos (0..4) {
            $freq[$pos]{$word[$pos]}  +=1;
        }
    }
    
    my @alpha =  split //, "abcdefghijklmnopqrstuvwxyz";
    my @f = ();
    foreach my $pos (0..4) {
        my %h = %{$freq[$pos]};
        # Sort according to frequency
        my @chars = sort {$h{$b} <=> $h{$a}} keys %h;
        # Missing chars
        my @missing = grep(!$h{$_}, @alpha);        
        push @f, join("", reverse(@chars,@missing));
    }

    return @f;
}

#
# Get all words from the word list
#
sub get_words {
    my ($wordlist) = @_;
    open my $df, $wordlist or die "Cannot open: $wordlist: $!";
    my @words = ();
    push(@words, split) while (<$df>);
    return @words;
}


#
# Return unique elements.
#
sub uniq {
    my %seen;
    grep !$seen{$_}++, @_;
}

