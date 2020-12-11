#!usr/bin/perl

use strict;
use warnings;
use diagnostics;

#scalar value
my @a=(1,2,3,4);
print "$a[0]\n";


#hash value
my %h=( "one"=>2, 
        "two"=>3);
        
my $c=$h{"one"}+$h{"two"};
print "$c\n";

#referencing array - putting whole array with @$ref1 to another 
my $ref1=\@a;
my @a2 = ("a", "b", @$ref1);
print "@a2 \n";

#referencing array $ref1->[2]
my $zzz=$ref1->[2];

print "$zzz \n";
print "$zzz $ref1->[0] \n";

#referencing hash value %$ref2
my $ref2=\%h;
my @a3 = ("a", "b", %$ref2);
print "@a3 \n";

my $aaa=$ref2->{"one"};
print "$aaa \n";
print "@a $aaa \n";




