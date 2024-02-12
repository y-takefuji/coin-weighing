#!/usr/bin/perl
package OddBall;
use strict;
use warnings;
our $VERSION = 0.6;  # July 12 2005

=head1 NAME

OddBall - solve the "Odd Ball Challenge", perlmonks node 469482

=head1 SYNOPSIS

 ./odd_ball          # Solve puzzle
 ./odd_ball numbers  # Print puzzle numbers without searching.

or

 ./odd_ball 13 3     # Try 13 balls with 3 weighings; report failure.
 ./odd_ball 16 4     # Find the answer for 16 balls with 4 weighings.
 ./odd_ball 24 4     # Run out of memory and crash.

=head1 CHALLENGE

 From http://perlmonks.org/index.pl?node_id=469482 :

   There is a well known riddle as follows:

     You are presented with 12 balls identical in appearance 
     but 1 of the 12 is either heavier or lighter than the other 11.
     Your task is to identify which is the odd ball and whether 
     if it is heavy or light. You're only allowed to make 3 
     weighings using a balance.

   Challenge: 

     Start out not knowing how to solve the riddle 
     and write code that tells you how.

=head1 OUTPUT

 ./odd_ball

 Searching for odd ball in 12 balls with 3 weighings ...
   '' : at node=1 
      ... 1 partitions (4 L's, 0 different)
       'b' : at node=2 
          ... 11 partitions (1 L's, 4 different)
          ... 32 partitions (2 L's, 4 different)
           'bb' : at node=3 
              ... 2 partitions (1 L's, 1 different)
           'bl' : at node=7 
              ... 7 partitions (1 L's, 3 different)
           'br' : at node=11 
              ... 7 partitions (1 L's, 3 different)
       'l' : at node=15 
          ... 443 partitions (2 L's, 8 different)
          ... 37 partitions (1 L's, 8 different)
          ... 1610 partitions (3 L's, 8 different)
           'lb' : at node=16 
              ... 4 partitions (1 L's, 2 different)
           'll' : at node=20 
              ... 7 partitions (1 L's, 3 different)
           'lr' : at node=24 
              ... 7 partitions (1 L's, 3 different)
       'r' : at node=28 
          ... 443 partitions (2 L's, 8 different)
          ... 37 partitions (1 L's, 8 different)
          ... 1610 partitions (3 L's, 8 different)
           'rb' : at node=29 
              ... 4 partitions (1 L's, 2 different)
           'rl' : at node=33 
              ... 7 partitions (1 L's, 3 different)
           'rr' : at node=37 
              ... 7 partitions (1 L's, 3 different)
 done.  Solution found after 40 nodes and 2147 partitions.

 In what follows, (L,R,o) means 'Left', 'Right', and 'off scale',
 and (b,l,r) mean 'balanced', 'left tilt', and 'right tilt'.

 start => [ L L L L R R R R o o o o ]

     b => [ R o o o o o o o L L R o ]
     l => [ L R o o L L R R o o o o ]
     r => [ L L R R L R o o o o o o ]

    bb => [ R o o o o o o o o o o L ]
    bl => [ o o o o o o o o L R o o ]
    br => [ o o o o o o o o L R o o ]
    lb => [ o o L R o o o o o o o o ]
    ll => [ o o o o o o L R o o o o ]
    lr => [ o o o o L R o o o o o o ]
    rb => [ o o o o o o L R o o o o ]
    rl => [ o o L R o o o o o o o o ]
    rr => [ L R o o o o o o o o o o ]

   bbl => ball 12 is heavy.
   bbr => ball 12 is light.
   blb => ball 11 is light.
   bll => ball 9 is heavy.
   blr => ball 10 is heavy.
   brb => ball 11 is heavy.
   brl => ball 10 is light.
   brr => ball 9 is light.
   lbl => ball 3 is heavy.
   lbr => ball 4 is heavy.
   llb => ball 1 is heavy.
   lll => ball 8 is light.
   llr => ball 7 is light.
   lrb => ball 2 is heavy.
   lrl => ball 6 is light.
   lrr => ball 5 is light.
   rbl => ball 7 is heavy.
   rbr => ball 8 is heavy.
   rlb => ball 5 is heavy.
   rll => ball 4 is light.
   rlr => ball 3 is light.
   rrb => ball 6 is heavy.
   rrl => ball 2 is light.
   rrr => ball 1 is light.

 Double check : 
  scenario 0 gives rrr => 0 : OK
  scenario 1 gives rrl => 1 : OK
  scenario 2 gives rlr => 2 : OK
  scenario 3 gives rll => 3 : OK
  scenario 4 gives lrr => 4 : OK
  scenario 5 gives lrl => 5 : OK
  scenario 6 gives llr => 6 : OK
  scenario 7 gives lll => 7 : OK
  scenario 8 gives brr => 8 : OK
  scenario 9 gives brl => 9 : OK
  scenario 10 gives blb => 10 : OK
  scenario 11 gives bbr => 11 : OK
 Looks good.

=cut

# For a discussion and more output, 
# see the rest of the POD at the end of the file.

# -- global constants --

my $n_balls     = $ARGV[0] || 12;
my $n_weighings = $ARGV[1] || 3; 

my @types       = qw(L R o); # where a ball goes on scale: (Left, Right, off)
my @directions  = qw(b l r); # weighing returns (balanced, left, right)
my %heavy = ( L => 'l', R => 'r', o => 'b' ); # "weighing" is this mapping,
my %light = ( L => 'r', R => 'l', o => 'b' ); # e.g. light on Left tilts right.

# -- shared variables --

my $n_different;       # number of balls to permute in partitions
my $partitions;        # e.g. [ \qw(L L L o o o R R R ...), \qw(L o R ..), ..]
my %saved_partitions;  # memoize get_partitions
my $n_partitions = 0;  # total number of partitions in %saved_partitions.
my %balls;             # ( type => how many )  e.g. ( o=>6, L=>3, R=>3 )
my %solution;          # See get_solution, print_solution, double_check
my $n_nodes = 0;       # number of nodes examined during recursive search

# -- main --

if ($ARGV[0] and $ARGV[0] eq 'numbers'){
  ($n_balls, $n_weighings) = (12, 3);
  print_puzzle_numbers();
  exit;
}

print "\n Searching for odd ball in $n_balls balls ",
      "with $n_weighings weighings ...\n";

my $root = new_node('root');
my $soln_found = $root->search;

print " done.  Solution ", ($soln_found ? '' : 'not '), 
  "found after $n_nodes nodes and $n_partitions partitions.\n\n";

if ($soln_found){
  $root->get_solution;
  print_solution();
  double_check();
}

# -- subroutines --

# Generate or look up permutations of the weighing partitions where
#    i) each partition is an $n_ball element list of (L R o),
#   ii) each has the given same number of L's and R's, 
#  iii) the first $n_different balls cycle through all permutations, but
#   iv) the remaining identical balls are simply appended in alpha order.
# Usage:  $partitions = get_partitions($n_different, $n_left);
sub get_partitions {
  $n_different = shift;
  my $n_left = shift;
  my $key = $n_balls . ' ' . $n_different . ' ' . $n_left;
  # die "input out of bounds" if ($n_different<0 or $n_different>$n_balls);
  if (exists $saved_partitions{$key}){
    return $saved_partitions{$key};
  }
  elsif ($n_different == $n_balls){
    # same; no need to do both.
    my $key_other = $n_balls . ' ' . ($n_balls-1) . ' ' . $n_left;
    if (exists $saved_partitions{$key_other}){
      return $saved_partitions{$key_other};
    }
    else {
      return get_partitions($n_balls-1, $n_left);
    }
  }
  else {
    $partitions = [];  # Reset any previous result.
    %balls   = ( L => $n_left, R => $n_left, o => ($n_balls - 2*$n_left) );
    permute();       # Give permute an empty list to start its recursion.
    $saved_partitions{$key} = $partitions;  # Remember this result,
    $n_partitions += scalar @$partitions;   # note how many more we made,
    return $partitions;                     # and return it.
  }     
}

# Recursive routine to find permutations of partitions of the balls.
# Modifies shared variables %balls and $partitions.
# Input list is the parition being built up.
sub permute {
  my @partition = @_;
  if (@partition >= $n_different){  # If enough balls have been permuted, then
    for my $p (@partition){         #   see if first L or R is L.
      if    ($p eq 'R'){ return; }  #      Nope, found R first, so skip it.
      elsif ($p eq 'L'){ last;   }  #      Yup, found L first.
    }                               
    push @partition, map {($_)x$balls{$_}} @types;  # Append remaining balls
    push @$partitions, \@partition;                 # and save it.
  }     
  else {                            # Otherwise,
    for my $type (@types){          #   for each ball type ( e.g. qw(o L R) ),
      if ($balls{$type}){           #     if there are any of those remaining,
        $balls{$type}--;            #       remove one from those available
        permute(@partition, $type); #       and continue with it in the answer.
        $balls{$type}++;            #     To clean up, make it available again.
      }
    }
  }     
}

# Collapse a list of scenarios to a list of balls.
# For example, with $n_balls=12, (0,1, 13,15) = (0,1, 12+1,12+3) => (0,1,3),
# which would be the balls in ((ball 0 or 1 light) or (ball 1 or 3 heavy)).
# Usage: \@balls = scenarios2balls(\@scenarios);
sub scenarios2balls {
  my $scenarios = shift;
  my $balls = [];
  my %seen;
  for my $s (@$scenarios){
    my $ball = $s < $n_balls ? $s : $s - $n_balls;
    push @$balls, $ball unless $seen{$ball};
    $seen{$ball}=1;
  }
  return $balls;
}

# Input:  array reference $balls, e.g. [5,2]
# Output: reference to copy of array with other integers < $_balls appended.
# Example with $nballs=6 : append_remaining_balls([5,2]) returns [5,2,0,1,3,4]
sub append_remaining_balls {
  my $original_balls = shift;
  my $balls = [@$original_balls];
  my %in_input;
  $in_input{$_}=1 for @$balls;
  for my $digit (0..$n_balls-1){
    push @$balls, $digit unless $in_input{$digit};
  }
  return $balls;
}

# Usage : $direction = weigh($scenario, \@partition)
# For example, weigh(3, [qw(L L L R R R o o o o o o)]) returns 'l';
# the scale goes left when the ball in position 3 is light and on the Right.
sub weigh {
  my ($s, $partition) = @_;
  if ($s < $n_balls){                # ball $s is light
    return $light{$partition->[$s]};
  }  
  else {                             # ball $s-$n_balls is heavy
    return $heavy{$partition->[$s-$n_balls]};
  }     
}

# Input:  $depth, \%outcomes = {b => [scenarios], l=>[..], r=>[..]}
# Output: true if each of the (b l r) scenarios in \%outcomes have 
# fewer than the number of terminal nodes below this depth; otherwise, false.
sub viable {
  my ($depth, $outcomes) = @_;
  my $max = 3**($n_weighings - $depth - 1);
  for my $w (@directions){
    return 0 if scalar( @{$outcomes->{$w}} ) > $max;
  }
  return 1;
}

# Usage: my $node = new_node('root')
#             or  = new_node( path=>'...', scenarios=>[...], ... );
sub new_node {
  my $self;
  if ($_[0] eq 'root'){
    $self = { path=>'', scenarios=>[0..2*$n_balls-1], };
  }
  else {
    $self = { @_ };
  }
  $self->{depth} = length($self->{path});
  return bless $self, __PACKAGE__;  # OddBall package, actually.
}

# Pull interesting parts out of tree and put in %solution
# for print_analysis to display.
sub get_solution {
  my $self = shift;
  if ($self->at_bottom){
    my $scenario = $self->{scenarios}[0];
    $solution{$self->{path}} = $scenario if defined $scenario;
  }
  else {
    $solution{$self->{path}} = $self->{partition};
    for my $direction (@directions){
      $self->{$direction}->get_solution;
    }
  }
}

# Summarize solution stored in in %solution.
sub print_solution {
  print " In what follows, (L,R,o) means 'Left', 'Right', and 'off scale',\n";
  print " and (b,l,r) mean 'balanced', 'left tilt', and 'right tilt'.\n\n";
   for my $length (0..$n_weighings){
    for my $path (sort keys %solution){
      next unless length($path) == $length;
      if (length($path) == $n_weighings){
        print "   $path => " , scenario_as_text( $solution{$path} ), ".\n";
      }
      elsif (length($path) == 0){
        print " start => [ @{$solution{$path}} ]\n";
      }
      else {
        printf " %5s => [ %s ]\n", $path, "@{$solution{$path}}";
      }
    }
    print "\n";
  }
}

# Return string like "ball 0 is heavy" for scenarios 0..2*$n_balls-1.
sub scenario_as_text {
  my $scenario = shift;
  my ($ball, $weight) = $scenario < $n_balls ? 
                       ($scenario, 'light') : ($scenario-$n_balls, 'heavy');
  return "ball ", ($ball+1), " is $weight";
}

# Test %solution to see if it works.
sub double_check {
  my $OK = 1;
  print " Double check : \n";
  for my $scenario (0..$n_balls-1){
    my $path = '';
    while (length($path)<$n_weighings){
      $path .= weigh( $scenario, $solution{$path} );
    }
    print "  scenario $scenario gives $path => ", $solution{$path}, " : ";
    print $scenario == $solution{$path} ? "OK" : "** oops **";
    $OK = 0 unless $scenario == $solution{$path};
    print "\n";
  }
  print $OK ? " Looks good.\n\n" : " Oops - failed double check.\n\n"
}

# Usage: $child = $node->child( $direction, $outcomes );
sub child {
  my $self = shift;
  my ($direction, $outcomes) = @_;
  my $child_path = $self->{path} . $direction;
  my $child = new_node( path      => $child_path, 
                        scenarios => $outcomes->{$direction}, );
  $self->{$direction} = $child;
  return $child;
}

# Usage: $node->prepare_for_descent
sub prepare_for_descent {
  my $self = shift;
  $self->{balls}       = scenarios2balls($self->{scenarios});
  $self->{n_different} = $self->{depth} == 0 ? 0 : scalar @{$self->{balls}};
  $self->{rearrange}   = append_remaining_balls($self->{balls});
}

# Usage: if ($node->at_bottom){...}
sub at_bottom {
  my $self = shift;
  return $self->{depth} == $n_weighings;
}

# Usage: $outcome = $node->weigh_balls($partition)
# Returns nodes's scenarios placed into three possible outcomes given
# the balls placement on the scale as specified in $partition.
# The form of $outcome is { b=> [@scenarios], l=>[...], r=>[...] }.
sub weigh_balls {
  my ($self, $partition) = @_;
  my $rearrangement = $self->rearrange($partition);
  $self->{partition} = $rearrangement;  # we'll need this to generate sol'n.
  my $outcomes = { b=>[], l=>[], r=>[] };
  for my $s (@{$self->{scenarios}}){                  # Weigh balls.
    push @{$outcomes->{ weigh($s, $rearrangement) }}, $s;
  }
  return $outcomes;
}

# Put a partition into an order matching balls in a given node.
# Input and output are array references to a permutation of 0..$n_balls-1.
# Usage: $rearrage = $node->rearrange($partition);
sub rearrange {
  my ($self, $partition) = @_;
  my @rearrangement;
  @rearrangement[ @{ $self->{rearrange} }  ] = @$partition;
  return \@rearrangement;
}

# Given n, return (n, n+1, n-1, n+2, ...); lower limit 1, upper $n_balls/2,
# a guess at the order to search n_lefts.
# Usage: @order = $node->zig_zag_order($starting_integer)
sub zig_zag_order {
  my $i = shift;
  my $offset = 1;
  my @order;
  while ( 0<$i and $i<=$n_balls/2 ){
    push @order, $i;
    $i += $offset*(-1)**$offset;
    $offset++;
  }
  my $last = $order[-1] || 0;
  if ($i<1){
    push @order, $last+1 .. $n_balls/2;
  }
  else {
    push @order, reverse 1 .. $last-1;
  }
  return @order;
}

# Recursive search for solutions to the puzzle.
# Usage: $node->search;
sub search {
  my $self = shift;

  # print progress
  $n_nodes++;                           
  my $tab = " "x(3+4*$self->{depth});
  print "$tab'".$self->{path}."' : at node=$n_nodes \n" if $self->{depth}<3;

  return 1 if $self->at_bottom;  

  $self->prepare_for_descent;
  my $first_n_left = int((scalar @{$self->{balls}})/3) || 1;
  for my $n_left (zig_zag_order($first_n_left)){  
    my $partitions = get_partitions( $self->{n_different}, $n_left );

    print $tab.'   ... '.(scalar @$partitions)." partitions ($n_left L's, ".
      $self->{n_different} . " different)\n" if $self->{depth}<3;

    for my $partition (@$partitions){
      my $outcomes = $self->weigh_balls($partition);
      next unless viable($self->{depth}, $outcomes);    # Skip if too uneven.
      my $all_directions_true = 1;
      for my $direction (@directions){                  # Search below.
        my $child = $self->child($direction, $outcomes);
        $all_directions_true=0, last unless $child->search;
      }
      return 1 if $all_directions_true;
    }
  }
  return 0; # Failure: there isn't a solution that includes this node.
}

# -- analyzing the size of the puzzle --
# (The rest of the code isn't used in the search.)

# Print various details about the size of the search tree.
# Side-effect : generates all saved_partitions for current value of n_balls,
# which can take up a lot of space.  
# Usage: print_puzzle_numbers();
sub print_puzzle_numbers {
  my $n_interesting = 0;
  my $outcomes = 3**$n_weighings;
  my $n_groups = 3**$n_balls;
  my $n_nodes  = sum( map { 3**$_ } 0..2 );
  # n_strategies = n * ( n*(n+n+n) + n*(n+n+n) + n*(n+n+n) )
  #              = n*(3*n*(3*n)) = n_groups**n_weighins*3**(n_weighings-1)
  my $n_strategies = $n_groups**$n_weighings * 3**($n_weighings-1);
  my $bruteforce = sprintf("%.4g",$n_strategies);
  print "\n Puzzle Numbers

   With $n_balls balls there are 2*$n_balls = ", 2*$n_balls, " scenarios.
   Balls are weighed $n_weighings times, ",
   "so the number of outcomes is 3**$n_weighings = $outcomes.
   (No solution is possible when outcomes < scenarios.)

   The number of nodes in the tree is ",
   "{ sum(3**(i-1)) for i=1..$n_weighings } = $n_nodes.
   The number of ball partitions is 3**$n_balls = $n_groups.
   The number of brute force strategies is ($n_groups**$n_weighings)*(3**",
   $n_weighings-1,") = $bruteforce.
   e.g. for 12 balls, p*(p*(p+p+p)+p*(p+p+p)+p*(p+p+p))=(p**3)*(3**2).

   The partitions where L's = R's, displayed as ,
   'm: C($n_balls,m)*C($n_balls-m,m)', are 
     ";
  for my $m (1..($n_balls/2)){
    my $count = combinations($n_balls,$m)*combinations($n_balls-$m,$m); 
    $n_interesting += $count;
    print "$m: $count   ";
  }
  my $brute2=sprintf("%.4g",$n_interesting**$n_weighings*3**($n_weighings-1));
  print "
   The sum of these is $n_interesting.
   The number of strategies is then ($n_interesting**$n_weighings)*(3**",
   $n_weighings-1,") = $brute2.\n";

  # This is the slow step: generate and store all partitions.
  # It fills my 1 Gig of memory and dies for $n_balls > 20 or so.
  print "\n  Generating all partitions ... \n";
  my @n_perms;
  for my $different (0..$n_balls-1){
    $n_perms[$different] = 0;
    my $partitions;
    for my $Ls (1..($n_balls/2)){
      $partitions = get_partitions($different, $Ls);
      $n_perms[$different] += scalar @$partitions;
    }
  }
  print "  done; total stored = ", $n_partitions, ".\n";

  print "
   The number of partitions with (i) L's=R's, (ii) n balls taken as different,
   and (iii) leaving out those with first R before first L :\n";
  for my $frac (0..1){
    my @nums = $n_balls * $frac/2 .. $n_balls * ($frac+1)/2 - 1;
    printf " "x4 . "%2i: %-5i  "x@nums . "\n", map{$_,$n_perms[$_]} @nums;
  }
  print "   where each is a subset of those that follow.\n";
  my @diffs = map {$_==0 ? 0 : 3**($n_weighings-$_)} 0..$n_weighings-1;
  print "   Using the numbers of different balls vs depth as (@diffs),
   the number of strategies is ";
  my $total = 1;
  for my $w (0..$n_weighings-1){
    my $different =  $w==0 ? 0 : 3**($n_weighings-$w);
    print $n_perms[$different] . " * ";
    $total *= $n_perms[$different];
  }
  $total *= 3**($n_weighings-1);
  printf "3**%i = %.4g.\n\n", $n_weighings-1, $total;
}
# Return the sum of the elements in an array.
sub sum {
  my $sum = 0;
  $sum += $_ for @_;
  return $sum;
}
# C(n,m) = n!/(m! (n-m)!) = number of combinations of n things m at a time.
sub combinations {
  my ($n, $m) = @_;
  return -1 if $n<$m || $n<0 || $m<0;  # error conditions
  my ($top, $bottom) = (1,1);  # numerator, denominator of result.
  while ($m>0){
    $top *= $n--;              # n*(n-1)*(n-2)*...*(n-m+1) = n!/(n-m)!
    $bottom *= $m--;           # m*(m-1)*(m-2)*...*1
  }
  return $top/$bottom;
}

__END__


=head1 DESCRIPTION

The following is mustly just thinking out loud, so feel free to skip
ahead to whatever part of the code seems appropriate.  The execution
starts in the "Main" section, if that helps.

First, any one of 12 balls may be heavier or lighter than the others,
so there are 24 different scenarios that the program is to decide
between.

Since each of the 3 weighings gives one of 3 results, we have a tree
of possible outcomes :

                                 |
                                [1]
            +--------------------+--------------------+
            |balanced            |left                |right
            |                    |                    |
           [2]                  [3]                  [4]
     +------+------+      +------+------+      +------+------+     
     |b     |l     |r     |b     |l     |r     |b     |l     |r
     |      |      |      |      |      |      |      |      |
    [5]    [6]    [7]    [8]    [9]    [A]    [B]    [C]    [D]
   +-+-+  +-+-+  +-+-+  +-+-+  +-+-+  +-+-+  +-+-+  +-+-+  +-+-+
   b l r  b l r  b l r  b l r  b l r  b l r  b l r  b l r  b l r 

A strategy for weighing the balls will partition them into groups at
each node, specifiying which balls go on the left and right pans
depending on the results of the previous weighings.

For a given strategy (the ball partitions at each node), the
24 scenarios will filter through the tree, taking various paths to the
bottom (e.g. (b,b,b), (l,b,r), (l,r,l), ...), and end up distributed
across the 27 bottom branches.

A solution to the puzzle needs to avoid putting any two scenarios in
the same bottom node; otherwise, the correct ball and its weight is
not uniquely determined.  Therefore all we need is search the
strategies for those with this property.  The search here isn't
looking for a single node, but for a property of the whole tree.

The numbers turn out like this.  

  Puzzle Numbers

  With 12 balls there are 2*12 = 24 scenarios.
  Balls are weighed 3 times, so the number of outcomes is 3**3 = 27.
  (No solution is possible when outcomes < scenarios.)

  The number of nodes in the tree is { sum(3**(i-1)) for i=1..3 } = 13.
  The number of ball partitions is 3**12 = 531441.
  The number of brute force strategies is (531441**3)*(3**2) = 1.351e+18.
  e.g. for 12 balls, p*(p*(p+p+p)+p*(p+p+p)+p*(p+p+p))=(p**3)*(3**2).

  The partitions where L's = R's, displayed as 'm: C(12,m)*C(12-m,m)', are 
    1: 132   2: 2970   3: 18480   4: 34650   5: 16632   6: 924   
  The sum of these is = 73788.
  The number of strategies is then (73788**3)*(3**2) = 3.616e+15.

  The number of partitions with (i) L's=R's, (ii) n balls taken as different,
  and (iii) leaving out those with first R before first L :
    0: 6       1: 11      2: 26      3: 65      4: 168     5: 430    
    6: 1080    7: 2620    8: 6031    9: 12811  10: 24068  11: 36894  
  Using the numbers of different balls vs depth as (0 9 3),
  the number of strategies is 6 * 12811 * 65 * 3**2 = 4.497e+07.


Here are some ways to reduce the first naive estimate of the size of
the search space, 1e18, down to something manageable.

=over 4 

=item I)

Putting a different number of balls on the left and right pans doesn't
give us interesting information: for small weight differences, the pan
with more balls always goes down.  While this fact is one that a brute
force search could find, and which may therefore be part of what the
program is supposed to figure out, out, I'll assume otherwise, and put
that as part of the "built-in" knowledge.

=item II)

The intitial ball grouping can be permuted without disturbing the
overall strategy; therefore, we only need 6 (i.e. n_balls/2) different
initial groupings.  (In other words, none of the balls are different
from the others at the root node.)  To be more specific, and using a
notation that described below,

  $root_partitions = [  [ L  R  o  o  o  o  o  o  o  o  o  o ],
                        [ L  L  R  R  o  o  o  o  o  o  o  o ],
                         ...
                        [ L  L  L  L  L  L  R  R  R  R  R  R ] ];


=item III)

For the nodes below the top of the tree, we can use the fact that many
of the balls are no longer possible heavy or light candidates in any
remaining scenarios to remove permutations over identical balls.  For
example, at the bottom of the tree, say, node [5], there are at most 3
viable scenarios, implying at least 9 balls that are already known to
be of normal weight.  There's no need to look at different
permutations of those 9 balls; therefore we need look at only
(12-9)**3 = 27 different permutations for the terminal nodes.

=item IV)

Because the puzzle is symetric between left and right, a strategy for,
say, (l,l,b) will also work for (r,r,b) once all L's and R's are
swapped.  That lets us cut the original 13 nodes down to 7, as shown
below, where "*" means "use the earlier node with left and right
swapped".
                                    |
                                   [1]
               +--------------------+--------------------+
               |balanced            |left                |right
               |                    |                    |
              [2]                  [3]                   *
        +------+------+      +------+------+      
        |b     |l     |r     |b     |l     |r     
        |      |      |      |      |      |   
       [5]    [6]     *     [8]    [9]     *
      +-+-+  +-+-+         +-+-+  +-+-+  
      b l *  b l r         b l r  b l r  

=item V)

Besides this node left-right symmetry, there's also a partition
left-right symmetry, since swapping the the balls on the left and
right pans for any given weighing only swaps the parity of what
follows.  Therefore, without loss of generality, any partition such as
[R o R L L o ] can be replaced by [L o L R R o].  This cuts the number
of partitions in half, at least for those with $n_different != 0.

Putting together (I) through (V), and assuming 3 identical balls at
nodes [2] and [3], and 9 identical balls at nodes [5]..[9], an
exhaustive search need not consider more than [1] [2] [5] [6] [3] [8]
[9] ( 6 ) * ( 12811 * ( 65 + 65 ) + 12811 * ( 65 + 65 ) ) = 19_985_160
possible weighing schemes.

=item VI)

Moreover, only partitions which divide the scenarios fairly evenly
need be considered.  This pruning is applied top down, eliminating big
chunks of the search as we go.  For example, nodes [2], [3], and [4]
only have 9 terminal branches below them each; therefore, any weighing
scheme at node [1] that puts more than 9 possible scenarios in one of
the nodes below may be discarded without further search.  In fact, it
turns out that only one of the possible six weighings at the top of
the tree is viable, thus immediately pruning the tree by a factor of
six.  Note that this implies we shouldn't do a straight depth-first
search, but instead should look briefly across the (b l r) branches
before descending.

In practice this reduces the search drastically.

=back

The upshot of all this is that for twelve balls, the whole tree can
be searched to find the puzzle answers.  

As the number of balls increases, the search space will quickly get
too big for an exhaustive search: permutations are like that.
however, solutions can still probably be found by wandering through
some the strategies by using heuristics and a method like steepest
descent, monte-carlo, or a genetic algorithm.  genetic algorithms,
beam searches, and so on.  My guess is that this problem is similar to
eight-queens in that it has a number of possible solutions, and that a
reasonable heuristic (such as how evenly each node partitions the
scenarios) would let head towards a solution.  But I haven't done this
myself.

I wouldn't be surprized if there's an analytic constructive solution f
or arbitrary n that a literature search would reveal

The current permutation over partitions is problematic in this
implementation : for a given n_balls, n_different, n_left, I generate
all of them at once and save 'em in memory.  That gets too expensive
around n_balls=24 or so, at least with 1Gig of memory.  I had a hard
time coming up with an iterator to do the same thing, turning it into
a time rather than space problem.  I'm sure that can be done, but even
then I think an exhastive search is going to have problems fairly
quickly as n increases.

 Here are some questions to look at for $n_balls != 12.

=over 4

=item *

Can we decide 13 balls in 3 weighings, since 2*13=26 < 27=3**3 ?

Answer: no.  Best division is (L L L L R R R R o o o o o), which
leaves (8, 8, 10) but only 9 branches under each.

=item *

Is there always a solution when 3**n_weighings >= 2*$n_balls ?

Answer: no; 13 balls can't be solved with 3 weighings.

=item *

Given $n_weighings, what is the largest solvable $n_balls ?  
(Note: upper bound is balls <= int((3**weighings)/2).)

Answer: 

   weighings   max balls  upper bnd   
   ---------   ---------  ---------   
    2           3            4        
    3          12           13        
    4          20 < n       41        
    5           ?          121
    6           ?          354
    7           ?         1093        1093 factorial, anyone?

=back

How fast does the search space grow, and when does an exhaustive
search become impractical?  

Answer: Real fast. Exhaustive search -
even clever exhaustive search - fails fairly quickly.

Finally, a few specific definitions of terms, data formats, and
implementation notes.

=over 4

=item *

Balls are integers from 0 to $n_balls-1.

=item *

Scenarios are also integers, but they run from 0 to 2*$n_balls-1.  The
idea is that most of the time I want not just a ball, but also to say
whether its lighter or heavier.  I define one possibility (e.g. "ball
3 is heavy") to be a "scenario" : 0 <= $scenario < $n_balls means ball
($s) is light, $n_balls <= $scenario < 2*$n_balls means ball
($s-$n_balls) is heavy.  So (0..2*$n_balls-1) is a list of all the
2*$n_balls scenarios.

=item *

Partitions are $n_ball element arrays of the ball types (L R o),
(representing "Left pan", "Right pan", "off the scale") that specify
how the balls are to be weighed.  For example, putting the first six
of twelve balls on the left and right pans would be (L L L R R R o o o
o o o).

=item *

The symbols (b l r) mean "balanced", "left tilt", and "right tilt",
and give the possible outcomes of the weighings which are the
directions downward through the search tree.

=item *

When permuting a partition a distinction is made between balls which
are "different", i.e. not identical to each other; those are the only
ones that are cycled through the all permutations of the ball types.
The partition generation subroutines leave those "different" balls on
the left, allowing them to be remembered rather than recalculated.
When the partitions are actually used in the search node code, they
must be rearranged so that the balls which are still candidates for
being heavy or light are the ones that are cycled through all types.

=item *

The permutations are generated for a specific number of L's, so that
the others need not be generated and the search can be terminated
quicker if a success is found.  Also, I'm zig zagging the search order
of the number of L's, to try to find a solution quicker, starting with
1/3 the number of remaining interesting balls.

=back

=head1 MORE OUTPUT

=head2 ./odd_ball 4 2

 Searching for odd ball in 4 balls with 2 weighings ...
   '' : at node=1 
      ... 1 partitions (1 L's, 0 different)
      ... 1 partitions (2 L's, 0 different)
 done.  Solution not found after 1 nodes and 2 partitions.

=head2 ./odd_ball 3 2

 Searching for odd ball in 3 balls with 2 weighings ...
   '' : at node=1 
      ... 1 partitions (1 L's, 0 different)
       'b' : at node=2 
          ... 2 partitions (1 L's, 1 different)
           'bb' : at node=3 
           'bl' : at node=4 
           'br' : at node=5 
       'l' : at node=6 
          ... 3 partitions (1 L's, 2 different)
           'lb' : at node=7 
           'll' : at node=8 
           'lr' : at node=9 
       'r' : at node=10 
          ... 3 partitions (1 L's, 2 different)
           'rb' : at node=11 
           'rl' : at node=12 
           'rr' : at node=13 
 done.  Solution found after 13 nodes and 6 partitions.

 In what follows, (L,R,o) means 'Left', 'Right', and 'off scale',
 and (b,l,r) mean 'balanced', 'left tilt', and 'right tilt'.

 start => [ L R o ]

     b => [ R o L ]
     l => [ o L R ]
     r => [ L o R ]

   bl => ball 3 is heavy.
   br => ball 3 is light.
   lb => ball 1 is heavy.
   lr => ball 2 is light.
   rb => ball 2 is heavy.
   rr => ball 1 is light.

 Double check : 
  scenario 0 gives rr => 0 : OK
  scenario 1 gives lr => 1 : OK
  scenario 2 gives br => 2 : OK
 Looks good.

=head2 ./odd_ball 13 3

 Searching for odd ball in 13 balls with 3 weighings ...
   '' : at node=1 
      ... 1 partitions (4 L's, 0 different)
      ... 1 partitions (3 L's, 0 different)
      ... 1 partitions (5 L's, 0 different)
      ... 1 partitions (2 L's, 0 different)
      ... 1 partitions (6 L's, 0 different)
      ... 1 partitions (1 L's, 0 different)
 done.  Solution not found after 1 nodes and 6 partitions.

=head2 ./odd_ball 13 4

 Searching for odd ball in 13 balls with 4 weighings ...
   '' : at node=1 
      ... 1 partitions (4 L's, 0 different)
       'b' : at node=2 
          ... 16 partitions (1 L's, 5 different)
           'bb' : at node=3 
              ... 7 partitions (1 L's, 3 different)
           'bl' : at node=16 
              ... 4 partitions (1 L's, 2 different)
           'br' : at node=29 
              ... 4 partitions (1 L's, 2 different)
       'l' : at node=42 
          ... 443 partitions (2 L's, 8 different)
           'lb' : at node=43 
              ... 11 partitions (1 L's, 4 different)
           'll' : at node=56 
              ... 4 partitions (1 L's, 2 different)
           'lr' : at node=69 
              ... 4 partitions (1 L's, 2 different)
       'r' : at node=82 
          ... 443 partitions (2 L's, 8 different)
           'rb' : at node=83 
              ... 11 partitions (1 L's, 4 different)
           'rl' : at node=96 
              ... 4 partitions (1 L's, 2 different)
           'rr' : at node=109 
              ... 4 partitions (1 L's, 2 different)
 done.  Solution found after 121 nodes and 485 partitions.

 In what follows, (L,R,o) means 'Left', 'Right', and 'off scale',
 and (b,l,r) mean 'balanced', 'left tilt', and 'right tilt'.

 start => [ L L L L R R R R o o o o o ]

     b => [ o o o o o o o o L R o o o ]
     l => [ o o o o L L R R o o o o o ]
     r => [ L L R R o o o o o o o o o ]

    bb => [ o o o o o o o o o o L R o ]
    bl => [ o o o o o o o o R L o o o ]
    br => [ o o o o o o o o L R o o o ]
    lb => [ L R o o o o o o o o o o o ]
    ll => [ o o o o o o L R o o o o o ]
    lr => [ o o o o L R o o o o o o o ]
    rb => [ o o o o L R o o o o o o o ]
    rl => [ o o L R o o o o o o o o o ]
    rr => [ L R o o o o o o o o o o o ]

   bbb => [ R o o o o o o o o o o o L ]
   bbl => [ R o o o o o o o o o o L o ]
   bbr => [ R o o o o o o o o o L o o ]
   blb => [ L R o o o o o o o o o o o ]
   bll => [ L R o o o o o o o o o o o ]
   blr => [ R o o o o o o o o L o o o ]
   brb => [ L R o o o o o o o o o o o ]
   brl => [ L R o o o o o o o o o o o ]
   brr => [ R o o o o o o o L o o o o ]
   lbb => [ o o L R o o o o o o o o o ]
   lbl => [ L R o o o o o o o o o o o ]
   lbr => [ R L o o o o o o o o o o o ]
   llb => [ L R o o o o o o o o o o o ]
   lll => [ R o o o o o o L o o o o o ]
   llr => [ R o o o o o L o o o o o o ]
   lrb => [ L R o o o o o o o o o o o ]
   lrl => [ R o o o o L o o o o o o o ]
   lrr => [ R o o o L o o o o o o o o ]
   rbb => [ o o o o o o L R o o o o o ]
   rbl => [ R o o o L o o o o o o o o ]
   rbr => [ R o o o o L o o o o o o o ]
   rlb => [ L R o o o o o o o o o o o ]
   rll => [ R o o L o o o o o o o o o ]
   rlr => [ R o L o o o o o o o o o o ]
   rrb => [ L R o o o o o o o o o o o ]
   rrl => [ R L o o o o o o o o o o o ]
   rrr => [ L R o o o o o o o o o o o ]

   bbbl => ball 13 is heavy.
   bbbr => ball 13 is light.
   bblb => ball 11 is heavy.
   bblr => ball 12 is light.
   bbrb => ball 12 is heavy.
   bbrr => ball 11 is light.
   blrb => ball 9 is heavy.
   blrr => ball 10 is light.
   brrb => ball 10 is heavy.
   brrr => ball 9 is light.

   lbbl => ball 3 is heavy.
   lbbr => ball 4 is heavy.
   lbll => ball 1 is heavy.
   lbrl => ball 2 is heavy.
   lllr => ball 8 is light.
   llrr => ball 7 is light.
   lrlr => ball 6 is light.
   lrrr => ball 5 is light.
   rbbl => ball 7 is heavy.
   rbbr => ball 8 is heavy.
   rbll => ball 5 is heavy.
   rbrl => ball 6 is heavy.
   rllr => ball 4 is light.
   rlrr => ball 3 is light.
   rrlr => ball 2 is light.
   rrrr => ball 1 is light.

 Double check : 
  scenario 0 gives rrrr => 0 : OK
  scenario 1 gives rrlr => 1 : OK
  scenario 2 gives rlrr => 2 : OK
  scenario 3 gives rllr => 3 : OK
  scenario 4 gives lrrr => 4 : OK
  scenario 5 gives lrlr => 5 : OK
  scenario 6 gives llrr => 6 : OK
  scenario 7 gives lllr => 7 : OK
  scenario 8 gives brrr => 8 : OK
  scenario 9 gives blrr => 9 : OK
  scenario 10 gives bbrr => 10 : OK
  scenario 11 gives bblr => 11 : OK
  scenario 12 gives bbbr => 12 : OK
 Looks good.

=head2 ./odd_ball 20 4

 Searching for odd ball in 20 balls with 4 weighings ...
   '' : at node=1 
      ... 1 partitions (6 L's, 0 different)
       'b' : at node=2 
          ... 443 partitions (2 L's, 8 different)
           'bb' : at node=3 
              ... 11 partitions (1 L's, 4 different)
              ... 32 partitions (2 L's, 4 different)
           'bl' : at node=16 
              ... 11 partitions (1 L's, 4 different)
           'br' : at node=29 
              ... 11 partitions (1 L's, 4 different)
       'l' : at node=42 
          ... 85010 partitions (4 L's, 12 different)
           'lb' : at node=43 
              ... 11 partitions (1 L's, 4 different)
           'll' : at node=56 
              ... 4 partitions (1 L's, 2 different)
           'lr' : at node=69 
              ... 142 partitions (2 L's, 6 different)
       'r' : at node=82 
          ... 85010 partitions (4 L's, 12 different)
           'rb' : at node=83 
              ... 11 partitions (1 L's, 4 different)
           'rl' : at node=96 
              ... 4 partitions (1 L's, 2 different)
           'rr' : at node=109 
              ... 142 partitions (2 L's, 6 different)
 done.  Solution found after 121 nodes and 85653 partitions.

 In what follows, (L,R,o) means 'Left', 'Right', and 'off scale',
 and (b,l,r) mean 'balanced', 'left tilt', and 'right tilt'.

 start => [ L L L L L L R R R R R R o o o o o o o o ]

     b => [ o o o o o o o o o o o o L L R R o o o o ]
     l => [ R R o o o o L L L L R R o o o o o o o o ]
     r => [ L L L L R R R R o o o o o o o o o o o o ]

    bb => [ R o o o o o o o o o o o o o o o L L R o ]
    bl => [ o o o o o o o o o o o o o o L R o o o o ]
    br => [ o o o o o o o o o o o o L R o o o o o o ]
    lb => [ o o L R o o o o o o o o o o o o o o o o ]
    ll => [ o o o o o o o o o o L R o o o o o o o o ]
    lr => [ o o o o o o L L R R o o o o o o o o o o ]
    rb => [ o o o o o o o o L R o o o o o o o o o o ]
    rl => [ o o o o L R o o o o o o o o o o o o o o ]
    rr => [ L L R R o o o o o o o o o o o o o o o o ]

   bbb => [ R o o o o o o o o o o o o o o o o o o L ]
   bbl => [ o o o o o o o o o o o o o o o o L R o o ]
   bbr => [ o o o o o o o o o o o o o o o o L R o o ]
   blb => [ o o o o o o o o o o o o L R o o o o o o ]
   bll => [ R o o o o o o o o o o o o o o L o o o o ]
   blr => [ R o o o o o o o o o o o o o L o o o o o ]
   brb => [ o o o o o o o o o o o o o o L R o o o o ]
   brl => [ R o o o o o o o o o o o o L o o o o o o ]
   brr => [ R o o o o o o o o o o o L o o o o o o o ]
   lbb => [ o o o o L R o o o o o o o o o o o o o o ]
   lbl => [ R o L o o o o o o o o o o o o o o o o o ]
   lbr => [ R o o L o o o o o o o o o o o o o o o o ]
   llb => [ L R o o o o o o o o o o o o o o o o o o ]
   lll => [ R o o o o o o o o o o L o o o o o o o o ]
   llr => [ R o o o o o o o o o L o o o o o o o o o ]
   lrb => [ L R o o o o o o o o o o o o o o o o o o ]
   lrl => [ o o o o o o o o L R o o o o o o o o o o ]
   lrr => [ o o o o o o L R o o o o o o o o o o o o ]
   rbb => [ o o o o o o o o o o L R o o o o o o o o ]
   rbl => [ R o o o o o o o L o o o o o o o o o o o ]
   rbr => [ R o o o o o o o o L o o o o o o o o o o ]
   rlb => [ L R o o o o o o o o o o o o o o o o o o ]
   rll => [ R o o o o L o o o o o o o o o o o o o o ]
   rlr => [ R o o o L o o o o o o o o o o o o o o o ]
   rrb => [ o o o o o o L R o o o o o o o o o o o o ]
   rrl => [ o o L R o o o o o o o o o o o o o o o o ]
   rrr => [ L R o o o o o o o o o o o o o o o o o o ]

   bbbl => ball 20 is heavy.
   bbbr => ball 20 is light.
   bblb => ball 19 is light.
   bbll => ball 17 is heavy.
   bblr => ball 18 is heavy.
   bbrb => ball 19 is heavy.
   bbrl => ball 18 is light.
   bbrr => ball 17 is light.
   blbl => ball 13 is heavy.
   blbr => ball 14 is heavy.
   bllr => ball 16 is light.
   blrr => ball 15 is light.
   brbl => ball 15 is heavy.
   brbr => ball 16 is heavy.
   brlr => ball 14 is light.
   brrr => ball 13 is light.
   lbbl => ball 5 is heavy.
   lbbr => ball 6 is heavy.
   lbll => ball 3 is heavy.
   lbrl => ball 4 is heavy.
   lllr => ball 12 is light.
   llrr => ball 11 is light.
   lrbl => ball 1 is heavy.
   lrbr => ball 2 is heavy.
   lrll => ball 10 is light.
   lrlr => ball 9 is light.
   lrrl => ball 8 is light.
   lrrr => ball 7 is light.
   rbbl => ball 11 is heavy.
   rbbr => ball 12 is heavy.
   rbll => ball 9 is heavy.
   rbrl => ball 10 is heavy.
   rllr => ball 6 is light.
   rlrr => ball 5 is light.
   rrbl => ball 7 is heavy.
   rrbr => ball 8 is heavy.
   rrll => ball 4 is light.
   rrlr => ball 3 is light.
   rrrl => ball 2 is light.
   rrrr => ball 1 is light.

 Double check : 
  scenario 0 gives rrrr => 0 : OK
  scenario 1 gives rrrl => 1 : OK
  scenario 2 gives rrlr => 2 : OK
  scenario 3 gives rrll => 3 : OK
  scenario 4 gives rlrr => 4 : OK
  scenario 5 gives rllr => 5 : OK
  scenario 6 gives lrrr => 6 : OK
  scenario 7 gives lrrl => 7 : OK
  scenario 8 gives lrlr => 8 : OK
  scenario 9 gives lrll => 9 : OK
  scenario 10 gives llrr => 10 : OK
  scenario 11 gives lllr => 11 : OK
  scenario 12 gives brrr => 12 : OK
  scenario 13 gives brlr => 13 : OK
  scenario 14 gives blrr => 14 : OK
  scenario 15 gives bllr => 15 : OK
  scenario 16 gives bbrr => 16 : OK
  scenario 17 gives bbrl => 17 : OK
  scenario 18 gives bblb => 18 : OK
  scenario 19 gives bbbr => 19 : OK
 Looks good.

=head2 ./odd_ball 24 4

 Searching for odd ball in 24 balls with 4 weighings ...
   '' : at node=1 
      ... 1 partitions (8 L's, 0 different)
       'b' : at node=2 
          ... 443 partitions (2 L's, 8 different)
           'bb' : at node=3 
              ... 11 partitions (1 L's, 4 different)
              ... 32 partitions (2 L's, 4 different)
           'bl' : at node=16 
              ... 11 partitions (1 L's, 4 different)
           'br' : at node=29 
              ... 11 partitions (1 L's, 4 different)
       'l' : at node=42 
 perl(359) malloc: *** vm_allocate(size=8421376) failed (err code=3)
 perl(359) malloc: *** error: can't allocate region
 perl(359) malloc: *** set a breakpoint in szone_error to debug
 Out of memory!

=head1 AUTHOR

Jim Mahoney, Marlboro College (mahoney@marlboro.edu)

=head1 COPYRIGHT

Copyright 2005 Jim Mahoney

This program is free software; you may redistribute it and/or modify
it under the same terms as Perl itself.

