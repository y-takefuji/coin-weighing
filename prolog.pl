% The original program is at:https://binding-time.co.uk/index.php/The_Counterfeit_Coin_Puzzle
% It was modified by Takefuji: length->lengthh
% The Counterfeit Coin Puzzle
% ===========================
% 
% Problem Definition
% ------------------
% 
% We are given 12 apparently identical coins - one of which is
% counterfeit. We know that the counterfeit has a different weight from
% the others, but we don't know if it's heavier or lighter.
% 
% Task:
% 
% Devise a procedure to identify any counterfeit coin using a balance to
% take up to three comparative weighings.
% 
% Strategy
% --------
% 
% The information from three suitable weighings will make all but one of
% the coins, which are untested initially, known_true. There are three
% alternative deductions that make a coin known_true:
% 
% -   if it is not_heavy and not_light - having been on both the
%     comparatively lighter and heavier sides of imbalances;
% -   if it was excluded from an imbalance;
% -   if it was included in a balanced weighing.
% 
% After three weighings there must be exactly one coin, the counterfeit,
% which is not known_true. If the counterfeit is not known_true and
% not_heavy we deduce that it must be light. If it is not known_true and
% not_light, it must be heavy.
% 
% We use a generate-and-test method as follows:
% 
% -   Create the set of all possible counterfeits: 12 coins × 2 weights ;
% -   Devise a procedure that can identify the first counterfeit coin;
% -   Show that the same procedure works for every other counterfeit coin.
% 
% Entry Point
% -----------
% 
% go
% 
% is the entry point. It solves the puzzle, then uses a DCG to
% pretty-print the resulting procedure.

go :-
     coins_puzzle( Procedure ),
     phrase( general_explanation( Procedure ), Chars ),
     put_chars( Chars ).

% coins_puzzle( ?Procedure )
% 
% generates the set of all possible counterfeit coins and finds, or
% proves, that Procedure can identify them all.

coins_puzzle( Procedure ) :-
     coins( Coins ),
     counterfeit( Counterfeit, Coin, Weight ),
     findall(
         Counterfeit,
         (member(Coin,Coins), counterfeit_weight(Weight)),
         Counterfeits
         ),
     coins_puzzle_solution( Counterfeits, Procedure ).

coins_puzzle_solution( [], _Procedure ).
coins_puzzle_solution( [Counterfeit|Counterfeits], Procedure ) :-
     solve_coins( Counterfeit, Procedure ),
     coins_puzzle_solution( Counterfeits, Procedure ).

% A procedure is either done, identifying a particular coin and whether it
% is heavy or light, or it is a step.
% 
% A step defines the coins to be placed on the Left and Right pans, with
% the residue remaining on the Table, and three Branches, one of which
% will be chosen depending on the outcome of the weighing.

step( step(Left,Right,Table,Branches), Left, Right, Table, Branches).

% The Branches are three procedures equating to:
% 
% -   > (left pan heavier),
% -   < (right pan heavier) and
% -   = (pans balance).

branch( >, branches(_Equal, GT, _LT), GT ).
branch( <, branches(_Equal, _GT, LT), LT ).
branch( =, branches(Equal, _GT, _LT), Equal ).

% The counterfeit is defined by its number and whether it is heavy or
% light.

counterfeit( counterfeit(Coin, HeavyOrLight), Coin, HeavyOrLight ).

coins( [1,2,3,4,5,6,7,8,9,10,11,12] ).

counterfeit_weight( heavy ).
counterfeit_weight( light ).

% A coin collection comprises four parts (subsets): the known_true,
% not_heavy, not_light and untested sets of coins.

part( known_true, collection(Coins,_,_,_), Coins ).
part( not_heavy, collection(_,Coins,_,_), Coins ).
part( not_light, collection(_,_,Coins,_), Coins ).
part( untested, collection(_,_,_,Coins), Coins ).

% Solution
% --------
% 
% solve_coins( +Counterfeit, ?Procedure )
% 
% holds when Procedure can correctly identify the Counterfeit coin.
% Beginning with a start collection, in which all the coins are untested,
% the Procedure comprises three steps.
% 
% For each step, a weighing is made and a branch is made in the Procedure,
% depending on the result of the weighing.
% 
% After three steps, the Procedure must have reached the end condition.
% 
% Finally, an assertion (redundant test) ensures that the Procedure has
% found the correct end condition.

solve_coins( Counterfeit, Procedure ) :-
     start( Coins0 ),
     assay( Counterfeit, Coins0, Procedure,  Branch1, Coins1 ),
     assay( Counterfeit, Coins1, Branch1, Branch2, Coins2 ),
     assay( Counterfeit, Coins2, Branch2, done(Coin, HeavyOrLight), Coins3 ),
     end( Coins3, Coin, HeavyOrLight ),
     counterfeit( Counterfeit, Coin, HeavyOrLight ).

start( Coins ) :-
     coins( Untested ),
     part( untested, Coins, Untested ),
     part( not_heavy, Coins, [] ),
     part( not_light, Coins, [] ),
     part( known_true, Coins, [] ).

end( Coins, Coin, HeavyOrLight ) :-
     part( untested, Coins, [] ),
     part( not_heavy, Coins, Light ),
     part( not_light, Coins, Heavy ),
     end_result( Heavy, Light, Coin, HeavyOrLight ).

end_result( [Coin], [], Coin, heavy ).
end_result( [], [Coin], Coin, light ).

% Simulating the weighing process
% 
% assay( +Counterfeit, +Coins0, ?Step, ?Branch, ?Coins1 )
% 
% holds when the appropriate Branch from Step is chosen by comparing the
% weights of two coin collections taken from the full set of coins:
% Coins0. Coins1 is the full set of coins updated with the inferences
% drawn from the weighing, where Counterfeit is used to determine the
% result of the weighing.
% 
% This predicate applies the critical insight into the solution of this
% puzzle: we have 24 (12 × 2) possible inputs to the procedure, with only
% 27 (3 × 3 × 3) possible outcomes from the weighings.
% 
% Therefore, it is clear that each weighing must have a very high
% information content. Choosing which weighing to make by estimating the
% available information content makes the problem tractable.

assay( Counterfeit, Coins0, Step, Branch, Coins ) :-
    step( Step, Left, Right, Table, Branches ),
    partition( Coins0, Left, Right, Table ),
    balance( Left, Right, Counterfeit, Result ),
    draw_inferences( Result, Left, Right, Table, Coins ),
    branch( Result, Branches, Branch ).

% balance( +Left, +Right, +Counterfeit, ?Result )
% 
% holds when Result simulates the outcome of testing the coin collections
% Left and Right with a balance, where either may contain the Counterfeit
% coin.

balance( Left, Right, Counterfeit, Result ):-
    counterfeit( Counterfeit, Coin, Weight ),
    ( contains_coin( Left, Coin ) ->
        balance_result( Weight, normal, Result )
    ; contains_coin( Right, Coin ) ->
        balance_result( normal, Weight, Result )
    ; otherwise ->
        Result = '='
    ).

balance_result( light, normal, < ).
balance_result( heavy, normal, > ).
balance_result( normal, heavy, < ).
balance_result( normal, light, > ).

% Choosing which coins to weigh
% 
% partition( +Coins, ?Left, ?Right, ?Table )
% 
% holds when Coins is partitioned into three collections: Left side, Right
% side and Table.
% 
% Operationally, alternative valid partitions are selected in order of
% reducing information content.

partition( Coins, Left, Right, Table ) :-
    Partition = ptn(Info,Left,Right,Table),
    findall(
        Partition,
        valid_partition(Coins, Info, Left, Right, Table),
        Partitions
        ),
    sort( Partitions, OrderedPartitions ),
    member( Partition, OrderedPartitions ).

% valid_partition( +Coins, ?Content, ?Left, ?Right, ?Table )
% 
% holds when Coins can be partitioned into three collections: Left side,
% Right side and Table; with the information content of the partition
% given by Content.
% 
% The definition of a valid_partition ensures that:
% 
% -   Left and Right must have the same number of coins (at least one);
% -   Left cannot contain any known_true coins, because adding true coins
%     to both sides creates redundant comparisons;
% -   Comparisons between mixtures of coins where only the choice of pans
%     is different are equivalent, so a partial order (>=) on mixtures of
%     coins is used to eliminate some redundant comparisons.

valid_partition( Coins, Content, Left, Right, Table ):-
    part( known_true, Left, [] ),
    selection( Coins, Left, Coins1, LeftSum, LeftInfo ),
    selection( Coins1, Right, Table, RightSum, RightInfo ),
    LeftSum =:= RightSum,
    LeftSum @>= RightSum,
    table_information( Table, TableInfo ),
    sum( [LeftInfo,RightInfo,TableInfo], Content ).

% selection( +Coins, ?Sample, ?Residue, ?Sum, ?Content )
% 
% holds when Coins is partitioned into two collections: Sample and
% Residue. Sum is used as both a fingerprint for the mixture of coins in
% Sample and a representation of the number of coins in Sample. Content
% estimates the information-content of Sample.

selection( Coins, Sample, Residue, Sum, Content ) :-
    Sum = Count1+Count2+Count3+Count4,
    select_coins( not_heavy, Coins, Sample, Residue, Count1 ),
    select_coins( not_light, Coins, Sample, Residue, Count2 ),
    select_coins( untested, Coins, Sample, Residue, Count3 ),
    select_coins( known_true, Coins, Sample, Residue, Count4 ),
    Sum >= 1,
    Sum =< 6,
    information_content( [Count1,Count2,Count3,Count4], Content ).

% table_information( +Coins, ?Content )
% 
% holds when Coins has 'information content' Content.

table_information( Coins, Content ) :-
    count_coins( not_heavy, Coins, Count1 ),
    count_coins( not_light, Coins, Count2 ),
    count_coins( untested, Coins, Count3 ),
    count_coins( known_true, Coins, Count4 ),
    information_content( [Count1,Count2,Count3,Count4], Content ).

count_coins( Part, Coins, Count ) :-
    part( Part, Coins, Selection ),
    lengthh( Selection, Count ).

% Updating what is known about the coins
% 
% draw_inferences( +Result, +Left, +Right, +Table, ?Coins )
% 
% holds when Result is one of:
% 
% -   > (imbalance - left pan heavier),
% -   < (imbalance - right pan heavier) or
% -   = (pans balanced)
% 
% from taking a weighing with the coin collections: Left, Right and Table.
% 
% Coins is derived from this information using the following rules:
% 
% -   If the pans are unbalanced then only the previously untested or
%     not_heavy coins on the lighter side of the balance are now
%     not_heavy. Similarly, only the previously untested or not_light
%     coins on the heavier side of the balance are now not_light. All the
%     coins on the table are now known_true.
% -   If the pans balance then all the coins weighed are known_true, with
%     the coins on the Table left in their prior states.

draw_inferences( <, Left, Right, Table, Coins ) :-
     imbalance_inferences( Left, Right, Table, Coins ).
draw_inferences( >, Left, Right, Table, Coins ) :-
     imbalance_inferences( Right, Left, Table, Coins ).
draw_inferences( =, Left, Right, Table, Coins ) :-
     becomes( [all(Left),known_true(Table),all(Right)], known_true( Coins ) ),
     becomes( untested(Table), untested(Coins) ),
     becomes( not_heavy(Table), not_heavy(Coins) ),
     becomes( not_light(Table), not_light(Coins) ).

% imbalance_inferences( +Lighter, +Heavier, +Table, ?Coins )
% 
% holds when:
% 
% -   Only the untested or not_heavy coins in Lighter are not_heavy in
%     Coins;
% -   Only the untested or not_light coins in Heavier are not_light in
%     Coins;
% -   All the other coins in Lighter and Heavier and all the coins in
%     Table are known_true in Coins;
% 
% There are no untested coins in Coins.

imbalance_inferences( Lighter, Heavier, Table, Coins ) :-
     becomes( [untested(Lighter),not_heavy(Lighter)], not_heavy(Coins) ),
     becomes( [untested(Heavier),not_light(Heavier)], not_light(Coins) ),
     becomes( [
         known_true(Lighter),
         not_light(Lighter),
         known_true(Heavier),
         not_heavy(Heavier),
         all(Table)
         ],
         known_true(Coins)
     ),
     part( untested, Coins, [] ).

% becomes( +CollectionA, ?CollectionB )
% 
% CollectionA becomes (part) CollectionB when CollectionB comprises the
% same coins as CollectionA.

becomes( CollectionA, CollectionB ) :-
     unfolded( CollectionA, Coins ),
     unfolded( CollectionB, Coins ).

% unfolded( +Collection, ?Coins )
% 
% holds when (part) Collection comprises Coins.

unfolded( not_light(Collection), Coins ) :-
    part( not_light, Collection, Coins ).
unfolded( not_heavy(Collection), Coins ) :-
    part( not_heavy, Collection, Coins ).
unfolded( known_true(Collection), Coins ) :-
    part( known_true, Collection, Coins ).
unfolded( untested(Collection), Coins ) :-
    part( untested, Collection, Coins ).
unfolded( all(Collection), Coins ) :-
    collection_to_set( Collection, Coins ).
unfolded( [Item|Items], Coins ) :-
    unfolded( Item, Value ),
    unfolded1( Items, Value, Coins ).

unfolded1( [], Coins, Coins ).
unfolded1( [Item|Items], Value, Coins ) :-
    unfolded( Item, Value0 ),
    ord_union( Value0, Value, Value1 ),
    unfolded1( Items, Value1, Coins ).

% Definite Clause Grammar
% -----------------------
% 
% The following DCG presents the method for finding the counterfeit coin
% as a structured procedure.

general_explanation( Procedure ) -->
     "Number the coins 1..12", newline,
     explanation( Procedure, 0 ).

explanation( done(Coin, Weight), N ) -->
     tab( N ),
     "Conclude that the counterfeit coin is number ", literal( Coin ),
     ", which is ", literal( Weight ), newline.
explanation( Step, N ) -->
     {step( Step, Left, Right, Table, Branches )},
     tab( N ), "BEGIN", newline,
     tab( N ), "Put ", literal( Left ), " on the left-hand pan", newline,
     tab( N ), "Put ", literal( Right ), " on the right-hand pan", newline,
     tab( N ), "Leaving ", literal( Table ), " on the table", newline,
     branches_explained( Branches, N ),
     tab( N ), "END", newline.

branches_explained( Branches, N ) -->
     next_step_explained( <, Branches, N ),
     next_step_explained( >, Branches, N ),
     next_step_explained( =, Branches, N ).

next_step_explained( Result, Branch, N ) -->
     {branch( Result, Branch, Step )},
     ( {var(Step)} ->
         ""
     | {nonvar(Step)} ->
         tab( N ), "If the ", literal( Result ), " then:", newline,
         explanation( Step, s(N) )
     ).

literal( 0 ) --> "0".
literal( 1 ) --> "1".
literal( 2 ) --> "2".
literal( 3 ) --> "3".
literal( 4 ) --> "4".
literal( 5 ) --> "5".
literal( 6 ) --> "6".
literal( 7 ) --> "7".
literal( 8 ) --> "8".
literal( 9 ) --> "9".
literal( 10 ) --> "10".
literal( 11 ) --> "11".
literal( 12 ) --> "12".
literal( true ) --> "true".
literal( heavy ) --> "heavy".
literal( light ) --> "light".
literal( = ) -->
     "pans balance".
literal( < ) -->
     "right-hand pan is heavier".
literal( > ) -->
     "left-hand pan is heavier".
literal( Collection ) -->
     {collection_to_set( Collection , [H|T] )},
     literal_set( T, H ).

literal_set( [], Number ) -->
     "the coin numbered ", literal( Number ).
literal_set( [H|T], Number ) -->
     "the coins numbered ", literal( Number ),
     literal_set1( T, H ).

literal_set1( [], Number ) -->
     " and ", literal( Number ).
literal_set1( [H|T], Number ) -->
     ", ", literal( Number ),
     literal_set1( T, H ).

tab( 0 ) --> "".
tab( s(N) ) -->
     "   ", tab( N ).

newline --> "
".

% Utility Predicates
% 
% contains_coin( ?Collection, ?Coin )
% 
% holds when Coin is a member of Collection.

contains_coin( Collection, Coin ) :-
     part( _Part, Collection, Coins ),
     member( Coin, Coins ).

% collection_to_set( +Collection, ?Set )
% 
% holds when Set is the distributed union of the known_true, not_heavy,
% not_light and untested ordsets comprising Collection.

collection_to_set( Collection, Set ) :-
     part( known_true, Collection, KnownTrue ),
     part( not_heavy, Collection, NotHeavy ),
     part( not_light, Collection, NotLight ),
     part( untested, Collection, Untested ),
     ord_union( [KnownTrue,NotHeavy,NotLight,Untested], Set ).

% Information Content
% 
% information_content( +Counts, ?Content )
% 
% holds when Content is the cumulative negative entropy of Counts. A
% reduction in entropy equates to a gain in information.

information_content( Counts, Content ) :-
    information_content1( Counts, 0, Content ).

information_content1( [], Content, Content ).
information_content1( [Count|Counts], Content0, Content ):-
    coins_entropy( Count, Entropy ),
    Content1 is Content0-Entropy,
    information_content1( Counts, Content1, Content ).

% coins_entropy( ?N, ?Entropy )
% 
% holds when Entropy estimates the entropy of a sample of N coins.
% 
% Entropy = P log2(^1/P), where P = N÷12.

coins_entropy(  0, 0.0 ).
coins_entropy(  1, 0.2987 ).
coins_entropy(  2, 0.4308 ).
coins_entropy(  3, 0.5 ).
coins_entropy(  4, 0.5283 ).
coins_entropy(  5, 0.5263 ).
coins_entropy(  6, 0.5 ).
coins_entropy(  7, 0.4536 ).
coins_entropy(  8, 0.39 ).
coins_entropy(  9, 0.3113 ).
coins_entropy( 10, 0.2192 ).
coins_entropy( 11, 0.1151 ).
coins_entropy( 12, 0.0 ).

% select_coins( +Part, +Coins, ?Sample, ?Residue, ?N )
% 
% holds when N coins from Part of Coins form Part of Sample, with the
% remainder forming Part of Residue.

select_coins( Part, Coins, Sample, Residue, Count ) :-
    part( Part, Coins, Input ),
    part( Part, Sample, Selection ),
    part( Part, Residue, Remainder ),
    select_n( Count, Input, Selection, Remainder ).

select_n( 0, In, [], In ).
select_n( 1, [A|Suffix], [A], Suffix ).
select_n( 2, [A,B|Suffix], [A,B], Suffix ).
select_n( 3, [A,B,C|Suffix], [A,B,C], Suffix ).
select_n( 4, [A,B,C,D|Suffix], [A,B,C,D], Suffix ).
select_n( 5, [A,B,C,D,E|Suffix], [A,B,C,D,E], Suffix ).
select_n( 6, [A,B,C,D,E,F|Suffix], [A,B,C,D,E,F], Suffix ).

% Load a small library of Puzzle Utilities.

:- ensure_loaded( misc ).

% Use the ordsets library.

:- use_module( library(ordsets), [ord_union/2,ord_union/3] ).

