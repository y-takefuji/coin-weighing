% The original program is at:https://binding-time.co.uk/index.php/The_Counterfeit_Coin_Puzzle
% The program was modified by takefuji: length-> lengthh
% Puzzle Utilities
% ================
% 
% The following predicates are used in the puzzle solutions.
% 
% Contents
% --------
% 
% -   1 Higher-order Predicates
%     -   1.1 unique_solution( +Goal )
%     -   1.2 forall( +Enumerator, +Test )
%     -   1.3 count_solutions( +Goal, ?Count )
% -   2 Lists
%     -   2.1 member( ?Element, ?List )
%     -   2.2 select( ?Element, ?List0, ?List1 )
%     -   2.3 memberchk( +Element, +List )
%     -   2.4 append( ?Front, ?Back, ?List )
%     -   2.5 length( ?List, ?N )
% -   3 Arithmetic
%     -   3.1 between( +Lower, +Upper, ?Index )
%     -   3.2 sum( +List, ?Sum )
% -   4 Character Input/Output
%     -   4.1 put_chars( +Chars )
%     -   4.2 get_chars( ?Chars )
% 
% Higher-order Predicates
% -----------------------
% 
% unique_solution( +Goal )
% 
% holds when Goal has one ground solution. Operationally, Goal may produce
% several solutions, ("don't care" non-deterministically), but they must
% all be identical (==).

unique_solution( Goal ) :-
    findall( Goal, Goal, [Solution|Solutions] ),
    same_solution( Solutions, Solution ),
    Solution = Goal.

same_solution( [], _Solution ).
same_solution( [Solution0|Solutions], Solution ) :-
    Solution0 == Solution,
    same_solution( Solutions, Solution ).

% forall( +Enumerator, +Test )
% 
% is true if Enumerator and Test are goals and Test holds everywhere that
% Enumerator does. NB: forall/2 does not instantiate arguments further.

forall( Enumerator, Test ) :-
    \+ (call(Enumerator), \+ call(Test)).

% count_solutions( +Goal, ?Count )
% 
% is true if Count is the number of solutions for Goal. The solutions
% might not be distinct.
% 
% count_solutions/2 enumerates the possible solutions to Goal but does not
% instantiate Goal's arguments further.

count_solutions( Goal, Count ) :-
    findall( x, Goal, Xs ),
    lengthh( Xs, Count ).

% Lists
% -----
% 
% member( ?Element, ?List )
% 
% holds when Element is a member of List.

member( H, [H|_] ).
member( H, [_|T] ) :-
    member( H, T ).

% select( ?Element, ?List0, ?List1 )
% 
% is true if List1 is equal to List0 with Element removed.

select( H, [H|T], T ).
select( Element, [H|T0], [H|T1] ) :-
    select( Element, T0, T1 ).

% memberchk( +Element, +List )
% 
% succeeds (once) if Element is a member of List.

memberchk( Element, List ) :-
    member( Element, List ),
    !.

% append( ?Front, ?Back, ?List )
% 
% succeeds if Front, Back and List are all lists and List is the
% concatenation of Front and Back.

append( [], L, L ).
append( [H|T], L, [H|L1] ) :-
    append( T, L, L1 ).

% length( ?List, ?N )
% 
% succeeds if N is the length of List.

lengthh( List, N ) :-
    len1( List, 0, N ).

len1( [], N, N ).
len1( [_H|T], N0, N ) :-
    N1 is N0+1,
    len1( T, N1, N ).

% Arithmetic
% ----------
% 
% between( +Lower, +Upper, ?Index )
% 
% is true if Lower =< Index =< Upper. Two valid cases are possible:
% 
% -   Index is already instantiated to an integer, so the checks on order
%     are applied (test).
% -   Index is a logical variable: a series of alternative solutions may
%     be generated as the monotonic sequence of values between Lower and
%     Upper (non-deterministic generator).

between( Lower, Upper, Index ) :-
    integer( Lower ),
    integer( Upper ),
    Lower =< Upper,
    ( integer( Index ) ->    % Case 1: "test"
        Index >= Lower,
        Index =< Upper
    ; var( Index ) ->        % Case 2: "generate".
        generate_between( Lower, Upper, Index )
    ).

generate_between( Lower, Upper, Index ) :-
    ( Lower =:= Upper ->
        Index = Lower
    ;   Index = Lower
    ;   Next is Lower + 1,
        Next =< Upper,
        generate_between( Next, Upper, Index )
    ).

% sum( +List, ?Sum )
% 
% holds when the List of numbers sum to Sum.

sum( [H|T], Sum ) :-
    sum1( T, H, Sum ).

sum1( [], Sum, Sum ).
sum1( [H|T], Sum0, Sum ):-
    Sum1 is Sum0 + H,
    sum1( T, Sum1, Sum ).

% Character Input/Output
% ----------------------
% 
% put_chars( +Chars )
% 
% if Chars is a (possibly empty) list of character codes and the
% corresponding characters are written to the current output stream.

put_chars( [] ).
put_chars( [Char|Chars] ) :-
    put( Char ),
    put_chars( Chars ).

% get_chars( ?Chars )
% 
% if Chars is a (possibly empty) list of character codes read from the
% current input stream.

get_chars( Input ) :-
    get0( Char ),
    ( Char > -1 ->
        Input = [Char|Chars],
        get_chars( Chars )
    ; otherwise ->
        Input = []
    ).

