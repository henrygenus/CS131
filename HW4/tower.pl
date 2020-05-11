% find the counts C for an NXN tower grid which has two ambiguous solutions T1 & T2
ambiguous(N, C, T1, T2) :-
        N > 0,
        tower(N, T1, C),
        tower(N, T2, C),
        T1 \== T2.

% compute the ratio of the speed of plain_tower & tower
speedup(FPR) :-
        cpu_time(Start_Time),
        tower(5, _, counts([5,3,3,2,1],[1,2,3,3,2],[5,3,3,2,1],[1,2,2,3,2])),
        cpu_time(Inter_Time),
        plain_tower(5, _, counts([5,3,3,2,1],[1,2,3,3,2],[5,3,3,2,1],[1,2,2,3,2])),
        cpu_time(End_Time),
        Denominator is Inter_Time - Start_Time,
        Numerator is End_Time - Inter_Time,
        FPR is Numerator / Denominator,
        !.

% SOLUTION THAT UTILIZES FINITE DOMAIN SOLVER
tower(N, T, counts(U, D, L, R)) :-
        length(T, N),
        maplist(length_wrapper(N), T),
        maplist(maplist(fd_domain_wrapper(1, N)), T),
        maplist(fd_all_different, T),
        transpose(T, NT),
        maplist(fd_all_different, NT),
        maplist(fd_labeling, T),
        maplist(verify_counts, T, L, R),
        maplist(verify_counts, NT, U, D).

fd_domain_wrapper(Low, High, List) :- fd_domain(List, Low, High).
length_wrapper(Length, List) :- length(List, Length).
% verify that TL & L are transposes of each other
transpose(TL, L) :- transpose(TL, L, 0), !.
transpose(TL, [], N):- length(TL, N).
transpose(TL, [L1|L], N) :-
        X is N + 1,
        maplist(nth(X), TL, L1),
        transpose(TL, L, X),
        !.

% SOLUTION WITHOUT USING SOLVER
plain_tower(N, T, counts(U, D, L, R)) :-
        create_grid(T, N, L, R),
        match_col_counts(T, U, D).

% fill in a 2D array with lists of fixed length (N)
within_domain(N, Domain) :-
    findall(X, between(1, N, X), Domain).
fill_2d([], _, [], []).
fill_2d([Head | Tail], N, [LCount1|LCounts], [RCount1|RCounts]) :-
    within_domain(N, Domain),
    permutation(Domain, Head),
    verify_counts(Head, LCount1, RCount1),
    fill_2d(Tail, N, LCounts, RCounts).
create_grid(Grid, N, L, R) :-
    length(Grid, N),
    fill_2d(Grid, N, L, R).


% verify that the visible number of towers from the top and bottom of a column is correct
match_col_counts(T, U, D) :- match_col_counts(T, U, D, 0).
match_col_counts(L, [], [], N) :- length(L, N).
match_col_counts(T, [UCount1|UCounts], [DCount1|DCounts], X) :-
        Y is X + 1,
        maplist(nth(Y), T, Col),
        verify_counts(Col, UCount1, DCount1),
        all_unique(Col),
        match_col_counts(T, UCounts, DCounts, Y).

% test uniqueness of elements of a list
all_unique([]).
all_unique([H|T]) :- exists(H, T), !, fail.
all_unique([_|T]) :- all_unique(T).
exists(X, [X|_]).
exists(X, [_|T]) :-
	exists(X, T).

% verify the counts on either end of the list of towers are correct
verify_counts(List, Pre_Count, Post_Count) :-
        visible_count(List, Pre_Count),
        reverse(List, Tsil),
        visible_count(Tsil, Post_Count).
% verify the number of visible towers in row [H|T] is N
visible_count([H|T], N) :-
        visible(T, [H], Result),
        length(Result, N),
        !.
visible([], Result, Result).
visible([H|T], [F|R], Result) :-
        H > F,
        visible(T, [H, F | R], Result),
        !.
visible([H|T], [F|R], Result) :-
        H =< F,
        visible(T, [F|R], Result),
        !.
