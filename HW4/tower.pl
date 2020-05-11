tower(N, T, counts(U, D, L, R)) :-
        length(T, N),
        maplist(length_wrapper(N), T),
        maplist(maplist(fd_domain_wrapper(1, N)), T),
        maplist(fd_all_different, T),
        transpose(T, NT),
        maplist(fd_all_different, NT),
        maplist(fd_labeling, T),
        match_rows_counts(T, L, R),
        match_rows_counts(NT, U, D).

fd_domain_wrapper(Low, High, List) :- fd_domain(List, Low, High).
length_wrapper(Length, List) :- length(List, Length).

plain_tower(N, T, counts(U, D, L, R)) :-
        maplist(length_wrapper(N), [U, D, L, R]),
        create_grid(T, N),
        match_rows_counts(T, L, R),
        transpose(T, NT),
        match_rows_counts(NT, U, D).

% verify that RL & L are transposes of each other
transpose(T, L) :- transpose(T, L, 0), !.
transpose(L, [], N):- length(L, N).
transpose([H|T], [L1|L], N) :-
        X is N + 1,
        maplist(nth(X), [H|T], L1),
        transpose([H|T], L, X),
        !.

% fill in a 2D array with lists of fixed length (N)
within_domain(N, Domain) :-
    findall(X, between(1, N, X), Domain).
fill_2d([], _).
fill_2d([Head | Tail], N) :-
    within_domain(N, Domain),
    permutation(Domain, Head),
    fill_2d(Tail, N).
create_grid(Grid, N) :-
    length(Grid, N),
    fill_2d(Grid, N).

% reverse each element list of a 2-D list
reverse_2d(X, RX) :-
    maplist(reverse, X, RX).

% verify that the number of visible towers from the right and left match
match_rows_counts(T, L, R) :-
        match_row_counts(T, L),
        reverse_2d(T, RT),
        match_row_counts(RT, R),
        !.
match_row_counts([], []).
match_row_counts([Row1|Rows], [Count1|Counts]) :-
        visible_count(Row1, Count1),
        match_row_counts(Rows, Counts),
        !.

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

ambiguous(N, C, T1, T2) :-
        N > 0,
        tower(N, T1, C),
        tower(N, T2, C),
        T1 \== T2.

speedup(FPR) :-
        cpu_time(Start_Time),
        tower(5, _, counts([5,3,3,2,1],[1,2,3,3,2],[5,3,3,2,1],[1,2,2,3,2])),
        cpu_time(Inter_Time),
        plain_tower(5, _, counts([5,3,3,2,1],[1,2,3,3,2],[5,3,3,2,1],[1,2,2,3,2])),
        cpu_time(End_Time),
        FPR is (End_Time - Inter_Time) / (Inter_Time - Start_Time).
