:- use_module(library(random)).

% Initialize the board with random positions for each queen
initialize_board(Board) :-
    findall(X, (between(1, 8, _), random_between(1, 8, X)), Board).

% Calculate the heuristic value (number of attacking pairs)
calculate_heuristic(Board, H) :-
    findall(1, (nth1(I, Board, Q1), nth1(J, Board, Q2), I < J, (Q1 =:= Q2 ; abs(Q1 - Q2) =:= abs(I - J))), Attacks),
    length(Attacks, H).

% Generate all neighbors by moving each queen to every other position in its column
get_neighbors(Board, Neighbors) :-
    findall(Neighbor, (nth1(I, Board, Q), between(1, 8, NewQ), NewQ \= Q, replace(Board, I, NewQ, Neighbor)), Neighbors).

% Replace the I-th element of a list with a new element
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

% Hill climbing algorithm to solve the 8-queen problem
hill_climbing(Board, Solution) :-
    calculate_heuristic(Board, H),
    hill_climb(Board, H, Solution).

hill_climb(Board, 0, Board) :- !. % Solution found
hill_climb(Board, H, Solution) :-
    get_neighbors(Board, Neighbors),
    find_best_neighbor(Neighbors, BestNeighbor, BestH),
    BestH < H, !,
    hill_climb(BestNeighbor, BestH, Solution).

% Find the best neighbor with the lowest heuristic value
find_best_neighbor([Neighbor], Neighbor, H) :-
    calculate_heuristic(Neighbor, H).
find_best_neighbor([Neighbor|Rest], BestNeighbor, BestH) :-
    find_best_neighbor(Rest, TempBestNeighbor, TempBestH),
    calculate_heuristic(Neighbor, H),
    (H < TempBestH -> (BestNeighbor = Neighbor, BestH = H) ; (BestNeighbor = TempBestNeighbor, BestH = TempBestH)).

% Solve the 8-queen problem
solve_8_queen(Solution) :-
    initialize_board(Board),
    hill_climbing(Board, Solution), !.