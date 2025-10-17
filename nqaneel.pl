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

% Simulated annealing algorithm to solve the 8-queen problem
simulated_annealing(Board, Solution) :-
    calculate_heuristic(Board, H),
    simulated_anneal(Board, H, 1000, Solution).

simulated_anneal(Board, 0, _, Board) :- !. % Solution found
simulated_anneal(Board, H, Temp, Solution) :-
    Temp > 0,
    get_neighbors(Board, Neighbors),
    random_member(Neighbor, Neighbors),
    calculate_heuristic(Neighbor, NeighborH),
    (NeighborH < H ->
        NewBoard = Neighbor,
        NewH = NeighborH
    ;
        DeltaH is NeighborH - H,
        P is exp(-DeltaH / Temp),
        (random_float < P ->
            NewBoard = Neighbor,
            NewH = NeighborH
        ;
            NewBoard = Board,
            NewH = H
        )
    ),
    NewTemp is Temp * 0.99,
    simulated_anneal(NewBoard, NewH, NewTemp, Solution).

% Solve the 8-queen problem
solve_8_queen(Solution) :-
    initialize_board(Board),
    simulated_annealing(Board, Solution).