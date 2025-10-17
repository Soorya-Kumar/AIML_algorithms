distance(0,1,6).
distance(1,0,6).
distance(0,3,15).
distance(3,0,15).
distance(0,2,5).
distance(2,0,5).
distance(1,2,20).
distance(2,1,20).
distance(2,3,10).
distance(3,2,10).
distance(1,3,7).
distance(1,3,7).

calculate_heuristic(CurrentCity, RemainingCities, Heuristic) :-
    findall(Cost, (member(City, RemainingCities), distance(CurrentCity, City, Cost)), Costs),
    min_list(Costs, Heuristic).

astar(StartCity, Path, Cost) :-
    findall(City, distance(_, City, _), Cities),
    list_to_set(Cities, AllCities),
    astar_search([state(StartCity, [StartCity], 0, 0)], AllCities, Path, Cost).

astar_search([state(City, Path, G, F) | _], AllCities, FinalPath, FinalCost) :-
    length(Path, N),
    length(AllCities, N),
    distance(City, 0, ReturnCost),
    FinalCost is G + ReturnCost,
    reverse([0 | Path], FinalPath).

astar_search([state(City, Path, G, F) | Rest], AllCities, FinalPath, FinalCost) :-
    findall(state(NextCity, [NextCity | Path], NewG, NewF),
            (member(NextCity, AllCities),
             \+ member(NextCity, Path),
             distance(City, NextCity, Cost),
             NewG is G + Cost,
             subtract(AllCities, [NextCity], RemainingCities),
             calculate_heuristic(NextCity, RemainingCities, H),
             NewF is NewG + H),
            Children),
    append(Rest, Children, Open),
    sort(4, @=<, Open, SortedOpen),
    astar_search(SortedOpen, AllCities, FinalPath, FinalCost).

solve_tsp(StartCity, Path, Cost) :-
    astar(StartCity, Path, Cost).
