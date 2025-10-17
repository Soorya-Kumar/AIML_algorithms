"""
clause -> list of tuples (a,b,c)
    -ve symbol means negation
    example (1,2,-3) means x U y U not z
    
symbol -> list of symbols used [a,b,c,d...]
model  -> allocation of variables....
        True / False / -1
"""

def check_all_true(clause, symbol, model):
    for c in clause:
        satisfied = False
        for lit in c:
            if lit > 0 and model[abs(lit) - 1] == True:
                satisfied = True
                break
            elif lit < 0 and model[abs(lit) - 1] == False:
                satisfied = True
                break
        if not satisfied:
            return False
    return True

def check_any_false(clause, symbol, model):
    for c in clause:
        unsatisfied = True
        for lit in c:
            if lit > 0 and model[abs(lit) - 1] == True:
                unsatisfied = False
                break
            elif lit < 0 and model[abs(lit) - 1] == False:
                unsatisfied = False
                break
        if unsatisfied:
            return True
    return False

def find_pure_symbols(clause, symbol, model):
    pure_symbols = []
    pure_values = []
    for s in symbol:
        is_pure = True
        value = None
        for c in clause:
            if s in c:
                if value is None:
                    value = True
                elif value == False:
                    is_pure = False
                    break
            elif -s in c:
                if value is None:
                    value = False
                elif value == True:
                    is_pure = False
                    break
        if is_pure:
            pure_symbols.append(s)
            pure_values.append(value)
    return pure_symbols, pure_values

def find_unit_symbols(clause, symbol, model):
    unit_symbols = []
    unit_values = []
    for c in clause:
        unassigned = []
        for lit in c:
            if model[abs(lit) - 1] == -1:
                unassigned.append(lit)
        if len(unassigned) == 1:
            unit_symbols.append(abs(unassigned[0]))
            unit_values.append(unassigned[0] > 0)
    return unit_symbols, unit_values

def dpll(clause, symbol, model):
    print(f"Current model: {model}")
    if check_all_true(clause, symbol, model):
        print("Model:", model)
        return True
    if check_any_false(clause, symbol, model):
        return False

    pure_symbols, pure_values = find_pure_symbols(clause, symbol, model)
    for x, v in zip(pure_symbols, pure_values):
        model[abs(x) - 1] = v
        symbol.remove(x)
    if pure_symbols:
        return dpll(clause, symbol, model)

    unit_symbols, unit_values = find_unit_symbols(clause, symbol, model)
    for x, v in zip(unit_symbols, unit_values):
        model[abs(x) - 1] = v
        symbol.remove(x)
    if unit_symbols:
        return dpll(clause, symbol, model)

    first_var = symbol[0]
    rest_var = symbol[1:]

    return dpll(clause, rest_var, model[:abs(first_var) - 1] + [True] + model[abs(first_var):]) or dpll(clause, rest_var, model[:abs(first_var) - 1] + [False] + model[abs(first_var):])

# Example 1: Satisfiable
clauses_satisfiable = [[1, 2, 3], [-1, 2, 3], [1, -2, 3]]
symbol_satisfiable = [1, 2, 3]
model_satisfiable = [-1, -1, -1]

print("Example 1: Satisfiable")
ans_satisfiable = dpll(clauses_satisfiable, symbol_satisfiable, model_satisfiable)
print(ans_satisfiable)
if ans_satisfiable == True:
    print("Yes, the given expression is satisfiable by the DPLL algorithm")
else:
    print("No, it is not satisfiable")

# Example 2: Unsatisfiable
clauses_unsatisfiable = [[1, 2, 3], [-1, 2, 3], [1, -2, 3], [-1, -2, -3]]
symbol_unsatisfiable = [1, 2, 3]
model_unsatisfiable = [-1, -1, -1]

print("\nExample 2: Unsatisfiable")
ans_unsatisfiable = dpll(clauses_unsatisfiable, symbol_unsatisfiable, model_unsatisfiable)
print(ans_unsatisfiable)
if ans_unsatisfiable == True:
    print("Yes, the given expression is satisfiable by the DPLL algorithm")
else:
    print("No, it is not satisfiable")