def parse_clause(clause):
    """Parse a clause into a set of literals."""
    return set(clause.split())

def resolve(clause1, clause2):
    """Resolve two clauses."""
    resolvents = []
    for literal in clause1:
        if literal.startswith('~'):
            complement = literal[1:]
        else:
            complement = '~' + literal
        if complement in clause2:
            new_clause = (clause1 - {literal}) | (clause2 - {complement})
            if not is_tautology(new_clause):
                resolvents.append(frozenset(new_clause))
    return resolvents

def is_tautology(clause):
    """Check if a clause is a tautology."""
    for literal in clause:
        if literal.startswith('~'):
            complement = literal[1:]
        else:
            complement = '~' + literal
        if complement in clause:
            return True
    return False

def resolution(clauses):
    """Apply the resolution rule iteratively."""
    clauses = [frozenset(parse_clause(clause)) for clause in clauses]
    new = set()
    while True:
        n = len(clauses)
        pairs = [(clauses[i], clauses[j]) for i in range(n) for j in range(i + 1, n)]
        for (ci, cj) in pairs:
            resolvents = resolve(ci, cj)
            if frozenset() in resolvents:
                return True
            new.update(resolvents)
        if new.issubset(set(clauses)):
            return False
        for clause in new:
            if clause not in clauses:
                clauses.append(clause)

# Example usage
clauses1 = [
    "P ~Q",
    "~P R",
    "~Q ~R"
]

clauses2 = [
    "P",
    "~P"
]

print(resolution(clauses1))  # Output: True or False
print(resolution(clauses2))  # Output: True or False