class BackwardChaining:
    def __init__(self):
        self.rules = []
        self.facts = set()

    def add_rule(self, conditions, conclusion):
        self.rules.append((conditions, conclusion))

    def add_fact(self, fact):
        self.facts.add(fact)

    def backward_chain(self, goal):
        if goal in self.facts:
            return True

        for conditions, conclusion in self.rules:
            if conclusion == goal:
                if all(self.backward_chain(condition) for condition in conditions):
                    self.facts.add(goal)
                    print(f"Inferred: {goal}")
                    return True
        return False

# Example usage of backward chaining

bc = BackwardChaining()

# Defining rules
bc.add_rule(["bought_fruits"], "recommend_vegetables")
bc.add_rule(["added_dairy_to_cart"], "offer_discount_on_milk")
bc.add_rule(["spent_over_100"], "offer_free_delivery")
bc.add_rule(["regular_buyer", "bought_bread"], "recommend_jam")

# Adding initial facts
bc.add_fact("bought_fruits")
bc.add_fact("spent_over_100")

# Query: Should we recommend vegetables?
goal = "recommend_vegetables"
if bc.backward_chain(goal):
    print(f"Conclusion: Yes, {goal}")
else:
    print(f"Conclusion: No, {goal}")

# Output:
# Inferred: recommend_vegetables
# Conclusion: Yes, recommend_vegetables
