class ForwardChaining:
    def __init__(self):
        self.rules = []
        self.facts = set()

    def add_rule(self, conditions, conclusion):
        self.rules.append((conditions, conclusion))

    def add_fact(self, fact):
        self.facts.add(fact)

    def infer(self):
        new_facts = True
        while new_facts:
            new_facts = False
            for conditions, conclusion in self.rules:
                if all(condition in self.facts for condition in conditions) and conclusion not in self.facts:
                    print(f"Inferred: {conclusion}")
                    self.facts.add(conclusion)
                    new_facts = True

# Example usage of forward chaining

fc = ForwardChaining()

# Defining rules
fc.add_rule(["bought_fruits"], "recommend_vegetables")
fc.add_rule(["added_dairy_to_cart"], "offer_discount_on_milk")
fc.add_rule(["spent_over_100"], "offer_free_delivery")
fc.add_rule(["regular_buyer", "bought_bread"], "recommend_jam")

# Adding initial facts (data about the customer)
fc.add_fact("bought_fruits")
fc.add_fact("spent_over_100")

# Inference: Applying rules to infer new facts
fc.infer()

# Output:
# Inferred: recommend_vegetables
# Inferred: offer_free_delivery
