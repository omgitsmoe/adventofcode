import re
import operator
import dataclasses

from typing import Callable

WORKFLOW_RE = re.compile(
    r"(?P<workflow_name>[a-zAR]+)\{"
    r"(?P<rules>([^,]+,)+)"
    r"(?P<default_workflow>[a-zAR]+)}")

RULE_RE = re.compile(
    r"(?P<category>x|m|a|s)"
    r"(?P<op>[<>])"
    r"(?P<operand>\d+):"
    r"(?P<workflow_if_true>[a-zAR]+),")

PARTS_RE = re.compile(r"{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}")


@dataclasses.dataclass
class Rule:
    cat: str
    operator: Callable[[int, int], bool]
    operand: int
    workflow_if_true: str


@dataclasses.dataclass
class Part:
    x: int
    m: int
    a: int
    s: int


with open("d19_input.txt", "r") as f:
    lines = f.readlines()

workflows = {}
# base case for recursion
workflows["R"] = lambda x: False
workflows["A"] = lambda x: True

# for part2
workflows_rules = {}


# parse a workflow string to a function that goes throug all the rules
# and recurses based on those rules
def parse_workflow(line) -> tuple[str, Callable[[Part], bool], list[Rule]]:
    match = WORKFLOW_RE.match(line)
    if not match:
        raise ValueError(f"Failed to match line: {line}")
    workflow_name = match.group("workflow_name")
    default_workflow = match.group("default_workflow")
    rules = []
    for rule in RULE_RE.findall(match.group("rules")):
        # NOTE: findall only returns group of matched tuples
        cat, op, operand, workflow_if_true = rule
        # use python operator as function
        op_func = operator.gt if op == ">" else operator.lt

        rules.append(Rule(cat, op_func, int(operand), workflow_if_true))

    def workflow(x: Part) -> bool:
        # print(x, workflow_name)
        for rule in rules:
            cat = getattr(x, rule.cat)
            if rule.operator(cat, rule.operand):
                # print(cat, rule.operator, rule.operand)
                # recurse
                return workflows[rule.workflow_if_true](x)
            else:
                pass
                # print(cat, 'not', rule.operator, rule.operand)
        # print('default', default_workflow)
        # recurse
        return workflows[default_workflow](x)

    # so we don't mess wih the closure
    rules_pt2 = rules.copy()
    # default case for pt2
    rules_pt2.append(Rule('x', lambda a, b: True, 0, default_workflow))

    return workflow_name, workflow, rules_pt2


matching_workflows = True
parts = []
for line in lines:
    line = line.strip()
    if not line:
        matching_workflows = False
        continue
    if matching_workflows:
        name, workflow, rules_pt2 = parse_workflow(line)
        workflows[name] = workflow
        workflows_rules[name] = rules_pt2
    else:
        x, m, a, s = [int(x) for x in PARTS_RE.match(line).groups()]
        parts.append(Part(x, m, a, s))

print("Part1:", sum((p.x + p.m + p.a + p.s)
                    if workflows["in"](p) else 0 for p in parts))


# NOTE: loops don't create a scope!?!?!?
def accepting_paths() -> list[list[tuple[str, int]]]:
    # iterate through the workflows and build paths that lead to
    # accepting a part
    paths: list[list[tuple[str, int]]] = []
    stack = [[("in", i)] for i in range(len(workflows_rules["in"]))]
    while stack:
        cur_path = stack.pop()
        cur_workflow, cur_rule_idx = cur_path[-1]
        next_workflow = (
                workflows_rules[cur_workflow][cur_rule_idx].workflow_if_true)
        workflow_q_rules: list[Rule] = workflows_rules[next_workflow]
        # queue the individual rules
        for i, rule in enumerate(workflow_q_rules):
            if rule.workflow_if_true == "R":
                continue

            to_q = cur_path + [(next_workflow, i)]
            if rule.workflow_if_true == "A":
                paths.append(to_q)
                continue
            else:
                stack.append(to_q)
    return paths


@dataclasses.dataclass
class Range:
    min: int
    max_excl: int

    def lt(self, operand: int) -> list["Range"]:
        if self.min >= operand:
            return []
        else:
            return [Range(self.min, min(self.max_excl, operand))]

    def gt(self, operand: int) -> list["Range"]:
        if (self.max_excl - 1) <= operand:
            return []
        else:
            return [Range(max(self.min, operand + 1),
                          self.max_excl)]

    def mod(self, op: Callable[[int, int],
            bool], operand: int, applies: bool) -> list["Range"]:
        # whether the op(x, operand) should return True
        if applies:
            if op == operator.lt:
                return self.lt(operand)
            elif op == operator.gt:
                return self.gt(operand)
            else:
                return [self]
        else:
            # reverse lt -> >= / gt -> <=
            # due to equal we need to bias by 1
            if op == operator.lt:
                return self.gt(operand - 1)
            elif op == operator.gt:
                return self.lt(operand + 1)
            else:
                raise RuntimeError("applies must be true if op isn't in <,>")


paths = accepting_paths()
# iterate through a path and cut the ranges of accepted numbers
# based on rules up to and including the rule index on that
# workflow
accepted_per_path = []
for path in paths:
    accepted = {k: [Range(1, 4001)] for k in ("x", "m", "a", "s")}
    # print('acc', path)
    for workflow_name, rule_idx in path:
        # print('wfname', workflow_name)
        cur_workflow = workflows_rules[workflow_name]
        for i, rule in enumerate(cur_workflow[:rule_idx + 1]):
            # print('r', i, rule)
            applies = True if i == rule_idx else False
            old_ranges = accepted[rule.cat]
            new_ranges = []
            for r in old_ranges:
                new_ranges.extend(r.mod(rule.operator, rule.operand, applies))
            accepted[rule.cat] = new_ranges
        # print('acc w/ rules', accepted)
    accepted_per_path.append(accepted)


# merge accpeted ranges so we don't count them multiple times
# WRONG: should already be distinct since we start at the same path
#        and always exclude the ranges that would've taken us to another
#        workflow
# -> sum up combinations of x/a/m/s directly
total_combinations = 0
for accepted in accepted_per_path:
    combinations = 1
    for ranges in accepted.values():
        distinct_numbers = sum(r.max_excl - r.min for r in ranges)
        combinations *= distinct_numbers
    total_combinations += combinations

print("Part2:", total_combinations)
