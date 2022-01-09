# actual input = 047801
inp = 47801

recipes = [3, 7]

elf1 = 0
elf2 = 1
# do inp nr of recipes and then 10 more
while len(recipes) < inp + 10:
    recipe_sum = recipes[elf1] + recipes[elf2]
    digits = str(recipe_sum)
    # each digit is a new recipe that get added to end of list
    for digit in digits:
        recipes.append(int(digit))
    # assign new recipes: their current recipe index + (score of current recipe + 1)
    elf1 = (elf1 + recipes[elf1] + 1) % len(recipes)
    elf2 = (elf2 + recipes[elf2] + 1) % len(recipes)
print("Part1:", "".join(str(r) for r in recipes[inp:]))

match = "047801"
match_i = 0
recipes = [3, 7]

elf1 = 0
elf2 = 1
# do inp nr of recipes and then 10 more
while True:
    recipe_sum = recipes[elf1] + recipes[elf2]
    digits = str(recipe_sum)
    # each digit is a new recipe that get added to end of list
    for digit in digits:
        recipes.append(int(digit))
        if digit == match[match_i]:
            # advance current match index
            match_i += 1
        else:
            # no match reset match index
            match_i = 0
        # complete match
        if match_i == len(match):
            print(recipes[-len(match):])
            # print nr of recipes that came before match
            print("Part2:", len(recipes) - len(match))
            break
    # just for breaking outer loop
    if match_i == len(match):
        break
    # assign new recipes: their current recipe index + (score of current recipe + 1)
    elf1 = (elf1 + recipes[elf1] + 1) % len(recipes)
    elf2 = (elf2 + recipes[elf2] + 1) % len(recipes)
