import re

with open("d9_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read()

# inp = """{}
# {{{}}}
# {{},{}}
# {{{},{},{{}}}}
# {<{},{},{{}}>}
# {<a>,<a>,<a>,<a>}
# {{<a>},{<a>},{<a>},{<a>}}
# {{<!>},{<!>},{<!>},{<a>}}""".splitlines()
def p1(ln):
    # match ! and any first char following it
    cancel_char_re = re.compile(r"!.")
    # match garbage
    garbage_re = re.compile(r"<.*?>")
    # cancel first since ! and canceled char dont count towards garbage nr and ! alos cancels other >
    ln_canceled = re.sub(cancel_char_re, "", ln)
    # <> dont count towards garbage nr -> nr_subs*2 == nr of <> removed
    ln_wo_garbage, nr_subs = re.subn(garbage_re, "", ln_canceled)
    print("part2, total garbage:", len(ln_canceled)-len(ln_wo_garbage)- 2*nr_subs)
    # CAREFUL was removing commas first -> resulted in e.g. "!,:" canceling : instead of ,
    # could leave this out and just use elif instead of else below
    ln_new = ln_wo_garbage.replace(",", "")
    score = 0
    level = 0
    # print(ln_new)
    for char in ln_new:
        # every { opens new level
        if char == "{":
            level += 1
        # we only have { and } left in str, every } closes a level -> level-1 and add score
        else:
            score += level 
            level = max(level -1, 0)

    print(score)

p1(inp)
