with open("d7_input.txt", "r", encoding="UTF-8") as f:
    inp = f.read()
# inp = """pbga (66)
# xhth (57)
# ebii (61)
# havc (66)
# ktlj (57)
# fwft (72) -> ktlj, cntj, xhth
# qoyq (66)
# padx (45) -> pbga, havc, qoyq
# tknk (41) -> ugml, padx, fwft
# jptl (61)
# ugml (68) -> gyxo, ebii, jptl
# gyxo (61)
# cntj (57)"""
inp = inp.splitlines()

def p2():
    prog_dict = {}
    for ln in inp:
        if "->" in ln:
            fi, se = ln.split(" -> ")
            name, wght = fi.split()
            wght = int(wght.replace(")","").replace("(",""))
            prog_dict[name] = [wght, se.split(", ")]
        else:
            name, wght = ln.split()
            wght = int(wght.replace(")","").replace("(",""))
            prog_dict[name] = [wght, None]
    wght_dict = {}
    # root == hlhomy
    # recursive func starting at root
    wght_rec(prog_dict, wght_dict, "hlhomy")

def wght_rec(prog_dict, wght_dict, current):
    # we dont need wght_dict with recursive func just use return vals
    wght, childs = prog_dict[current]

    # wenn node keine kinder -> val in wght_dict and return wght
    if childs is None:
        wght_dict[current] = wght
        return wght
    else:
        try:
            # i think this is obsolete since this value wont ever be set
            return wght_dict[current]
        except KeyError:
            wght_sum = 0
            prev = None
            missmatch = False
            for child in childs:
                # func returns wght if it is an end node or if alrdy in whgt_dict
                # check if same wghts after since otherwise not in wght dict yet
                # call this func again on child -> recursive
                wght_new = wght_rec(prog_dict, wght_dict, child)
                # None is retruned on missmatch -> break out of func
                if wght_new is None:
                    return
                wght_sum += wght_new

                # check if wghts match (using wght_dict so we access wght sums, but not needed since wght_rec alrdy returns wght_sum if node has childs)
                if prev and (wght_dict[prev] != wght_dict[child]):
                    print(f"Found missmatching weights in {childs}!")
                    missmatch = True
                prev = child
            if missmatch:
                # list of wght sums
                wghtlist = [wght_dict[child] for child in childs]
                # most occurring wght in list -> balanced/correct wght
                wght_balanced = max(set(wghtlist), key=wghtlist.count)
                # least occurring wght in list -> error
                wght_err = min(set(wghtlist), key=wghtlist.count)
                # get index of wght_err in wghtlist -> same index of program with wrong weight in child list
                node_err = childs[wghtlist.index(wght_err)]
                node_err_wght = prog_dict[node_err][0]
                # result is sum of wrong wght of the node and the diff between wrong and correct weight sums
                result = node_err_wght + (wght_balanced - wght_err)
                # max(set(lst), key=lst.count) returns most occurring element in lst
                print(f"Missmatch! {node_err} weight would need to be (diff: {wght_balanced-wght_err}): {result}\nComplete weight list: {wghtlist}")
                return None

            wght_dict[current] = wght_sum + wght
            return (wght_sum + wght)
p2()
#print(li)
def p1():
    prog_dict = {}
    li = []
    for ln in inp:
        if "->" in ln:
            fi, se = ln.split(" -> ")
            name, wght = fi.split()
            wght = int(wght.replace(")","").replace("(",""))
            li.append([name, wght, se.split(", ")])
        else:
            name, wght = ln.split()
            wght = int(wght.replace(")","").replace("(",""))
            li.append([name, wght])
    parents = [p for p in li if len(p) > 2]
    print(parents)
    i = 0
    n = 0
    # start at beginning then search if that program is a child of another, continue searching
    # till program without parent -> root is found
    while True:
        # print(i,n)
        # check if i-th program name is in childs of n-th element
        if parents[i][0] in parents[n][2]:
            # true -> cant be root, search for parents of n-th element next
            i = n
            n = 0
        else:
            n = (n+1)
            if n == len(parents):
                # end of list reached and program wasnt found in childs -> its root
                break
    print(parents[i][0]) # i 149

# p1()

# found = False
# li_end_new = []
# if False:
#     while not found:
#         for i, pn in enumerate(li):
#             # print(li_end,i)
#             # print(li_end[0][0])
#             # print(li)
#             # wenn n-ter program name von end liste in verlinkungen vom i-ten eintrag in li ist
#             if li_end[0][0] in pn[2]: 
#                 wg_li = None
#                 # verlinkungen des gefundenen eintrags durchegehen
#                 for prog in li[i][2]:
#                     # ueber alle listeneintraege von end liste iterieren
#                     a = 0
#                     while not found:
#                         if a >= len(li_end):
#                             print(prog, a, len(li_end))#, li_end)
#                         # wenn name des eintrags in endlist mit verlinkung in li uebereinstimmt
#                         if prog == li_end[a][0]:
#                             if wg_li:
#                                 if wg_li != li_end[a][1]:
#                                     found = True
#                                     print("found", wg_li - li_end[a][1])
#                                     break
#                             wg_li = li_end[a][1]
#                             if li_end[a][0] == "wzcgi":
#                                 print("match")
#                             del li_end[a]
#                             break
#                         else:
#                             a += 1
#                 # print("weight", wg_li)
#                 li[i][1] = wg_li * len(li[i][2]) + li[i][1]
#                 # gefundene base program wird zu neuem teil der endliste
#                 li.append(li[i])
#                 break
#         if len(li_end) == 0:
#             li_end = li_end_new#break
#             li_end_new = []
