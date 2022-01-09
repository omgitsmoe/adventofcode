# -*- coding: utf-8 -*-
def readfile(fname):
    with open(fname, "r", encoding="UTF-8") as f:
        return f.read()

inp = readfile("d4_input.txt")

def p1():
    lines = inp.splitlines()
    lines = [ln.split() for ln in lines]
    # Denis Otkidach https://stackoverflow.com/questions/1541797/check-for-duplicates-in-a-flat-list
    # set removes duplicates -> check if nr of entries are the same (no cuplicates) -> if not not a valid passphrase
    # faster since c code of set() filters the duplicates
    valid = [passphrase for passphrase in lines if len(set(passphrase)) == len(passphrase)]
    print(len(valid))

# valid passphrase mustnt contain two words that are anagrams of each other
# abcde fghij -> valid
# abcde xyz ecdab -> invalid

def p2_sort():
    lines = inp.splitlines()
    lines = [ln.split() for ln in lines]
    # sort string (->list) and convert to str again by joining on empty str -> anagrams will be duplicates
    line_str_vals = [["".join(sorted(s)) for s in ln] for ln in lines]
    # filter duplicates
    valid = [passphrase for passphrase in line_str_vals if len(set(passphrase)) == len(passphrase)]
    #print(len(valid))
p2_sort()

def p2():
    lines = inp.splitlines()
    lines = [ln.split() for ln in lines]
    # i could just sort the chars but thats not efficient
    # opt2: use Counter from collections to count chars in word and return dict that is converted to a frozenset (immutable hashable set) and gets added to set, that set will contains all frozensets with charcounts from all the words in a passphrase/line, while adding to that set (.add()) check if frozenset with same wwordcount alrdy in set: (by r_u_Cole_from_SE)
    # def soln(pwd, hashfn):
        # words = set()
        # for word in pwd.split(' '):
        #     hashable = hashfn(word)
        #     if hashable in words:
        #         return False
        #     words.add(hashable)
        # return True
    # counter_hash = lambda x: frozenset(Counter(x).items())
    # alle lines durchgehen und für jeden str in der ln die ascii value für jeden char im string aufsummieren -> selbe buchstaben im string (anagramm) gleiche summe ABER auch bei ungleichen buchstaben kann es die selbe summe sein
    # ord: it is used to convert character to its ascii values ..
    # my solution with ord wouldnt work even after checking if the sorted words matched, since i only got the first index of the charsum but there might be more matching csums in the list, continuing would have made no sense since even this solution already took longer than just sorting the chars (see below)
    #print(valid)

# import timeit
# print(timeit.timeit(p2_sort, number=100))
# print(timeit.timeit(p2, number=100))
# result: 0.7358505168448354, 0.9378904543522949
