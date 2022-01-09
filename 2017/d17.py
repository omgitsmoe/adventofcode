inp = 356
circ = [0]
current = 0

for n in range(1,2017+1):
    # step forward inp number of steps and the location after(+1) becomes pos of new insert
    # len(circ) == n
    current = (current+inp) % n + 1
    # inserts n at idx current
    circ.insert(current, n)
#print(circ[current-3:current+4])
print("part1:",circ[current+1])
# part2 whats after 0? -> index 1, 0 always stays at index 0
# storing and inserting in list is the slow part -> remove that part
# print inserted number when were on index 1; answer -> last number printed
current = 0
for n in range(1,50000000+1):
    # start at 1 and go to 50*10**6 (1,50*10**6+1) or
    # range(50*10**6): current = (current + inp) % (n+1) + 1
    # answer is n+1 then (last printed)
    current = (current+inp) % n + 1
    if current == 1:
        # even better to just assign to var and print after loop is done
        last_n_idx1 = n
        # print(n)
print("part2:", last_n_idx1)
