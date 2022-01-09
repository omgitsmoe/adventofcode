# INP: Generator A starts with 703
# INP: Generator B starts with 516

# Generatoren speichern ihren lokalen Zustand (Variablenwerte usw.) zwischen den Funktionsaufrufen. Dadurch eignen sie sich bestens zur Erzeugung von komplexen zustandsorientierten Iteratoren, z.B. ein Generator, der die Zahlen einer Fibonacci-Folge erzeugt. 
# Ein Generator wird wie eine Funktion aufgerufen. Er liefert als Rückgabewert ein Iterator-Objekt zurück. Dabei wird der Code des Generators noch nicht ausgeführt.
# Wird nun der zurückgelieferte Iterator benutzt, dann wird jedes mal, wenn ein neues Objekt des Iterators benötigt wird (next-Methode), der Code innerhalb des Generators so lange ausgeführt, bis er bei einer yield-Anweisung angelangt.
# yield wirkt dann wie ein return einer Funktion, d.h. der Wert des Ausdrucks oder Objektes hinter yield wird zurückgegeben. Allerdings wird der Generator dabei nicht wie eine Funktion beendet, sondern er wird nur unterbrochen und wartet nun auf den nächsten Aufruf, um hinter dem yield weiterzumachen. Sein gesamter Zustand wird bis zum nächsten Aufruf zwischengespeichert.
# da generator nach yield weitermacht bei nächstem aufruf brauchen wir eine schleife (meist while True:) um immmer wieder Werte zu generieren
# man kann entweder einen endlosen generator verwenden, dann muss bei dessen verwendung darauf geachtet werden, eine abbruchsbedingung zu prüfen
# oder man baut diese direkt in den generator ein und bricht mit return ab
def Gen_A(multiple=False):
    a = 703
    while True:
        a *= 16807
        a %= 2147483647
        if multiple is False or (a % 4) == 0:
            yield a
def Gen_B(multiple=False):
    b = 516
    while True:
        b *= 48271
        b %= 2147483647
        if multiple is False or (b % 8) == 0:
            yield b

# iteratoren definieren; wär auch möglich z.B.
# for a, b in itertools.islice(zip(gen1, gen2), 40000000):
A, B = Gen_A(), Gen_B()
matching = 0
for n in range(40*10**6):
    if (n % 1000000) == 0:
        print(n)
    bin_a = next(A) & 0xffff
    bin_b = next(B) & 0xffff
    matching += 1 if (bin_a == bin_b) else 0
print("part1", matching)

A, B = Gen_A(multiple=True), Gen_B(multiple=True)
matching = 0
for n in range(5*10**6):
    if (n % 1000000) == 0:
        print(n)
    bin_a = next(A) & 0xffff
    bin_b = next(B) & 0xffff
    matching += 1 if (bin_a == bin_b) else 0
print("part2", matching)
# prev_a = 703
# # prev_a = 65
# prev_b = 516
# # prev_b = 8921
# matching = 0
# # faster to use generators
# for n in range((40*10**6)):
#     prev_a = (prev_a * 16807) % 2147483647
#     prev_b = (prev_b * 48271) % 2147483647
#     if (n % 1000000) == 0:
#         print(n)
# 
#     # print(prev_a, prev_b)
#     # get last 16bits by masking -> how to get mask explained at bottom
#     # bin returns binary string representation of given nr
#     # so masking isnt possbile: normally binarynr & mask
#     # but masking also works on normal int
#     mask = 0xffff
#     # bin -> str repreesentation -> use slicing to get last 16 "bits"
#     # this is working but takes wayy too long
#     # bin_a = bin(prev_a)[-16:]
#     # bin_b = bin(prev_b)[-16:]
#     bin_a = prev_a & mask
#     bin_b = prev_b & mask
#     matching += 1 if (bin_a == bin_b) else 0
# 
# print(matching) # 594

prev_a = 703
prev_b = 516
matching = 0
for n in range((5*10**6)):
    while True:
        prev_a = (prev_a * 16807) % 2147483647
        if (prev_a % 4) == 0:
            break
    while True:
        prev_b = (prev_b * 48271) % 2147483647
        if (prev_b % 8) == 0:
            break
    if (n % 1000000) == 0:
        print(n)

    mask = 0xffff
    bin_a = prev_a & mask
    bin_b = prev_b & mask
    matching += 1 if (bin_a == bin_b) else 0

print(matching) # 328
# to mask out the 16 last bits
# either x & 0b1111111111111111 (0b->binary literal for binary nr in python, then 16 ones following)
# or hex represetation x & 0xffff (or normal int works as well, here: 65535)
# >>> int("1111111111111111", 2)
# 65535
# >>> int("1111111111111111", 2)
# 65535
# >>> hex(65535)
# '0xffff'
