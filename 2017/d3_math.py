import math

def p1_side(x):
    # beschreibungen für n,i,a in d3.py
    n = math.ceil(0.5*(math.sqrt(x)-1))
    i = 1+4*n*(n+1)
    a = 2*n+1
    # abstand von i zu x -> ganzzahl div durch schritte zur nächsten ecke -> seite 0=unten 1=links etc.. s[0..4]
    s = (i-x)//(a-1)
    # mitte der seite: i-n -> mitte unten -> davon seite*(a-1) schritte laufen um zur mitte der seite zu kommen
    m = (i-n) - s*(a-1)
    # von der mitte immer n schritte zum zentrum -> absoluter abstand von x zur mitte [0..n] liefert schritte von mitte -> summe ist schritte/manhattan dist zum zentrum
    steps = n + abs(m-x)
    print(f"x {x} n {n} i {i} a {a} s {s} m {m} steps {steps}")

def p1_i(x):
    # imgur solution -> not working
    n = math.floor(0.5*(math.sqrt(x)+1))
    i = 1+4*n*(n-1)
    steps = n + abs((i+n-x) % (2*n))
    print(f"x {x} n {n} i {i} steps {steps}")


def p1(x):
    n = math.ceil(0.5*(math.sqrt(x)-1))
    i = 1+4*n*(n+1)
    a = 2*n+1
    # steps = n + abs((i-n-x) % ((a-1)))
    steps = a + ((i-x) % a) - 1
    print(f"x {x} n {n} i {i} a {a} steps {steps}")


for x in range(26,50):
    p1_side(x)
    # p1_i(x)

p1_side(325489)
p1_side(1024)
p1_side(23)


