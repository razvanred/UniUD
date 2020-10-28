#Provo ad imparare le basi di python:
#Funzioni ricorsive
#Semplice controllo di flusso
#Espressioni lambda
#Liste

#Fibonacci iterativo
def fib1(n):
    a, b = 0, 1
    while n > 0:
        a, b = b, a + b
        n = n - 1
    return b

#Fibonacci ricorsivo generalizzato
def growth(base, period, increment, n):
    rec = lambda x: growth(base, period, increment, x)
    if n <= period:
        return base
    else:
        return increment(rec(n - 1), rec(n - 1 - period))

#Implementazione dalla generalizzazione
fib2 = lambda n: growth(1, 1, lambda a, b: a + b, n)

#Fibonacci iterativo generalizzato (memoization)
def growth_mem(base, period, increment, n):
    #Le liste di python sono array incrementali
    list = []
    #Casi base
    for i in range(period + 1):
        list.append(base)

    #Passi ricorsivi (iterazioni non base)
    for i in range(period, n):
        list.append(increment(list[-1], list[i - period]))

    #L'elemento n-1 contiene l'ennesima iterazione
    return list[-1]

#Implementazione dalla generalizzazione
fib3 = lambda n: growth_mem(1, 1, lambda a, b: a + b, n)

#Stampa
for a in range(20):
    print(fib1(a), fib2(a), fib3(a))

#Crescita lenta
slow_growth = lambda n: growth_mem(2, 10, lambda a, b: a + b, n)
for a in range(30):
    print(slow_growth(a))

#Crescita rapida
fast_growth = lambda n: growth_mem(2, 1, lambda a, b: a * b, n)
for a in range(8):
    print(fast_growth(a))

#Potenze di 2
two_power = lambda n: growth_mem(1, 0, lambda a, b: a + b, n)
for a in range(8):
    print(two_power(a))