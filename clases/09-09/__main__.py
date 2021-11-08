def aux(p, ns) =
    resultado = 0

    for n in ns:
        resultado = n * p + resultado
        p = p // 10

    return resultado