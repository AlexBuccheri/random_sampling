from math import log
import random
from random import uniform


def hidden_shuffle(n, N):
    """Random sampling with replacement

     Obtain a sample of size n, from population size N
    Values range [0, N).

    See https://www.pcg-random.org/posts/bounded-rands.html
    for how to simply map [0, N). to [i, j]

    :param n:
    :param N:
    :return:
    """
    H = 0
    i = 0
    N_minus_m = float(N - n)
    if N > n:
        H = n
        while i < n:
            q = 1.0 - N_minus_m / float(N-i)
            i = i + int(log(uniform(0, 1), 1-q) )
            p_i = 1.0 - N_minus_m / float(N-i)
            if i < n and uniform(0, 1) < p_i / q:
                H = H-1
            i = i + 1
    L = n - H; a = 1.0
    while H > 0:  # STEP 2: draw high−items
        S_old = n + int(a * N_minus_m)
        a = a * uniform(0, 1)**(1.0 / H)
        S = n + int(a * N_minus_m)
        if S < S_old:
            yield N - 1 - S
        else:
            L = L + 1  # duplicate detected
        H = H-1
    while L > 0:  # STEP 3: draw low−items
        u = uniform(0, 1)
        s = 0
        F = float(L) / float(n)
        while F < u and s < (n-L-1):
            F = 1 - (1 - float(L) / float(n-s)) * (1 - F)
            s = s + 1
        L = L-1
        n = n-s-1
        yield (N-1) - n


random.seed(42)  # You can use any number as a seed

ref = [ 148, 179, 218, 229, 475, 561, 907, 1054, 1121,
        1468, 1619, 1660, 1717, 1860, 1909, 1929, 2399,
        2418, 2452, 2551, 2722, 2726, 2827, 3045, 3255,
        3270, 3316, 3335, 3364, 3422, 3425, 3517, 3573,
        3591, 3637, 3652, 3706, 3741, 4045, 4187, 4306,
        4544, 4680, 4889, 5004, 5045, 5137, 5227, 5368,
        5486, 5492, 5532, 5577, 5740, 5769, 5936, 6024,
        6025, 6067, 6123, 6159, 6176, 6201, 6350, 6682,
        6789, 6911, 7053, 7058, 7071, 7181, 7222, 7312,
        7321, 7400, 7535, 7675, 7732, 7866, 7920, 7931,
        8029, 8189, 8264, 8521, 8810, 8993, 9031, 9051,
        9128, 9353, 9423, 9424, 9481, 9484, 9504, 9857,
        9869, 9943, 9974]

result = [value for value in hidden_shuffle(100, 10000)]

assert result == ref
