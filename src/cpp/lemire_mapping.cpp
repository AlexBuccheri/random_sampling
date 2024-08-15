// Map uniformly random bits (from some other RNG) to a non-power of two[1] range 1..N
// (or 0..N-1) efficiently .
//
// Original paper: https://arxiv.org/pdf/1805.10941
//
// My implementation is adapted from this reference:
//  https://www.pcg-random.org/posts/bounded-rands.html
//
// Originally stated here:
//  https://lemire.me/blog/2019/06/06/nearly-divisionless-random-integer-generation-on-various-systems/
//
// Follow-up blog:
//  https://sts10.github.io/2020/10/10/lemire-neaarly-divisionless-random.html

#include <iostream>
#include <cassert>
#include <cstdint>
#include <random>

uint32_t mock_rng() {
    static std::mt19937 rng{std::random_device{}()};
    std::uniform_int_distribution<uint32_t> dist(0, UINT32_MAX);
    return dist(rng);
}

uint32_t bounded_rand(uint32_t (*rng)(), uint32_t range) {
    uint32_t x = rng();
    uint64_t m = uint64_t(x) * uint64_t(range);
    uint32_t l = uint32_t(m);
    if (l < range) {
        uint32_t t = -range;
        if (t >= range) {
            t -= range;
            if (t >= range)
                t %= range;
        }
        while (l < t) {
            x = rng();
            m = uint64_t(x) * uint64_t(range);
            l = uint32_t(m);
        }
    }
    return m >> 32;
}

int main() {
    // Map random numbers to [0,range), 100 times
    const uint32_t range = 100;
    for (int i=0; i< 100; i++) {
        uint32_t result = bounded_rand(mock_rng, range);
        std::cout << result << std::endl;
    }
    return 0;
}
