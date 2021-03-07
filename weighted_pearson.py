
from random import shuffle
import numpy as np

# if tight on memory, can replace with something like:
# T[i] = 255 - i
cache_table = list(range(0, 256))
shuffle(cache_table)

def hash8(message: str, table) -> int:
    hash = len(message) % 256
    for i in message:
        hash = table[hash ^ ord(i)]
    return hash

def weighted_voting(mem_addr, cache_array, modifier="modulo"):    
    # pearson hash and mod by # of tiles to get a distinguished tile
    distinguished_tile = hash8(str(hex(mem_addr)), cache_table) % len(caches)

    # use a numeric property of the memory addr e.g. sum 
    # of the bits, or its length, to add as a modifier bonus 
    # to the size of the distinguished tile
    if modifier == "modulo":
        modifier_bonus = mem_addr % len(cache_array)                    # addr mod cache sizes
    elif modifier == "popcount":
        modifier_bonus = bin(mem_addr).count("1")                       # sum of positive bits
    else:
        modifier_bonus = np.median(caches)                             # median of cache sizes

    # copy old array to make a new one with the modifier bonus
    voters = caches.copy()
    voters[distinguished_tile] += modifier_bonus

    # majority sum decides the memory address's home tile
    home_tile = np.argmax(voters)

    return home_tile

def test_generation(test_size, cache_array, mod):
    # generate some random test_size-bit memory addresses
    test_addrs = [i for i in range(test_size)]

    # home them
    homes = []
    for addr in test_addrs:
        homes.append(weighted_voting(addr, cache_array, mod))

    # plot the distribution, should look like
    # caches list if it were a histogram
    return homes
