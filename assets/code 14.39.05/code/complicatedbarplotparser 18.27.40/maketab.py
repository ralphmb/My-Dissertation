#Reads a file containing output from reading an R table
# Parses into a form I can feed back into R as a dataframe
# R suuuuuucks
# Groups Home_or_Away and grouping into one combo variable corresponding to a short vector of frequencies
# Used to make the 4- bar barplot with these multiple factors
import math
f = open("tab.txt")
lines = f.readlines()
nums = []
results = []
sides = []
groups = []
freqs = []

for line in lines[1:]:
    parts = [item for item in line.split(' ') if item != '']
    num = parts[0]
    result = parts[1]
    side = parts[2]
    group = parts[3]
    freq = parts[4].rstrip()
    nums.append(num)
    results.append(result)
    sides.append(side)
    groups.append(group)
    freqs.append(freq)

freqs = list(map(float,freqs))
combos = [f"{a} + {b}" for (a,b) in zip(sides, groups)]

for i in [0,1,2,3]:
    these = [freqs[3*i],freqs[1+3*i], freqs[2+3*i]]
    sums_to_one = [math.floor(1000000* (x /sum(these)))/1000000 for x in these]
    print( f"'{combos[3*i]}' =  c({sums_to_one[2]}, {sums_to_one[0]}, {sums_to_one[1]})")

"""
   Result Home_or_Away grouping       Freq
1    Draw         Away    Lower 0.07500000
2    Loss         Away    Lower 0.15394737
3     Win         Away    Lower 0.07105263
4    Draw         Home    Lower 0.07763158
5    Loss         Home    Lower 0.09868421
6     Win         Home    Lower 0.12368421
7    Draw         Away   Higher 0.03947368
8    Loss         Away   Higher 0.08815789
9     Win         Away   Higher 0.07236842
10   Draw         Home   Higher 0.03684211
11   Loss         Home   Higher 0.04473684
12    Win         Home   Higher 0.11842105
"""
