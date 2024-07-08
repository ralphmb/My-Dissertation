with open("./inp.txt", "r") as inp:
    with open("out.txt", "w") as out:
        for line in inp.readlines():
            if '[' in line and ']' in line:
                line = line.replace('[', 'text(size:8pt,[').replace(']', '])')
            out.write(line)

