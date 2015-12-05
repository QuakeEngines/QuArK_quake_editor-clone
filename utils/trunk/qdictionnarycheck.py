# qdictionnarycheck.py - check for uniqueness in qdictionnary entries

numbers = []
i = open("qdictionnary.py")
try:
    for s in i.readlines():
        if s == "":
            break
        s = s.strip()
        c = s.split(":")
        try:
            number = int(c[0])
            if number in numbers:
                print "duplicate on line: %s" % (s, )
            else:
                numbers.append(number)
        except:
            pass
finally:
    i.close()
