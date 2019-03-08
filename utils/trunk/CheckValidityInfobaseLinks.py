import os

infobase_dir = "Z:\\workspace\\infobase\\output"
base_dir = "Z:\\workspace\\runtime"

def checkFile(filename):
    if not filename.endswith(".py"):
        return
    file = open(filename, "r")
    try:
        for lineNR, line in enumerate(file.readlines()):
            if line.lstrip().startswith("#"):
                continue
            index = line.find(".html")
            if index == -1:
                continue
            if not line[index + len(".html")] in ("|", "\"", "#"):
                continue
            index2a = line.rfind("|", 0, index)
            index2b = line.rfind("\"", 0, index)
            if index2a == -1:
                index2 = index2b
            else:
                if index2b == -1:
                    index2 = index2a
                else:
                    index2 = max(index2a, index2b)
            if index2 == -1:
                raise RuntimeError("Unable to extract infobase link for \"%s\" on line %i!" % (filename, lineNR + 1))
            infobase_link = line[index2 + len("|"):index + len(".html")] #FIXME: len("|") OR len("\"")
            if not os.path.exists(os.path.join(infobase_dir, infobase_link)):
                print "Possible bad infobase link in \"%s\" on line %i to \"%s\"!" % (filename, lineNR + 1, infobase_link)
    finally:
        file.close()

def checkDir(dir):
    for filename in os.listdir(dir):
        full_filename = os.path.join(dir, filename)
        if os.path.isdir(full_filename):
            checkDir(full_filename)
        else:
            checkFile(full_filename)

checkDir(base_dir)
