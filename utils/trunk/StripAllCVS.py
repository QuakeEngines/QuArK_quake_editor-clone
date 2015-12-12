#This script can be run over a 'runtime' directory, and it removes all CVS files, and CVS information from text files.
#WARNING: Be careful! Many files get modified or deleted! Make sure you run it over the right directory!

import os
import shutil
import sys

if len(sys.argv) != 2:
    raise RuntimeError("Wrong number of arguments!")

base_path = sys.argv[1]

def DoFile(filename):
    if not (filename.endswith(".py") or filename.endswith(".txt") or filename.endswith(".qrk")):
        #Not a file we want/need to handle!
        return

    lines = []
    file = open(filename, "r")
    try:
        for line in file.readlines():
            lines += [line]
    finally:
        file.close()

    def getLinesToRemove(lines, identifier):
        whatLines = []
        lineIdentifier = -1
        for i, line in enumerate(lines):
            if line.find(identifier) != -1:
                lineIdentifier = i
                break
        if lineIdentifier != -1:
            index = lines[lineIdentifier].find(identifier)
            #Everything in front of the identifier is the prepend-marker to use to figure out the size of the CVS information block.
            marker = lines[lineIdentifier][:index]
            if len(marker) != 0:
                i = lineIdentifier - 1
                while (i >= 0) and (lines[i].startswith(marker)):
                    i -= 1
                startOfBlock = i + 1
                i = lineIdentifier + 1
                while (i < len(lines)) and (lines[i].startswith(marker)):
                    i += 1
                endOfBlock = i - 1
                for i in range(startOfBlock, endOfBlock + 1):
                    whatLines += [i, ]
            else:
                whatLines += [lineIdentifier, ]
        return whatLines

    removeLines = []
    removeLines += getLinesToRemove(lines, "$Id:")
    removeLines += getLinesToRemove(lines, "$Header")
    removeLines += getLinesToRemove(lines, "$Log")
    removeLines += getLinesToRemove(lines, "# ----------- REVISION HISTORY ------------") #Special case: This line too!
    if len(removeLines) != 0:
        file = open(filename, "w")
        try:
            for i, line in enumerate(lines):
                if not i in removeLines:
                    file.write(line)
        finally:
            file.close()

def DoDir(dir):
    for filename in os.listdir(dir):
        full_filename = os.path.join(dir, filename)
        if os.path.isdir(full_filename):
            if filename == "CVS":
                shutil.rmtree(full_filename)
            else:
                DoDir(full_filename)
        else:
            if filename in (".cvsignore", ".project"):
                os.remove(full_filename)
            else:
                DoFile(full_filename)

raw_input("This will modify files and may delete entire directories in the given path! Are you sure? (BREAK the script if not!)")
DoDir(base_path)
