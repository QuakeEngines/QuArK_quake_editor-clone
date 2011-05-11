"""
Pentium166 says:
integer = 2 bytes
float = 4bytes
char = 4bytes per char
Pentium166 says:
texsize=4
	trisize=12
	framesize=40+(4*vertnum)
"""

import struct
import array

infile  = "c:\\Python24\\t.b3d"
# FILE = open(infile, 'rb')
FILE = open("c:\\Python24\\t.b3d")
# readin = FILE.read(4)
# try:
#     print "struct is",struct.calcsize(readin)
# except:
#     pass

while 1:
    line = FILE.readline()
    if line == '':
        break
    line = line.split(" ")
    print "line type is",type(line)
    print "len line is",len(line)
    print line
    if type(line) == 'list':
        print "THIS LINE IS A LIST"
    else:
     #   size = struct.calcsize(line)
     #   print size
        pass
    line = line[0].split("\x00")
    print line
    print "len line",len(line)
    for item in line:
        if item == '' or item == '\n':
            continue
        else:
            print type(item), item
        try:
            size = struct.calcsize(item)
            print "size is", size
        except:
            pass
        try:
            (Integer,) = struct.unpack('c', item)
            print "Integer is", Integer
        except:
            pass
        try:
            a = array.array('l')
            a.item
            thisis = a[0]
            print "a is",a
            print "thisis",thisis
        except:
            pass
    print ""
 
 ### Reads through file and prints format 'l' long integer but hangs.
  #  try:
  #      (Integer,) = struct.unpack('l', FILE.read(4))
  #      if Integer == '':
  #          break
  #      print Integer
  #  except:
  #      pass

print ""
FILE.close()
FILE = open("c:\\Python24\\t.b3d")
print "============== printing for item in FILE ==============="
count = 0
for item in FILE:
    count = count + 1
    print item
print "len file",count

FILE.close()
FILE = open("c:\\Python24\\t.b3d")
print "============== Using a single read in method ==============="
(Integer,) = struct.unpack('l', FILE.read(4))
print Integer
print ""

FILE.close()
FILE = open("c:\\Python24\\t.b3d")
print "============== Using the array method ==============="
a = array.array('l')
a.read(FILE, 1)
Integer = a[0]
# (a.byteswap())  # This changes what 'a' is, not sure what it becomes though.
print a
print ""

FILE.close()
FILE = open("c:\\Python24\\t.b3d")
print "=========== Using the array method with while ==========="
while 1:
    # Format 'l'
    l = array.array('l')
    try:
        l.read(FILE, 3)
    except:
        break
    Integer = l[0]
    print l
    print Integer
    # Format 'I'
 #   I = array.array('I')
 #   try:
 #       I.read(FILE, 2)
 #   except:
#        break
  #  Integer = I[0]
#    print I
print ""

FILE.close()
"""FILE = open("c:\\Python24\\t.b3d")
print "========= H Using the array method with while ==========="
while 1:
    # Format 'l'
    a1 = array.array('c')
    try:
        a1.fromfile(FILE, 1)
    except:
        break
    Integer = a1[0]
    print a1
    print Integer
    # Format 'I'
 #   I = array.array('I')
 #   try:
 #       I.read(FILE, 2)
 #   except:
#        break
  #  Integer = I[0]
#    print I
"""













