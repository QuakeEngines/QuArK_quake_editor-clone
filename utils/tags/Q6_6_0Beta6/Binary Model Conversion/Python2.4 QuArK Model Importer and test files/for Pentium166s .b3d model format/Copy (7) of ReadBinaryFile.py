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

#FILE = open("c:\\Python24\\t.b3d")
#FILE = open("c:\\Python24\\t.b3d", 'rb')
# readin = FILE.read(4)
# try:
#     print "struct is",struct.calcsize(readin)
# except:
#     pass

#import binascii
#wholefile = ""
#while 1:
#    wholefile = FILE.read(45)
#    if wholefile == '':
#        break
#    myfile = binascii.b2a_uu(wholefile)
#    print myfile
"""
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
 """
 ### Reads through file and prints format 'l' long integer but hangs.
  #  try:
  #      (Integer,) = struct.unpack('l', FILE.read(4))
  #      if Integer == '':
  #          break
  #      print Integer
  #  except:
  #      pass
#print ""
#FILE.close()



FILE = open("c:\\Python24\\t.b3d")
#A lines = 0
print "========= printing for item in FILE with while ========"
headerbytes = 0
numObj = "None"
name = "None"
faces = "None"
vertices = "None"
while vertices == "None":
#A while lines < 420:
    count = 0
#A    lines = lines + 1
    line = FILE.readline()
    if line == '':
#A        continue
        break
    print "line is"
    print line
    line = line.split(" ")
    line = line[0].split("\x00")
    print "line now"
    print line
    for item in line:
        string = ""
        if item == '' or item == '\n':
            continue
        count = count + 1
        try:
            (b,) = struct.unpack('b', item)
            if numObj == "None":
                numObj = b
                headerbytes = headerbytes + struct.calcsize('b')
            print "b format is", b, type(b)
            print "headerbytes is",headerbytes
        except:
            try:
                (B,) = struct.unpack('B', item)
                print "B format is", B, type(B)
            except:
                pass
        try:
            (h,) = struct.unpack('h', item)
            print "h format is", h, type(h)
            if faces == "None":
                faces = h
                headerbytes = headerbytes + struct.calcsize('h')
            if faces != "None":
                vertices = h
                headerbytes = headerbytes + struct.calcsize('h')
        except:
            pass
        try:
            (f,) = struct.unpack('f', item)
            print "f format is", f, type(f)
        except:
            pass
        try:
            (d,) = struct.unpack('d', item)
            print "d format is", d, type(l)
        except:
            pass
        try:
            (l,) = struct.unpack('l', item)
            print "l format is", l, type(l)
        except:
            pass
        try:
            for letter in item:
                (c,) = struct.unpack('c', letter)
                string = string + c
                print "byte is",struct.calcsize('c')
                headerbytes = headerbytes + struct.calcsize('c')
            print "c format, string is", string
            if name == "None" and string[0].isalpha():
                name = string
        except:
            pass
    print "len file",count
print ""
print "numObj",numObj
print "name",name
print "faces",faces
print "vertices",vertices
print "headerbytes",headerbytes
print "ALL DONE"
FILE.close()

FILE = open("c:\\Python24\\t.b3d", 'rb')
FILE.seek(headerbytes)
facelist = array.array('h')
facelist.read(FILE, faces*3)
print "facelist is"
print facelist
vertlist = array.array('f')
vertlist.read(FILE, vertices)
print "vertlist is"
print vertlist

"""
FILE = open("c:\\Python24\\t.b3d")
print "============== printing for item in FILE ==============="
count = 0
line = FILE.readline()
print "line is"
print line
line = line.split(" ")
line = line[0].split("\x00")
print "line now"
print line
for item in line:
    string = ""
    if item == '' or item == '\n':
        continue
    count = count + 1
    try:
        (b,) = struct.unpack('b', item)
        print "b is", b, type(b)
    except:
        try:
            (B,) = struct.unpack('B', item)
            print "B is", B, type(B)
        except:
            pass
    try:
        (h,) = struct.unpack('h', item)
        print "h is", h, type(h)
    except:
        pass
    try:
        for letter in item:
            (c,) = struct.unpack('c', letter)
            string = string + c
        print "string is", string
    except:
        pass
print "len file",count
FILE.close()
"""


"""
FILE = open("c:\\Python24\\t.b3d")
print "============== Using a single read in method ==============="
(Integer,) = struct.unpack('l', FILE.read(4))
(String,) = struct.unpack('c', FILE.read(1))
size = struct.calcsize('c')

print Integer
print String
print size
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
"""


"""
FILE = open("c:\\Python24\\t.b3d")
print "=========== Using the array method with while ==========="
while 1:
    # Format 'l'
    l = array.array('l')
    c = array.array('c')
    try:
        l.read(FILE, 1)
  #      l.read(FILE, 14)
        c.read(FILE, 3)
    except:
        break
    Integer = l[0]
    print l
    print c
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
"""


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













