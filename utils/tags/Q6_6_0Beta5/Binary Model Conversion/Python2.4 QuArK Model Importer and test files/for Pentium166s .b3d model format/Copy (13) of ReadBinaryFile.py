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
frames = "None"
texmap = "None"
numFrames = "None"
numTVerts = "None"
#while vertices == "None":
while texmap == "None":
    count = 0
    line = FILE.readline()
    if line == '':
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
            elif vertices == "None":
                vertices = h
                headerbytes = headerbytes + struct.calcsize('h')
       #     elif frames == "None":
       #         frames = h
       #         headerbytes = headerbytes + struct.calcsize('h')
            elif texmap == "None":
                texmap = h
                headerbytes = headerbytes + struct.calcsize('h')
        except:
            try:
                (H,) = struct.unpack('H', item)
                print "H format is", H, type(H)
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
            try:
                (L,) = struct.unpack('L', item)
                print "L format is", L, type(L)
            except:
                pass
        try:
            (s,) = struct.unpack('s', item)
            print "s format is", s, type(s)
        except:
            try:
                (p,) = struct.unpack('p', item)
                print "p format is", p, type(p)
            except:
                try:
                    (P,) = struct.unpack('P', item)
                    print "P format is", P, type(P)
                except:
                    pass
        try:
            (q,) = struct.unpack('q', item)
            print "q format is", q, type(q)
        except:
            try:
                (Q,) = struct.unpack('Q', item)
                print "Q format is", Q, type(Q)
            except:
                pass
        try:
            (i,) = struct.unpack('i', item)
            print "i format is", i, type(i)
            if numFrames == "None":
                numFrames = i
                headerbytes = headerbytes + struct.calcsize('i')
            if numFrames != "None":
                numTVerts = i
                headerbytes = headerbytes + struct.calcsize('i')
        except:
            try:
                (I,) = struct.unpack('I', item)
                print "I format is", I, type(I)
            except:
                pass
        try:
            for letter in item:
                (c,) = struct.unpack('c', letter)
                string = string + c
    #            print "byte is",struct.calcsize('c')
    #        print "c format, string is", string
            if name == "None" and string[0].isalpha():
                name = string
                print "headerbytes is",headerbytes
                print "len name is",len(name)
                headerbytes = headerbytes + struct.calcsize('c') * len(name)
                print "headerbytes is",headerbytes
        except:
            pass
    print "len file",count
print ""
o = open("c:\\Python24\\MYPRINT.txt", "w")
print "numObj",numObj
o.write("numObj = " + str(numObj) + "\n")
print "name",name
o.write("name = " + str(name) + "\n")
print "faces",faces
o.write("faces = " + str(faces) + "\n")
print "vertices",vertices
o.write("vertices = " + str(vertices) + "\n")
print "frames",frames
print "texmap",texmap
print "numFrames",numFrames
print "numTVerts",numTVerts
print "headerbytes",headerbytes
o.write("headerbytes = " + str(headerbytes) + "\n")
print "ALL DONE"
FILE.close()

FILE = open("c:\\Python24\\t.b3d", 'rb')
FILE.seek(30)
#what = FILE.read(30)
#o.write(str(what))
#o.write("\n")
#o.write("what")
#o.write("\n")
#numObj = struct.unpack('i', FILE.read(4))
#o.write(str(numObj))
#name = struct.unpack('ccccc', FILE.read(5))
#o.write(str(name))
#faces = struct.unpack('h', FILE.read(2))
#o.write(str(faces))
#vertices = struct.unpack('h', FILE.read(4))
#o.write(str(vertices))
#numFrames = struct.unpack('h', FILE.read(4))
#o.write(str(numFrames))
#numTVerts = struct.unpack('h', FILE.read(4))
#o.write(str(numTVerts))

texmaplist = array.array('f')
texmaplist.read(FILE, (texmap*2)-1)
#print "texmaplist is"
#print texmaplist
o.write("\n")
o.write("texmaplist is")
o.write("\n")
o.write(str(texmaplist))
o.write("\n")
o.write("\n")
o.write("\n")
facelist = array.array('i')
facelist.read(FILE, (2691*3))
#print "facelist is"
#print facelist
o.write("facelist is")
o.write("\n")
o.write(str(facelist))
o.write("\n")
o.write("\n")
o.write("\n")
frames = 10
frame = 0
while frame < frames:
    QuArKframe = {}
    QuArKvertices = ()
    if frame == frames-1:
        for vtx in range((vertices-1)*3):
            vertex = struct.unpack('f', FILE.read(4))
            QuArKvertices = QuArKvertices + (vertex)
        QuArKframe['index']= (frame,)
        QuArKframe['Vertices']= QuArKvertices
        o.write("\n")
        o.write("frame " + str(frame) + " vertlist has " + str(len(QuArKframe['Vertices'])) + " vertexes")
        o.write("\n")
        o.write(str(QuArKframe))
        o.write("\n")
        break
    else:
        for vtx in range(vertices*3):
            vertex = struct.unpack('f', FILE.read(4))
            print "type vertex is ",vertex, type(vertex)
            QuArKvertices = QuArKvertices + (vertex)
        QuArKframe['index']= (frame,)
        QuArKframe['Vertices']= QuArKvertices
        o.write("\n")
        o.write("frame " + str(frame) + " vertlist has " + str(len(QuArKframe['Vertices'])) + " vertexes")
        o.write("\n")
        o.write(str(QuArKframe))
        o.write("\n")
        frame = frame + 1
 
 #   vertlist = array.array('f')
 #   vertlist.read(FILE, vertices*3)
 #   print "frame " + str(frame) + " vertlist is"
 #   print vertlist
 #   o.write("\n")
 #   o.write("frame " + str(frame) + " vertlist is")
 #   o.write("\n")
 #   o.write(str(vertlist))
 #   o.write("\n")
 #   frame = frame + 1
 #   if frame == frames-1:
 #       vertlist = array.array('f')
 #       vertlist.read(FILE, ((vertices-1)*3))
 #       o.write("\n")
 #       o.write("frame " + str(frame) + " vertlist is")
 #       o.write("\n")
 #       o.write(str(vertlist))
 #       o.write("\n")
 #       break

o.close()
FILE.close()

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













