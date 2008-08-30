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

print ""
while 1:
 #   line = FILE.readline()
 #   if line == '':
 #       break
 #   line = line.split(" ")
 #   print "type is",type(line), line
 #   print "line has",len(line)
 #   if type(line) == 'str':
 #       print string
 #   else:
 #       size = struct.calcsize(line)
 #       print size
    try:
        (Integer,) = struct.unpack('l', FILE.read(4))
        if Integer == '':
            break
        print Integer
    except:
        pass


(Integer,) = struct.unpack('l', FILE.read(4))
count = 0
for item in FILE:
    count = count + 1
    print item
print "len file",count
print "Using a single read in method."
print Integer
print ""

FILE.close()
FILE = open("c:\\Python24\\t.b3d")

a = array.array('l')
a.read(FILE, 1)
Integer = a[0]
(a.byteswap())
print "Using the array method."
print a
print ""













