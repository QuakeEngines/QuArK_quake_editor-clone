"""   QuArK  -  Quake Army Knife

Various Model importer\exporter utility functions.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


import math, os, os.path, time
import quarkx
import quarkpy.qutils

# Globals
SS_MODEL = 3
logging = 0
textlog = "model_ie_log.txt"
tobj = None

'''
NOTE: ALL IMPORTERS AND EXPORTERS SHOULD INCLUDE THIS LOGGING CODE.

1) To add logging to an importer or exporter put these lines near the top, under the file header, in this order:
import os, time, operator
import ie_utils
from ie_utils import tobj

# Globals
logging = 0
exportername = "ie_md2_export.py" (or importername = "ie_md2_import.py" depending on which one you're doing)
textlog = "md2_ie_log.txt"

2) Then add needed globals and calls to start and end the logging in your main file function like this:
def save_md2(filename):
    global tobj, logging, exportername, textlog ### Needed globals.
    ### Next line starts the logging.
    logging, tobj, starttime = ie_utils.default_start_logging(exportername, textlog, filename, "EX") ### Use "EX" for exporter text, "IM" for importer text.

    ### Line below here saves the model (just for this example---DO NOT COPY NEXT LINE).
    fill_md2(md2, component)

    ### Next line is optional, it adds additional text at the bottom of the default message,
    ### with a blank line between them. If none then just exclude it from the function arguments below.
    add_to_message = "Any used skin textures that are not a .pcx\nwill need to be created to go with the model"
    ### Next line ends the logging.
    ie_utils.default_end_logging(filename, "EX", starttime, add_to_message) ### Use "EX" for exporter text, "IM" for importer text.


3) Then in any function you want logging declair the global and call for tobj like this: (all items must be strings)
def fill_md2(md2, component):
    global tobj
    if logging == 1:
        tobj.logcon ("#####################################################################")
        tobj.logcon ("Skins group data: " + str(md2.num_skins) + " skins")
        tobj.logcon ("#####################################################################")
        tobj.logcon ("")

'''


def default_start_logging(IM_EX_name, IM_EX_textlog, filename, IM_or_EX, add_to_message=""):
    global tobj, textlog

    starttime = time.time()
    if quarkx.setupsubset(SS_MODEL, "Options")['IELogging'] != "0":
        logging = 1
        if quarkx.setupsubset(SS_MODEL, "Options")['IELogByFileType'] != "1" and textlog != "model_ie_log.txt" and tobj is not None:
            if tobj.txtobj is not None:
                tobj.txtobj.close()
                textlog = "model_ie_log.txt"
                tobj = dotext(textlog) # Calls the class to handle logging.

        if quarkx.setupsubset(SS_MODEL, "Options")['IELogByFileType'] == "1" and textlog == "model_ie_log.txt" and tobj is not None:
            if tobj.txtobj is not None:
                tobj.txtobj.close()
                textlog = IM_EX_textlog
                tobj = dotext(textlog) # Calls the class to handle logging.

        if quarkx.setupsubset(SS_MODEL, "Options")['IELogAll'] != "1":
            if quarkx.setupsubset(SS_MODEL, "Options")['IELogByFileType'] != "1":
                textlog = "model_ie_log.txt"
                tobj = dotext(textlog) # Calls the class to handle logging.
            else:
                textlog = IM_EX_textlog
                tobj = dotext(textlog) # Calls the class to handle logging.
        else:
            if tobj is None:
                if quarkx.setupsubset(SS_MODEL, "Options")['IELogByFileType'] != "1":
                    textlog = "model_ie_log.txt"
                    tobj = dotext(textlog) # Calls the class to handle logging.
                else:
                    textlog = IM_EX_textlog
                    tobj = dotext(textlog) # Calls the class to handle logging.

        if IM_or_EX == "IM":
            tobj.logcon ("#####################################################################")
            tobj.logcon ("This is: %s" % IM_EX_name)
            tobj.logcon ("Importing file:")
            tobj.logcon (filename)
            if add_to_message == "":
                tobj.logcon ("#####################################################################")
            else:
                tobj.logcon ("")
                add2log = add_to_message.split('\n')
                for item in add2log:
                    tobj.logcon (item)
                tobj.logcon ("#####################################################################")
        else:
            tobj.logcon ("#####################################################################")
            tobj.logcon ("This is: %s" % IM_EX_name)
            tobj.logcon ("Exporting file:")
            tobj.logcon (filename)
            if add_to_message == "":
                tobj.logcon ("#####################################################################")
            else:
                tobj.logcon ("")
                add2log = add_to_message.split('\n')
                for item in add2log:
                    tobj.logcon (item)
                tobj.logcon ("#####################################################################")
    else:
        logging = 0

    return logging, tobj, starttime


def default_end_logging(filename, IM_or_EX, starttime, add_to_message=""):
    global tobj

    end = time.time()
    seconds = "in %.2f %s" % (end-starttime, "seconds")
    if quarkx.setupsubset(SS_MODEL, "Options")['IELogging'] != "0":
        if IM_or_EX == "IM":
            tobj.logcon ("=====================================================================")
            tobj.logcon ("Successfully imported " + os.path.basename(filename))
            tobj.logcon (seconds + " " + time.asctime(time.localtime()))
            if add_to_message == "":
                tobj.logcon ("=====================================================================")
                tobj.logcon ("")
                tobj.logcon ("")
            else:
                tobj.logcon ("")
                add2log = add_to_message.split('\n')
                for item in add2log:
                    tobj.logcon (item)
                tobj.logcon ("=====================================================================")
                tobj.logcon ("")
                tobj.logcon ("")
            if quarkx.setupsubset(SS_MODEL, "Options")['IELogAll'] != "1":
                tobj.txtobj.close()
        else:
            tobj.logcon ("=====================================================================")
            tobj.logcon ("Successfully exported " + os.path.basename(filename))
            tobj.logcon (seconds + " " + time.asctime(time.localtime()))
            if add_to_message == "":
                tobj.logcon ("=====================================================================")
                tobj.logcon ("")
                tobj.logcon ("")
            else:
                tobj.logcon ("")
                add2log = add_to_message.split('\n')
                for item in add2log:
                    tobj.logcon (item)
                tobj.logcon ("=====================================================================")
                tobj.logcon ("")
                tobj.logcon ("")
            if quarkx.setupsubset(SS_MODEL, "Options")['IELogAll'] != "1":
                tobj.txtobj.close()
    if IM_or_EX == "EX":
        if add_to_message == "":
            message = "Successfully exported " + os.path.basename(filename) + "\n" + seconds + " " + time.asctime(time.localtime())
        else:
            message = "Successfully exported " + os.path.basename(filename) + "\n" + seconds + " " + time.asctime(time.localtime()) + "\n\n" + add_to_message
        quarkx.msgbox(message, quarkpy.qutils.MT_INFORMATION, quarkpy.qutils.MB_OK)



def safestring(st):
    "Makes sure what it gets is a string,"
    "deals with strange chars"

    myst = ""
    for ll in xrange(len(st)):
        if st[ll] < " ":
            myst += "#"
        else:
            myst += st[ll]
    return myst

class dotext:

    _NO = 0    #use internal to class only
    LOG = 1    #write only to LOG
    CON = 2    #write to both LOG and CONSOLE

    def __init__(self, textlog, where=LOG):
        self.textlog = textlog
        self.dwhere = where
        self.txtobj = None

    def write(self, wstring, maxlen=80):
        # Opens a text file in QuArK's main directory for logging to.
        # See QuArK's Defaults.qrk file for additional setup code for IELogging option.
        if quarkx.setupsubset(SS_MODEL, "Options")['IELogging'] != "0":
            if self.txtobj == None or not os.path.exists(quarkx.exepath + self.textlog):
                self.txtobj = open(quarkx.exepath + self.textlog, "w")
        if (self.txtobj==None):
            return
        while (1):
            ll = len(wstring)
            if (ll>maxlen):
                self.txtobj.write((wstring[:maxlen]))
                self.txtobj.write("\n")
                if int(quarkx.setupsubset(SS_MODEL, "Options")['IELogging']) == 2:
                    print (wstring[:maxlen])
                wstring = (wstring[maxlen:])
            else:
                try:
                    self.txtobj.write(wstring)
                except:
                    self.txtobj = open(quarkx.exepath + self.textlog, "w")
                    self.txtobj.write(wstring)
                if int(quarkx.setupsubset(SS_MODEL, "Options")['IELogging']) == 2:
                    if wstring != "\n":
                        print wstring
                break

    def pstring(self, ppstring, where = _NO):
        where = int(quarkx.setupsubset(SS_MODEL, "Options")['IELogging'])
        if where == dotext._NO: where = self.dwhere
        self.write(ppstring)
        self.write("\n")

    def plist(self, pplist, where = _NO):
        self.pprint ("list:[")
        for pp in xrange(len(pplist)):
            self.pprint ("[%d] -> %s" % (pp, pplist[pp]), where)
        self.pprint ("]")

    def pdict(self, pdict, where = _NO):
        self.pprint ("dict:{", where)
        for pp in pdict.keys():
            self.pprint ("[%s] -> %s" % (pp, pdict[pp]), where)
        self.pprint ("}")

    def pprint(self, parg, where = _NO):
        if parg == None:
            self.pstring("_None_", where)
        elif type(parg) == type ([]):
            self.plist(parg, where)
        elif type(parg) == type ({}):
            self.pdict(parg, where)
        else:
            self.pstring(safestring(str(parg)), where)

    def logcon(self, parg):
        self.pprint(parg, dotext.CON)

"""
NOTE: ALL IMPORTERS AND EXPORTERS SHOULD INCLUDE THIS PATH CHECKING CODE.

1) To add path checking to an importer or exporter put this line near the top:
import ie_utils

2) Call for the path check like this:
def loadmodel(root, filename, gamename, nomessage=0):
    ### First we test for a valid (proper) model path.
    basepath = ie_utils.validpath(filename)
    if basepath is None:
        return

"""

def validpath(filename):
    "Tests for a proper model path."

    basepath = ""
    name = filename.split('\\')
    for word in name:
        if word == "models":
            break
        basepath = basepath + word + "\\"
    if not filename.find(basepath + "models\\") != -1:
        quarkx.beep() # Makes the computer "Beep" once if folder structure is not valid.
        quarkx.msgbox("Invalid Path Structure!\n\nThe location of a model must be in the\n    'gamefolder\\models' sub-folder.\n\nYour model selection to import shows this path:\n\n" + filename + "\n\nPlace this model or model's folder within the game's 'models' sub-folder\nor make a main folder with a 'models' sub-folder\nand place this model or model's folder in that sub-folder.\n\nThen re-select it for importing.\n\nAny added textures needed half to also be placed\nwithin the 'game' folder using their proper sub-folders.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        return None
    else:
        return basepath


# We want this -0.0000003936 as a string, but it gives this -3.936e-007
# So this function creates the proper string and removes zeros from the end.
def NicePrintableFloat(amt):
    amt = round(amt, 10)
    amt = str(amt)
    if amt.find("e") != -1:
        nbr = ""
        if amt.startswith("-"):
            nbr = nbr + "-"
        fix = amt.replace("-", "").replace(".", "").split("e")
        fix[1] = int(fix[1])-1
        amt = nbr + "0." + "0" * fix[1] + fix[0]
    amt = amt.rstrip("0")
    amt = amt.rstrip(".")
    return amt

# CString to Python string function
def ConvertToString(data, length, start=0):
    result = ''
    for i in xrange(length):
        char = data[start+i]
        if char == "\x00":
            #NULL character found: End of string
            break
        result += char
    return result


######################################################
# Vector, Quaterion, Matrix math stuff - some taken from
# Jiba's blender2cal3d script
######################################################
def quaternion2matrix(q):
    xx = q[0] * q[0]
    yy = q[1] * q[1]
    zz = q[2] * q[2]
    xy = q[0] * q[1]
    xz = q[0] * q[2]
    yz = q[1] * q[2]
    wx = q[3] * q[0]
    wy = q[3] * q[1]
    wz = q[3] * q[2]
    return [[1.0 - 2.0 * (yy + zz),       2.0 * (xy + wz),       2.0 * (xz - wy), 0.0],
            [      2.0 * (xy - wz), 1.0 - 2.0 * (xx + zz),       2.0 * (yz + wx), 0.0],
            [      2.0 * (xz + wy),       2.0 * (yz - wx), 1.0 - 2.0 * (xx + yy), 0.0],
            [0.0                  , 0.0                  , 0.0                  , 1.0]]

def matrix2quaternion(m):
    #See: http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/index.htm
    s = math.sqrt(abs(m[0][0] + m[1][1] + m[2][2] + m[3][3]))
    if s < 0.001:
        if ((m[0][0] > m[1][1]) and (m[0][0] > m[2][2])):
            s = math.sqrt(m[3][3] + m[0][0] - m[1][1] - m[2][2]) * 2.0
            return quaternion_normalize([
            -0.25 * s,
            -(m[0][1] + m[1][0]) / s,
            -(m[0][2] + m[2][0]) / s,
            (m[2][1] - m[1][2]) / s,
            ])
        elif (m[1][1] > m[2][2]):
            s = math.sqrt(m[3][3] + m[1][1] - m[0][0] - m[2][2]) * 2.0
            return quaternion_normalize([
            -(m[0][1] + m[1][0]) / s,
            -0.25 * s,
            -(m[1][2] + m[2][1]) / s,
            (m[0][2] - m[2][0]) / s,
            ])
        else:
            s = math.sqrt(m[3][3] + m[2][2] - m[0][0] - m[1][1]) * 2.0
            return quaternion_normalize([
            -(m[0][2] + m[2][0]) / s,
            -(m[1][2] + m[2][1]) / s,
            -0.25 * s,
            (m[1][0] - m[0][1]) / s,
            ])
    return quaternion_normalize([
        -(m[2][1] - m[1][2]) / (2.0 * s),
        -(m[0][2] - m[2][0]) / (2.0 * s),
        -(m[1][0] - m[0][1]) / (2.0 * s),
        0.5 * s,
        ])

def quaternion_normalize(q):
    l = math.sqrt(q[0] * q[0] + q[1] * q[1] + q[2] * q[2] + q[3] * q[3])
    return q[0] / l, q[1] / l, q[2] / l, q[3] / l

def quaternion_multiply(q1, q2):
    r = [
            q2[3] * q1[0] + q2[0] * q1[3] + q2[1] * q1[2] - q2[2] * q1[1],
            q2[3] * q1[1] + q2[1] * q1[3] + q2[2] * q1[0] - q2[0] * q1[2],
            q2[3] * q1[2] + q2[2] * q1[3] + q2[0] * q1[1] - q2[1] * q1[0],
            q2[3] * q1[3] - q2[0] * q1[0] - q2[1] * q1[1] - q2[2] * q1[2],
        ]
    d = math.sqrt(r[0] * r[0] + r[1] * r[1] + r[2] * r[2] + r[3] * r[3])
    r[0] /= d
    r[1] /= d
    r[2] /= d
    r[3] /= d
    return r

def matrix_translate(m):
    m2 = [[m[0][0], m[1][0], m[2][0], m[3][0]],
          [m[0][1], m[1][1], m[2][1], m[3][1]],
          [m[0][2], m[1][2], m[2][2], m[3][2]],
          [m[0][3], m[1][3], m[2][3], m[3][3]]]
    return m2

def matrix_multiply(b, a):
    return [ [
        a[0][0] * b[0][0] + a[0][1] * b[1][0] + a[0][2] * b[2][0],
        a[0][0] * b[0][1] + a[0][1] * b[1][1] + a[0][2] * b[2][1],
        a[0][0] * b[0][2] + a[0][1] * b[1][2] + a[0][2] * b[2][2],
        0.0,
        ], [
        a[1][0] * b[0][0] + a[1][1] * b[1][0] + a[1][2] * b[2][0],
        a[1][0] * b[0][1] + a[1][1] * b[1][1] + a[1][2] * b[2][1],
        a[1][0] * b[0][2] + a[1][1] * b[1][2] + a[1][2] * b[2][2],
        0.0,
        ], [
        a[2][0] * b[0][0] + a[2][1] * b[1][0] + a[2][2] * b[2][0],
        a[2][0] * b[0][1] + a[2][1] * b[1][1] + a[2][2] * b[2][1],
        a[2][0] * b[0][2] + a[2][1] * b[1][2] + a[2][2] * b[2][2],
         0.0,
        ], [
        a[3][0] * b[0][0] + a[3][1] * b[1][0] + a[3][2] * b[2][0] + b[3][0],
        a[3][0] * b[0][1] + a[3][1] * b[1][1] + a[3][2] * b[2][1] + b[3][1],
        a[3][0] * b[0][2] + a[3][1] * b[1][2] + a[3][2] * b[2][2] + b[3][2],
        1.0,
        ] ]

def matrix_inverse(m):
    det = matrix_determinant(m)
    if det == 0:
        return None
    a = m[0][0]
    b = m[0][1]
    c = m[0][2]
    d = m[1][0]
    e = m[1][1]
    f = m[1][2]
    g = m[2][0]
    h = m[2][1]
    i = m[2][2]
    #FIXME: no idea how to handle 4-th dimension, so let's ignore it (I know I won't need it)
    return [ [(e*i - f*h) / det, (h*c - i*b) / det, (b*f - c*e) / det, 0.0],
             [(g*f - d*i) / det, (a*i - g*c) / det, (d*c - a*f) / det, 0.0],
             [(d*h - g*e) / det, (g*b - a*h) / det, (a*e - d*b) / det, 0.0],
             [              0.0,               0.0,               0.0, 1.0] ]

def matrix_rotate_x(angle):
    cos = math.cos(angle)
    sin = math.sin(angle)
    return [
        [1.0,  0.0, 0.0, 0.0],
        [0.0,  cos, sin, 0.0],
        [0.0, -sin, cos, 0.0],
        [0.0,  0.0, 0.0, 1.0],
    ]

def matrix_rotate_y(angle):
    cos = math.cos(angle)
    sin = math.sin(angle)
    return [
        [cos, 0.0, -sin, 0.0],
        [0.0, 1.0,  0.0, 0.0],
        [sin, 0.0,  cos, 0.0],
        [0.0, 0.0,  0.0, 1.0],
    ]

def matrix_rotate_z(angle):
    cos = math.cos(angle)
    sin = math.sin(angle)
    return [
        [ cos, sin, 0.0, 0.0],
        [-sin, cos, 0.0, 0.0],
        [ 0.0, 0.0, 1.0, 0.0],
        [ 0.0, 0.0, 0.0, 1.0],
    ]

def matrix_rotate(axis, angle):
    vx  = axis[0]
    vy  = axis[1]
    vz  = axis[2]
    vx2 = vx * vx
    vy2 = vy * vy
    vz2 = vz * vz
    cos = math.cos(angle)
    sin = math.sin(angle)
    co1 = 1.0 - cos
    return [
        [vx2 * co1 + cos,          vx * vy * co1 + vz * sin, vz * vx * co1 - vy * sin, 0.0],
        [vx * vy * co1 - vz * sin, vy2 * co1 + cos,          vy * vz * co1 + vx * sin, 0.0],
        [vz * vx * co1 + vy * sin, vy * vz * co1 - vx * sin, vz2 * co1 + cos,          0.0],
        [0.0, 0.0, 0.0, 1.0],
    ]
  # return [
  #     [vx2 * co1 + cos,          vx * vy * co1 + vz * sin, vz * vx * co1 - vy * sin, 0.0],
  #     [vz * vx * co1 + vy * sin, vy * vz * co1 - vx * sin, vz2 * co1 + cos,          0.0],
  #     [vx * vy * co1 - vz * sin, vy2 * co1 + cos,          vy * vz * co1 + vx * sin, 0.0],
  #     [0.0, 0.0, 0.0, 1.0],
  # ]
  
def matrix_scale(fx, fy, fz):
  return [
        [ fx, 0.0, 0.0, 0.0],
        [0.0,  fy, 0.0, 0.0],
        [0.0, 0.0,  fz, 0.0],
        [0.0, 0.0, 0.0, 1.0],
    ]

def point_by_matrix(p, m):
  return [
        p[0] * m[0][0] + p[1] * m[1][0] + p[2] * m[2][0] + m[3][0],
        p[0] * m[0][1] + p[1] * m[1][1] + p[2] * m[2][1] + m[3][1],
        p[0] * m[0][2] + p[1] * m[1][2] + p[2] * m[2][2] + m[3][2]
    ]

def point_distance(p1, p2):
  return math.sqrt((p2[0] - p1[0]) ** 2 + (p2[1] - p1[1]) ** 2 + (p2[2] - p1[2]) ** 2)

def vector_by_matrix(p, m):
  return [
        p[0] * m[0][0] + p[1] * m[1][0] + p[2] * m[2][0],
        p[0] * m[0][1] + p[1] * m[1][1] + p[2] * m[2][1],
        p[0] * m[0][2] + p[1] * m[1][2] + p[2] * m[2][2]
    ]

def vector_length(v):
    v = v.tuple
    return math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])

def vector_normalize(v):
    v = v.tuple
    l = math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])
    try:
        return v[0] / l, v[1] / l, v[2] / l
    except:
        return 1, 0, 0

def vector_dotproduct(v1, v2):
    v1 = v1.tuple
    v2 = v2.tuple
    return v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2]

def vector_crossproduct(v1, v2):
    v1 = v1.tuple
    v2 = v2.tuple
    return [
        v1[1] * v2[2] - v1[2] * v2[1],
        v1[2] * v2[0] - v1[0] * v2[2],
        v1[0] * v2[1] - v1[1] * v2[0],
    ]

def vector_angle(v1, v2):
    s = vector_length(v1) * vector_length(v2)
    if s == 0.0:
        return 0.0
    f = vector_dotproduct(v1, v2) / s
    if f >=  1.0:
        return 0.0
    if f <= -1.0:
        return math.pi / 2.0
    return math.atan(-f / math.sqrt(1.0 - f * f)) + math.pi / 2.0


# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.10  2010/03/07 09:46:31  cdunde
#Added new function for converting floats into nice printable strings.
#
#Revision 1.9  2009/08/01 05:31:13  cdunde
#Update.
#
#Revision 1.8  2008/07/29 02:21:08  cdunde
#Fixed comment typo error.
#
#Revision 1.7  2008/07/27 19:28:49  cdunde
#Comment update.
#
#Revision 1.6  2008/07/21 18:06:14  cdunde
#Moved all the start and end logging code to ie_utils.py in two functions,
#"default_start_logging" and "default_end_logging" for easer use and consistency.
#Also added logging and progress bars where needed and cleaned up files.
#
#Revision 1.5  2008/07/17 00:49:49  cdunde
#Fixed proper switching of logging options during the same session of QuArK.
#
#Revision 1.4  2008/06/17 20:39:13  cdunde
#To add lwo model importer, uv's still not correct though.
#Also added model import\export logging options for file types.
#
#Revision 1.3  2008/06/16 00:11:46  cdunde
#Made importer\exporter logging corrections to work with others
#and started logging function for md2 model importer.
#
#Revision 1.2  2008/06/15 02:41:21  cdunde
#Moved importer\exporter logging to utils file for global use.
#
#Revision 1.1  2008/06/14 07:52:15  cdunde
#Started model importer exporter utilities file.
#
#