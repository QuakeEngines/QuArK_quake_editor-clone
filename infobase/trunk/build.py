#! /usr/bin/env python

#
#  $Header$
#

import string, htmlentitydefs, time, os

EXTENSION = ".txt"


#
# Text-to-HTML character conversion
#
TEXT_TO_HTML = { }
for c in range(256):
    TEXT_TO_HTML[chr(c)] = chr(c)
for entity, character in htmlentitydefs.entitydefs.items():
    TEXT_TO_HTML[character] = "&" + entity + ";"
TEXT_TO_HTML_NBSP = TEXT_TO_HTML.copy()
TEXT_TO_HTML_NBSP[" "] = "&nbsp;"

#
# ------------------------------------------------------------
#

#today = time.strftime("%d %b %Y", time.localtime(time.time()))

def text2html(text):
    return string.join(map(TEXT_TO_HTML.get, text), "")

def text2html_nbsp(text):
    return string.join(map(TEXT_TO_HTML_NBSP.get, text), "")

def path2html(path):
    return string.join(filter(None, string.split(path, "/"))+["html"], ".")

def climbpath(curpath, relpath):
    if relpath[:3] == "../" :
       return climbpath(curpath[:-1],relpath[3:])
    else:
#       print 'CURPATH '+`curpath`
       newpath = string.join(curpath,'/')+relpath
#       print 'NEWPATH '+`newpath`
       return newpath
  

def relpath(curpath, relpath):
    if relpath[0]!='.':
       return relpath
    elif relpath[1]=='/':
       return curpath+relpath[2:]
    elif relpath[1:3]=='./':
       track = string.split(curpath,'/')
       return climbpath(track[:-2],relpath[2:])
       
def findref(root, path, fkw):
#    print 'FKW: '+`fkw["path"]`
    path = relpath(fkw["path"], path)
    print 'PATH: '+`path`
    path0 = path
    path = string.split(path, "/")
    path1 = ""
    while path:
        path1 = path1 + path[0] + "/"
        for folder in root.folders:
            if folder.path == path1:
                root = folder
                del path[0]
                break
        else:
            if len(path) == 1:
                for kw, text in root.files:
                    if kw["hrefaname"] == path[0]:
                        return REFFILE % kw
            raise "Reference not found : " + path0
    return REFDIR % root.kw


def procpic(kw, path):  #tiglari
    picrl = string.join(filter(None, string.split(kw["path"], "/"))+[path], ".")
    img = '<img src="%s">'%picrl
    data = open(kw["path"]+path,"rb").read()
    f = open("output/"+picrl, "wb")
    f.write(data)
    f.close()
    kw["forgotten"].remove(path)
    return img
    
def processtext(root, text, data, kw):
    currentpara = None
    TEXT = 1
    HTML = 2
    variableformat = (string.lower(kw.get("format", ""))!="html")
    for line in text:
        test = string.lower(line)
        if test[:6]=="<text>":
            lineconvert = TEXT
            line = line[6:]
        elif test[:6]=="<html>":
            lineconvert = HTML
            line = line[6:]
        else:
            lineconvert = None
        test = string.strip(test)
        if not test:
            # this line is empty
            currentpara = None
        elif test[:5]=="<ref>":
            # this line is a reference
            line = findref(root, string.strip(string.strip(line)[5:]), kw)
        elif test[:5]=="<pic>": #tiglari
            # this line is an image
            line = procpic(kw, string.strip(string.strip(line)[5:]))
        elif variableformat:
            if test[:3]=="<p>" or currentpara==HTML:
                # this line is direct HTML, no formatting
                currentpara = HTML
                if lineconvert == TEXT:
                    line = text2html(line)
            else:
                # this line is text
                if lineconvert != HTML:
                    line = text2html(line)
                if currentpara!=TEXT:
                    line = "<p>" + line
                    currentpara = TEXT
        data.append(line)


def parse(file):
    f = open(file, "r")
    kw = { }
    while 1:
        line = string.strip(f.readline())
        if not line: break
        key = string.split(line, ":")[0]
        value = string.strip(line[len(key)+1:])
        try:
            data = kw[key]
        except:
            kw[key] = value
        else:
            kw[key] = data+"\n"+value
    return kw, f.readlines(), os.stat(file)[9]


class Folder:

    def __init__(self, path, classif, parents, prev=None):
        self.path = path
#        print 'Path: '+self.path
        self.classif = classif
        if classif: #DECKER
            shortname = string.join(map(lambda s: s+".", classif), "") + "&nbsp;"
        else: #DECKER
            shortname = "" #DECKER - Make the 'index.html' title _not_ prefixed with a single space
        print shortname,
        self.kw, self.text, ctime = parse(self.path+"index"+EXTENSION)
        s = self.kw["title"]
        print s
        self.kw["htmltitle"] = text2html_nbsp(s)
        self.kw["classif"] = shortname
        self.kw["path"] = path
        if not classif:
            shortname = "index.html"
        else:
#            shortname = string.join(filter(None, string.split(self.path, "/"))+["html"], ".")
            shortname = path2html(path)
        self.kw["htmlfile"] = shortname
        if parents:
            self.kw["parenthtmlfile"] = parents[-1].kw["htmlfile"]
        self.folders = []
        self.forgotten = map(string.lower, os.listdir("./"+self.path))
        self.forgotten.remove("index"+EXTENSION)
        self.kw["forgotten"] = self.forgotten
        self.kw["next"]=""
        self.kw["nextfooter"] = ""
        htmlpath = path2html(path)
        previous = None
        if prev:
            nextref = 'Next:&nbsp;<a href="%s">%s</a>'%(htmlpath,self.kw['title'])
            prev.kw["headerlvl"] = prev.kw["headerlvl"]+'<br>'+nextref
#            prev.kw["nextfooter"] = '<a href="%s">next</a>'%htmlpath
            prev.kw["nextfooter"] = nextref
        for foldername in string.split(self.kw.get("subdir", "")):
            folder = Folder(path+foldername+"/", classif+(str(len(self.folders)+1),), parents+(self,), previous)
            if folder.ctime > ctime:
                ctime = folder.ctime
            self.folders.append(folder)
            self.forgotten.remove(foldername)
            previous = folder
        self.files = []
        for filename in string.split(self.kw.get("desc", "")):
            kw, text, ctime1 = parse(self.path+filename+EXTENSION)
            if ctime1>ctime:
                ctime = ctime1
            kw["htmlfile"] = shortname
            kw["hrefaname"] = filename
            kw["updateday"] = time.strftime("%d %b %Y", time.localtime(ctime1))
            kw["path"] = path  # tiglari
            self.files.append((kw, text))
            self.forgotten.remove(filename+EXTENSION)
        self.ctime = ctime
        self.kw["updateday"] = time.strftime("%d %b %Y", time.localtime(ctime))
        if not parents:
            lvl = MAINHEADERLVL
        else:
            lvl = SUBHEADERLVL
            for folder in parents:
                lvl = lvl + HEADERLVL % folder.kw
#        if self.kw["next"]:
#                print 'NEXT'
#                lvl = lvl + 'Next: <a href="%s">%s</a>'%(path2html(self.kw["next"]),self.kw["next"])
        self.kw["headerlvl"] = lvl

    def writefiles(self, root, filewriter):
        print self.kw["htmlfile"], "  [%s]" % self.kw["title"]
        filewriter(self.kw["htmlfile"], self.makefile(root))
        for folder in self.folders:
            folder.writefiles(root, filewriter)

    def makefile(self, root):
        data = [ HEADER % self.kw ]
        processtext(root, self.text, data, self.kw)
        if self.folders:
            data.append(SUBDIR_BEGIN % self.kw)
            for folder in self.folders:
                data.append(SUBDIR_ITEM % folder.kw)
                if folder.folders:
                    data.append(SUBSUBDIR_BEGIN % folder.kw)
                    for subfolder in folder.folders:
                        data.append(SUBSUBDIR_ITEM % subfolder.kw)
                    data.append(SUBSUBDIR_END % folder.kw)
                if folder.files:
                    data.append(SUBFILES_BEGIN % folder.kw)
                    for subfiles in folder.files:
                        data.append(SUBFILES_ITEM % subfiles[0])
                    data.append(SUBFILES_END % folder.kw)
            data.append(SUBDIR_END % self.kw)
        if self.files:
            data.append(FILES_BEGIN % self.kw)
            for kw, text in self.files:
                data.append(FILES_ITEM % kw)
            data.append(FILES_MIDDLE % self.kw)
            for kw, text in self.files:
                data.append(FILE_BEGIN % kw)
                processtext(root, text, data, kw)
                data.append(FILE_END % kw)
            data.append(FILES_END % self.kw)
        data.append(FOOTER % self.kw)
        return data

    def viewforgotten(self):
        for s in self.forgotten:
            if s[-1:]!="~" and s!="cvs":
                print "*** NOTE: file '%s' not found in index" % (self.path+s)
        for folder in self.folders:
            folder.viewforgotten()


def run(filewriter):
    # load format file
    execfile("format"+EXTENSION, globals(), globals())
    # recursively load everything in memory
    root = Folder("", (), ())
    print "-"*50
    # recursively write everything to disk
    root.writefiles(root, filewriter)
    for filename in string.split(root.kw.get("extrafiles_text", "")):
        filewriter(filename, [open(filename, "r").read()])
    for filename in string.split(root.kw.get("extrafiles_binary", "")):
        filewriter(filename, [open(filename, "rb").read()], "wb")
    root.forgotten = []
    root.viewforgotten()


def defaultwriter(filename, data, writemode="w"):
    # write the target file
    f = open("output/"+filename, writemode)
    f.writelines(data)
    f.close()

run(defaultwriter)

#
# $Log$
# Revision 1.5  2000/10/18 16:39:34  tiglari
# added image-handling facility, preliminary
#
#