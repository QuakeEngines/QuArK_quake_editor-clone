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

def text2html_nbsp(text, maxlen=999):
    if (len(text) > maxlen):
        text = text[:maxlen] + "..."
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
    
def procrsc(kw, path):  #tiglari
    rscrl = string.join(filter(None, string.split(kw["path"], "/"))+[path], ".")
    data = open(kw["path"]+path,"rb").read()
    f = open("output/"+rscrl, "wb")
    f.write(data)
    f.close()
    kw["forgotten"].remove(path)
    return '"%s"'%rscrl
    
def processtext(root, text, data, kw):
    currentpara = None
    TEXT = 1
    HTML = 2
#+ Those lines with "#+" are included, so the proper </p> tag is added always,
#+ else the formatting won't be proper... It will look bad actually :-/ So better
#+ remember to add those </p> tags, when doing your own <html>!! /Decker
    p_tag_added = 0 #+
    variableformat = (string.lower(kw.get("format", "")) != "html")
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
            if (p_tag_added > 0):               #+
                line = "</p>"                   #+
                p_tag_added = p_tag_added - 1   #+

#! It would be great, if we could implement start _and_ end tags. Something like;
#! "and if you look at <ref> ../dir/file </ref>, then it would" - having these
#! tags directly _in_ the text. /Decker

        elif test[:5]=="<ref>":
            # this line is a reference
            line = findref(root, string.strip(string.strip(line)[5:]), kw)
        elif test[:5]=="<pic>": #tiglari
            # this line is an image
            line = procpic(kw, string.strip(string.strip(line)[5:]))
        elif test[:5]=="<rsc>": #tiglari
            # this line names a resource we want to
            # be moved into output and renamed like a pic or ref
            # function returns only the new name, double-quoted
            line = procrsc(kw, string.strip(string.strip(line)[5:]))
        # Tags to turn on/off html-conversion
        elif test[:9]=="<html on>": # Decker
            currentpara = HTML
            variableformat = 0
            line=line[10:]
        elif test[:10]=="<html off>": # Decker
            currentpara = None
            variableformat = (string.lower(kw.get("format", "")) != "html")
            line=line[11:]

        # Tags to turn on/off code-text which is preformattet
        elif test[:6]=="<code>": # Decker
            currentpara = HTML
            variableformat = 0
            line="<div class=\"doccode\"><pre>"
        elif test[:7]=="</code>": # Decker
            currentpara = None
            variableformat = (string.lower(kw.get("format", "")) != "html")
            line="</pre></div>"

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
                if currentpara != TEXT:
                    line = "<p>" + line
                    p_tag_added = p_tag_added + 1   #+
                    currentpara = TEXT

        data.append(line)

    for ptags in range(p_tag_added):    #+
        data.append("</p>")             #+

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
    return kw, f.readlines(), os.stat(file)[8] # Decker - changed from [9] to [8] to get the right file-modification-date on Win2K

class Folder:

    def __init__(self, path, classif, parents, prev=None):
        self.prev = prev
        self.parents = parents
        self.path = path
#        print 'Path: '+self.path
        self.classif = classif
        if classif: # Decker
            shortname = string.join(map(lambda s: s+".", classif), "") + "&nbsp;"
        else: # Decker
            shortname = "" # Decker - Make the 'index.html' title _not_ prefixed with a single space
        print shortname,
        self.kw, self.text, lastmodifydate = parse(self.path + "index" + EXTENSION)
        s = self.kw["title"]
        print s
        self.kw["htmltitle"] = text2html_nbsp(s)
        self.kw["htmltitleshort"] = text2html_nbsp(s, 25) # Decker - Try to prevent text-wrapping, so make it max 25 characters long
        self.kw["classif"] = shortname
        self.kw["path"] = path
        if not classif:
            shortname = "index.html"
        else:
            shortname = path2html(path)
        self.kw["htmlfile"] = shortname
        self.kw["navprev"] = NAVNOPREV
        self.kw["navup"]   = NAVNOUP
        self.kw["navnext"] = NAVNONEXT
        if parents:
            self.kw["parenthtmlfile"] = parents[-1].kw["htmlfile"]
            self.kw["navup"] = NAVUP % parents[-1].kw
        # Recusivee into sub-folders
        self.folders = []
        self.forgotten = map(string.lower, os.listdir("./" + self.path))
        self.forgotten.remove("index" + EXTENSION)
        self.kw["forgotten"] = self.forgotten
        self.kw["next"]=""
        self.kw["nextfooter"] = ""
        htmlpath = path2html(path)
        previous = None
        for foldername in string.split(self.kw.get("subdir", "")):
            folder = Folder(path + foldername + "/", classif + (str(len(self.folders) + 1),), parents + (self,), previous)
            if folder.lastmodifydate > lastmodifydate:
                lastmodifydate = folder.lastmodifydate
            self.folders.append(folder)
            self.forgotten.remove(foldername)
            previous = folder
        self.files = []
        for filename in string.split(self.kw.get("desc", "")):
            kw, text, lastmodifydate1 = parse(self.path + filename + EXTENSION)
            if lastmodifydate1 > lastmodifydate:
                lastmodifydate = lastmodifydate1
            kw["htmlfile"] = shortname
            kw["hrefaname"] = filename
            kw["updateday"] = time.strftime("%d %b %Y", time.localtime(lastmodifydate1))
            kw["path"] = path  # tiglari
            self.files.append((kw, text))
            self.forgotten.remove(filename + EXTENSION)
        self.lastmodifydate = lastmodifydate
        self.kw["updateday"] = time.strftime("%d %b %Y", time.localtime(lastmodifydate))
        # Setup backwards navigation links
        if not parents:
            lvl = MAINHEADERLVL
        else:
            lvl = SUBHEADERLVL
            for folder in parents:
                lvl = lvl + HEADERLVL % folder.kw
        self.kw["headerlvl"] = lvl

    def navigation(self):
        # Setup navigation links (Prev-Up-Next) # Decker
        try:
            prev = self.parents[-1]
            i = len(prev.folders) - 1
            while (i >= 0 and prev.folders[i] != self):
                i = i - 1
            if (i > 0):
                prev = prev.folders[i - 1]
                while (len(prev.folders) > 0):
                    prev = prev.folders[-1]
            prev.kw["navnext"] = NAVNEXT % self.kw
            self.kw["navprev"] = NAVPREV % prev.kw
        except:
            pass
        for folder in self.folders:
            folder.navigation()

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


def defaultwriter(filename, data, writemode="w"):
    # write the target file
    f = open("output/"+filename, writemode)
    f.writelines(data)
    f.close()

def run(filewriter):
    # load format file
    execfile("format"+EXTENSION, globals(), globals())
    # recursively load everything in memory
    root = Folder("", (), ())
    print "-"*50
    root.navigation() # Decker
    print "-"*50
    # recursively write everything to disk
    root.writefiles(root, filewriter)
    for filename in string.split(root.kw.get("extrafiles_text", "")):
        filewriter(filename, [open(filename, "r").read()])
    for filename in string.split(root.kw.get("extrafiles_binary", "")):
        filewriter(filename, [open(filename, "rb").read()], "wb")
    root.forgotten = []
    root.viewforgotten()



run(defaultwriter)

#
# $Log$
# Revision 1.7  2000/10/24 19:43:13  decker_dk
# Prev/Up/Next navigation, new CSS and misc. changes.
#
# Revision 1.6  2000/10/19 20:06:39  tiglari
# relative paths (./,../) for <pic> and <ref>
# cross-links to next added to output
#
# Revision 1.5  2000/10/18 16:39:34  tiglari
# added image-handling facility, preliminary
#
#
