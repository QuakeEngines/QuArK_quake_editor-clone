#! /usr/bin/env python

#
#  $Header$
#

import string, htmlentitydefs, time, os, sys

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
    newtext = string.join(map(TEXT_TO_HTML.get, text), "")
    # Fix a problem with "&lt;" "&gt;" becomming "&amp;lt;" "&amp;gt;"
    newtext = string.replace(newtext, "&amp;lt;",   "&lt;")
    newtext = string.replace(newtext, "&amp;gt;",   "&gt;")
    # Hmmm? Lets fix "&nbsp;" too
    newtext = string.replace(newtext, "&amp;nbsp;", "&nbsp;")
    return newtext

def text2html_nbsp(text, maxlen=999):
    if (len(text) > maxlen):
        text = text[:maxlen] + "..."
    return string.join(map(TEXT_TO_HTML_NBSP.get, text), "")

def path2html(path):
    return string.join(filter(None, string.split(path, "/"))+["html"], ".")

def climbpath(curpath, relpath):
    if relpath[:3] == "../" :
       return climbpath(curpath[:-1], relpath[3:])
    else:
#      print 'CURPATH ' + `curpath`
       newpath = string.join(curpath, '/') + relpath
#      print 'NEWPATH ' + `newpath`
       return newpath


def relpath(curpath, relpath):
    if relpath[0] != '.':
       return relpath
    elif relpath[1] == '/':
       return curpath + relpath[2:]
    elif relpath[1:3] == './':
       track = string.split(curpath, '/')
       return climbpath(track[:-2], relpath[2:])

def findref(root, path, name, fkw, extraargs):
#   print 'FKW: ' + `fkw["path"]`

#    def ref(refnormal, refwithname, kw, name=name, extraargs):
    def ref(refnormal, refwithname, kw, name=name):
        if name == "":
            return refnormal % kw
        else:
            kw['refname'] = name
            return refwithname % kw

    path = relpath(fkw["path"], path)
#   print 'PATH: ' + `path`
#   print 'name: ' + `name`
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
                        return ref(REFFILE, REFFILE_NAME, kw)
            raise "Reference not found to " + path0 + " in " + fkw["htmlfile"]
    return ref(REFDIR, REFDIR_NAME, root.kw)

def proc_g(kw, words):
    # '<g>...</g>' for Glossary-link
    # Ugly hack! This needs proper fixing, and not this semi-hardcoded bullshit.
    if words[:1] == '.':
        namelink = "fileext"
    elif words[:1] >= '0' and words[:1] <= '9':
        namelink = "numbers"
    else:
        namelink = string.lower(words[:1])
    return "<a href=\"glossary.html#%s\">%s</a>" % (namelink, words)

def procpic(kw, path, extraargs):  #tiglari
    if (string.find(path, "/") > -1) or (string.find(path, "\\") > -1) or (path[:1] == "."):
        raise "Illegal picture filename: [%s]" % path
    picrl = string.join(filter(None, string.split(kw["path"], "/"))+[path], ".")
    img = '<img %s src="%s">' % (extraargs, picrl)
    try:
        data = open(kw["path"]+path, "rb").read()
    except:
        raise "open-error for file \"%s\"" % (kw["path"]+path)
    f = open("output/"+picrl, "wb")
    f.write(data)
    f.close()
#    kw["forgotten"].remove(path)
    return img

def procrsc(kw, path):  #tiglari
    rscrl = string.join(filter(None, string.split(kw["path"], "/"))+[path], ".")
    data = open(kw["path"]+path, "rb").read()
    f = open("output/"+rscrl, "wb")
    f.write(data)
    f.close()
#    kw["forgotten"].remove(path)
    return '"%s"' % rscrl

def proczip(kw, path):  #tiglari
    if localMode:
        data = open("zips/"+path, "rb").read()
        if not os.path.exists("output/zips"):
            os.mkdir("output/zips")
        f = open("output/zips/"+path, "wb")
        f.write(data)
        f.close()
        return '<a href="%s">%s</a>' % (path, path)
    else:
        return '<a href="%s%s">%s</a>' % (ZIPLOC, path, path)

def procact(kw, actionstring):
    # An 'action' is usually composed of a series of menu-actions the user
    # has to drill into. An example: "<act> RMB | Curves|Arch </act>"
    actionstring = string.replace(actionstring, " | ", " -&gt; ")
    actionstring = string.replace(actionstring, "|",   " -&gt; ")
    return ACT_HTML % actionstring


def processtext(root, text, data, kw):

    def perform_tag_action(tag, line, flags, root, kw):

        def perform_ref_action(extraargs, datastring, root, kw):
            datastring = string.strip(datastring)
            try:
                # figure out, if there is a alternative text for the link-reference
                idx = string.index(datastring, '\\')
                pathname = string.strip(datastring[:idx])
                refname = string.strip(datastring[idx+1:])
            except (ValueError):
                pathname = datastring
                refname = "";
            return findref(root, pathname, refname, kw, string.strip(extraargs))

        def perform_pic_action(extraargs, datastring, root, kw):
            return procpic(kw, string.strip(datastring), string.strip(extraargs))

        def perform_zip_action(datastring, root, kw):
            return proczip(kw, string.strip(datastring))

        def perform_rsc_action(datastring, root, kw):
            return procrsc(kw, string.strip(datastring))

        def perform_act_action(datastring, root, kw):
            return procact(kw, string.strip(datastring))

        def perform_g_action(datastring, root, kw):
            return proc_g(kw, string.strip(datastring))


        if (tag[:5] == "<code"):
            replacewith = "<div class=\"doccode\"><pre>"
            flags["preformatmode"] = flags["preformatmode"] + 1
        elif (tag[:6] == "</code"):
            replacewith = "</pre></div>"
            if (flags["preformatmode"] > 0):
                flags["preformatmode"] = flags["preformatmode"] - 1
        elif (tag[:4] == "<tt>"):
            replacewith = "&nbsp;<tt>"
        elif (tag[:5] == "</tt>"):
            replacewith = "</tt>&nbsp;"
        elif (tag[:4] == "<ref"):
            end_tag = string.find(line, "</ref>")
            if end_tag == -1:
                # A <ref>-tag must have a </ref>-tag on the same line, else this code won't work.
                raise "<ref>-tag without any </ref>-tag on same line! <File>.TXT title: \"%s\"" % kw["title"]
            replacewith = perform_ref_action(tag[4:-1], line[:end_tag], root, kw)
            line = line[end_tag+len("</ref>"):]
        elif (tag[:4] == "<img"):
            end_tag = string.find(line, "</img>")
            if end_tag == -1:
                # A <img>-tag must have a </img>-tag on the same line, else this code won't work.
                raise "<img>-tag without any </img>-tag on same line! <File>.TXT title: \"%s\"" % kw["title"]
            replacewith = perform_pic_action(tag[4:-1], line[:end_tag], root, kw)
            line = line[end_tag+len("</img>"):]
        elif (tag[:4] == "<pic"):
            end_tag = string.find(line, "</pic>")
            if end_tag == -1:
                # A <pic>-tag must have a </pic>-tag on the same line, else this code won't work.
                raise "<pic>-tag without any </pic>-tag on same line! <File>.TXT title: \"%s\"" % kw["title"]
            replacewith = perform_pic_action(tag[4:-1], line[:end_tag], root, kw)
            line = line[end_tag+len("</pic>"):]
        elif (tag[:4] == "<zip"):
            end_tag = string.find(line, "</zip>")
            if end_tag == -1:
                # A <zip>-tag must have a </zip>-tag on the same line, else this code won't work.
                raise "<zip>-tag without any </zip>-tag on same line! <File>.TXT title: \"%s\"" % kw["title"]
            replacewith = perform_zip_action(line[:end_tag], root, kw)
            line = line[end_tag+len("</zip>"):]
        elif (tag[:4] == "<rsc"):
            end_tag = string.find(line, "</rsc>")
            if end_tag == -1:
                # A <rsc>-tag must have a </rsc>-tag on the same line, else this code won't work.
                raise "<rsc>-tag without any </rsc>-tag on same line! <File>.TXT title: \"%s\"" % kw["title"]
            replacewith = perform_rsc_action(line[:end_tag], root, kw)
            line = line[end_tag+len("</rsc>"):]
        elif (tag[:4] == "<act"):
            end_tag = string.find(line, "</act>")
            if end_tag == -1:
                # A <act>-tag must have a </act>-tag on the same line, else this code won't work.
                raise "<act>-tag without any </act>-tag on same line! <File>.TXT title: \"%s\"" % kw["title"]
            replacewith = perform_act_action(line[:end_tag], root, kw)
            line = line[end_tag+len("</act>"):]
        elif (tag[:2] == "<g"):
            end_tag = string.find(line, "</g>")
            if end_tag == -1:
                # A <g>-tag must have a </g>-tag on the same line, else this code won't work.
                raise "<g>-tag without any </g>-tag on same line! <File>.TXT title: \"%s\"" % kw["title"]
            replacewith = perform_g_action(line[:end_tag], root, kw)
            line = line[end_tag+len("</g>"):]
        elif (tag[:4] == "</i>"):
            replacewith = tag
            if (line[:6] <> "&nbsp;"):
                # Force in a non-breakable-space after end-of-italic.
                replacewith = replacewith + "&nbsp;"
        elif (tag[:2] == "< "):
            raise "Illegal use of '<'-char. Use '&lt;' if a single '<' is needed! <File>.TXT title: \"%s\"" % kw["title"]
        else:
            replacewith = tag
            if (tag[:4] == "<pre"):
                flags["preformatmode"] = flags["preformatmode"] + 1
            elif (tag[:5] == "</pre"):
                if (flags["preformatmode"] > 0):
                    flags["preformatmode"] = flags["preformatmode"] - 1
        return replacewith, line, flags

    paragraf_tags_added = 0
    flags = { }
    flags["prevlineempty"] = 1
    flags["preformatmode"] = 0
    flags["inhtmlcomment"] = 0

    for line in text:
        correctedline = ""
        trimmedline = string.strip(line)
        if not trimmedline:
            correctedline = "\n"
            flags["prevlineempty"] = 1
            if (paragraf_tags_added > 0) and (flags["preformatmode"] == 0) and (flags["inhtmlcomment"] == 0):
                correctedline = "</p>"
                paragraf_tags_added = paragraf_tags_added - 1
        else:
            # Scan through the 'line' in search for "<tag's" to replace/perform actions on
            while len(line) > 0:
                if (flags["inhtmlcomment"] == 1):
                    endofcomment_found = string.find(line, "-->")
                    if endofcomment_found == -1:
                        # We're still in HTML-comment
                        correctedline = correctedline + line
                        line = ""
                    else:
                        # Exiting HTML-comment mode
                        correctedline = correctedline + line[:endofcomment_found+len("-->")]
                        line = line[endofcomment_found+len("-->"):]
                        flags["inhtmlcomment"] = 0
                else:
                    startchar_tag_found = string.find(line, "<")
                    if startchar_tag_found == -1:
                        # No "<tag" were found, so just copy the entire line
                        correctedline = correctedline + text2html(line)
                        line = ""
                    else:
                        # Found a "<tag". Take anything before that, and append to 'correctedline'
                        correctedline = correctedline + text2html(line[:startchar_tag_found])
                        line = line[startchar_tag_found:]
                        if (line[:4] == "<!--"):
                            flags["inhtmlcomment"] = 1
                            correctedappend = line[:len("<!--")]
                            line = line[len("<!--"):]
                        else:
                            endchar_tag_found = string.find(line, ">")
                            if endchar_tag_found == -1:
                                # there must exist an endchar_tag on the same line!
                                raise "'%s' without ending '>' problem! <File>.TXT title: \"%s\"" % (line[:5], kw["title"])
                            else:
                                tag = string.lower(line[:endchar_tag_found+1])
                                if (tag == "<p>") or (tag == "</p>") or (tag[:5] == "<html"):
                                    # do now allow these tags anymore!
                                    raise "The %s tag is not allowed! <File>.TXT title: \"%s\"" % (tag, kw["title"])
                                correctedappend, line, line_flags = perform_tag_action(tag, line[endchar_tag_found+1:], flags, root, kw)

                        correctedline = correctedline + correctedappend

            if flags["prevlineempty"] == 1:
                if (flags["preformatmode"] == 0) and (flags["inhtmlcomment"] == 0):
                    # prepend with paragraf-tag
                    correctedline = "<p>" + correctedline
                    paragraf_tags_added = paragraf_tags_added + 1

            flags["prevlineempty"] = 0

        data.append(correctedline)

    for ptags in range(paragraf_tags_added):
        data.append("</p>")

def parse(file):
    try:
        f = open(file, "r")
    except:
        raise "File missing:", file
    kw = { }
    # Read the beginning non-empty lines, which should contain "key: value"'s
    while 1:
        line = string.strip(f.readline())
        if not line: # empty line found, stop reading for "key: value"'s
            break
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
#       print shortname,
        self.kw, self.text, lastmodifydate = parse(self.path + "index" + EXTENSION)
        s = self.kw["title"]
#       print s
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
#       print self.kw["htmlfile"], "  [%s]" % self.kw["title"]
        filewriter(self.kw["htmlfile"], self.makefile(root))
        for folder in self.folders:
            folder.writefiles(root, filewriter)

    def makefile(self, root):
        data = [ HEADER_BEGIN % self.kw ]
        processtext(root, self.text, data, self.kw)
        data.append(HEADER_END)
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
                    if len(folder.files) < 11:
                        data.append(SUBFILES_BEGIN % folder.kw)
                        for subfiles in folder.files:
                            data.append(SUBFILES_ITEM % subfiles[0])
                        data.append(SUBFILES_END % folder.kw)
                    else:
                        # If more than 10 files, put into two columns
                        data.append(SUBFILES_TABLEBEGIN);
                        data.append(SUBFILES_BEGIN % folder.kw)
                        cnt = 0
                        for subfiles in folder.files:
                            if cnt == ((len(folder.files)+1) / 2):
                                data.append(SUBFILES_END % folder.kw)
                                data.append(SUBFILES_TABLEMIDDLE);
                                data.append(SUBFILES_BEGIN % folder.kw)
                            data.append(SUBFILES_ITEM % subfiles[0])
                            cnt = cnt + 1
                        data.append(SUBFILES_END % folder.kw)
                        data.append(SUBFILES_TABLEEND);
            data.append(SUBDIR_END % self.kw)
        if self.files:
            data.append(FILES_BEGIN % self.kw)
            if len(self.files) < 11:
                data.append(FILES_ITEMBEGIN % self.kw)
                for kw, text in self.files:
                    data.append(FILES_ITEM % kw)
                data.append(FILES_ITEMEND % self.kw)
            else:
                # If more than 10 files, put into two columns
                data.append(SUBFILES_TABLEBEGIN);
                data.append(FILES_ITEMBEGIN % self.kw)
                cnt = 0
                for kw, text in self.files:
                    if cnt == ((len(self.files)+1) / 2):
                        data.append(FILES_ITEMEND % self.kw)
                        data.append(SUBFILES_TABLEMIDDLE);
                        data.append(FILES_ITEMBEGIN % self.kw)
                    data.append(FILES_ITEM % kw)
                    cnt = cnt + 1
                data.append(FILES_ITEMEND % self.kw)
                data.append(SUBFILES_TABLEEND);
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
    # recursively set navigation links
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

localMode=0
for flag in sys.argv:
    if flag=='-local':
        localMode=1
if not os.path.exists('output'):
    os.mkdir('output')

run(defaultwriter)

#
# $Log$
# Revision 1.16  2001/02/28 19:54:10  tiglari
# removed extraarg from ref in findref
#
# Revision 1.15  2001/02/28 19:12:25  decker_dk
# Added <g>...</g> Glossary-links. Though not the best method.
#
# Revision 1.14  2001/02/25 16:38:22  decker_dk
# Added <act> </act> functionality
#
# Revision 1.13  2001/02/20 19:33:14  decker_dk
# Changed to .PNG image-format, and a comment in BUILD.PY
#
# Revision 1.12  2001/02/15 19:43:16  decker_dk
# Recoded the BUILD.PY to support somewhat basic-HTML.
#
# Revision 1.11  2000/11/12 06:31:50  tiglari
# <REF> file \ name
# <ZIP> file.zip
#
# Revision 1.10  2000/11/02 06:36:24  tiglari
# support for explicit names in REF's
#
# Revision 1.9  2000/11/01 21:15:23  decker_dk
# Misc. updates.
#
# Revision 1.8  2000/10/29 03:04:04  tiglari
# added <rsc> (resource) tag to get a resource renamed & shifted into the output
# in the same style as <pic>, but only the quoted new name is returned into
# the doc, so that the thing can be part of a normal <img > etc. tag. eg:
#  ...<img src=
# <rsc>coolpic.jpg
# width=200 height=100>...
#
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
