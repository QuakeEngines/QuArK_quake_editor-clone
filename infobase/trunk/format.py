#
# Document formatting - this is a Python-script that is executed from within Build.PY
#

EXTENSION = ".txt"

#
# Location for zip etc. archives for non-local version
#
ZIPLOC = "http://quark.sourceforge.net/infobase/" #Was: ftp://ftp.fileplanet.com/

# Path for all the pictures
PICLOC = "pics/"

HEADER_BEGIN = """<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"  "http://www.w3.org/TR/html4/loose.dtd">
<html lang="en">
<head>
  <title>%(title)s</title>
  <meta name="Description" content="QuArK Information Database - Page: %(classif)s%(title)s">
  <meta name="Keywords" content="QuArK InfoBase, Quake Army Knife, QRK, QKM, Python, PY, Map Editor, Model Editor">
  <link rel=stylesheet href="standard.css" type="text/css">
</head>

<body>
<a name="__top__"></a>
<table class="tight" width="100%%" border=0 cellspacing=0>
  <tr>
    <td width=213>
      <a target="_blank" href="http://quark.sourceforge.net/"><img src="quarkicon.png" width=213 height=90 border=0 alt="Go to QuArK Web Site"></a>
    </td>
    <td width="70%%" align=center>
      <div class="topheadline">%(title)s</div>
      <div class="sm">Updated&nbsp;%(updateday)s</div>
    </td>
    <td width="30%%" valign=bottom nowrap>
      %(headerlvl)s
    </td>
  </tr>
</table>
<br>
<table class="tight" width="100%%" border=0 cellspacing=0>
  <tr class="headline">
    <td width="99%%">
      <p class="headline">&nbsp;%(classif)s%(title)s</p>
    </td>
    <td width="1%%" align=right nowrap>
      &nbsp;%(navprev)s%(navup)s%(navnext)s&nbsp;
    </td>
  </tr>
</table>
<table border=0 width="100%%" cellspacing=10><tr><td>"""
HEADER_END = """</td></tr></table>
<br>
"""

# nesting-level-dependent formatting
#  main page
MAINHEADERLVL = """Upper&nbsp;levels:<br>-&nbsp;<i>None</i><br>"""
#  other pages begin with this
SUBHEADERLVL  = """Upper&nbsp;levels:<br>"""
#  then repeat this
HEADERLVL     = """-&nbsp;<a href="%(htmlfile)s">%(classif)s%(htmltitleshort)s</a><br>"""

FOOTER = """
<table class="tight" width="100%%" border=0 cellspacing=0>
  <tr class="headline">
    <td width="99%%" align=center>
      <p class="sm">
        Copyright (c) 2009, GNU General Public License by The QuArK (Quake Army Knife) Community - <a target="_blank" href="http://quark.sourceforge.net/">http://quark.sourceforge.net/</a>
      </p>
    </td>
    <td width="1%%" align=right nowrap>
      &nbsp;%(navprev)s-&nbsp;<a href="#__top__">Top</a>&nbsp;-%(navnext)s&nbsp;
    </td>
  </tr>
</table>
</body>
</html>
"""

#
# Formatting: List of folders
#
SUBDIR_BEGIN = """
<table class="tight" width="100%%" border=0 cellspacing=0>
  <tr class="headline">
    <td width="100%%">
      <p class="headline">&nbsp;Sections</p>
    </td>
  </tr>
</table>
<ul class="section">"""
SUBDIR_ITEM_BEGIN = """
  <li><b>%(classif)s&nbsp;<a href="%(htmlfile)s">%(title)s</a></b>&nbsp;<span class="added">(%(updateday)s)</span>"""
SUBDIR_ITEM_MORE = """<br>"""
SUBDIR_ITEM_END = """</li>"""
SUBDIR_END = """
</ul>
<br>
"""

#
# Formatting: List of files (first an index with titles only, then with detailed answers)
#
FILES_BEGIN = """
<table class="tight" width="100%%" border=0 cellspacing=0>
  <tr class="headline">
    <td width="100%%">
      <p class="subheadline">&nbsp;Index</p>
    </td>
  </tr>
</table>"""

FILES_ITEMBEGIN = """
<ul class="index">"""
FILES_ITEM = """
  <li>- <a href="#%(hrefaname)s">%(title)s</a>&nbsp;<span class="added">(%(updateday)s)</span>"""
FILES_ITEMEND = """
</ul>"""

FILES_MIDDLE = """
<br>

"""
FILES_END = """
<br>
"""

FILE_BEGIN = """
<a name="%(hrefaname)s"></a>
  <table class="tight" width="100%%" border=0 cellspacing=0>
    <tr class="headline">
      <td>
        <p class="item">&nbsp;%(title)s</p>
      </td>
      <td align=right>
        <font size=-2>%(author)s&nbsp;-&nbsp;%(updateday)s</font>
      </td>
      <td width="1%%" align=right nowrap>
        &nbsp;&nbsp;[&nbsp;<a href="#__top__">Top</a>&nbsp;]&nbsp;
      </td>
    </tr>
  </table>
  <table border=0 width="100%%" cellspacing=10><tr><td>
"""
FILE_END = """
  </td></tr></table>
  <br>
"""

#
# Sub-sub-folder list inside a folder list (one level of nesting)
#
SUBSUBDIR_BEGIN = """
  <ul class="subsection">"""
SUBSUBDIR_ITEM  = """
    <li>%(classif)s<a href="%(htmlfile)s">%(title)s</a>&nbsp;<span class="added">(%(updateday)s)</span></li>"""
SUBSUBDIR_END   = """
  </ul>"""

#
# Sub-files list inside a folder list (one level of nesting too)
#
SUBFILES_BEGIN = """
  <ul class="index">"""
SUBFILES_ITEM  = """
    <li>- <a href="%(htmlfile)s#%(hrefaname)s">%(title)s</a>&nbsp;<span class="added">(%(updateday)s)</span></li>"""
SUBFILES_END   = """
  </ul>"""

#
# Split up into two columns
#
SUBFILES_TABLEBEGIN = """
  <table class="tight" border=0><tr><td width="50%%" valign=top>"""
SUBFILES_TABLEMIDDLE = """
  </td><td width="50%%" valign=top>"""
SUBFILES_TABLEEND = """
  </td></tr></table>"""

#
# Cross-references
#
REFFILE      = """<a href="%(htmlfile)s#%(hrefaname)s">'%(title)s'</a>"""
REFDIR       = """<a href="%(htmlfile)s">'%(title)s'</a>"""
REFFILE_NAME = """<a href="%(htmlfile)s#%(hrefaname)s">%(refname)s</a>"""
REFDIR_NAME  = """<a href="%(htmlfile)s">%(refname)s</a>"""
REFPIC       = """<img src="%(imgfile)s">"""

#
# Navigation buttons
#
## Enabled
NAVPREV   = """[&nbsp;<span class="navenable"><a href="%(htmlfile)s">Prev</a></span>&nbsp;"""
NAVUP     = """-&nbsp;<span class="navenable"><a href="%(htmlfile)s">Up</a></span>&nbsp;-"""
NAVNEXT   = """&nbsp;<span class="navenable"><a href="%(htmlfile)s">Next</a></span>&nbsp;]"""
## Disabled
NAVNOPREV = """[&nbsp;<span class="navdisable">Prev</span>&nbsp;"""
NAVNOUP   = """-&nbsp;<span class="navdisable">Up</span>&nbsp;-"""
NAVNONEXT = """&nbsp;<span class="navdisable">Next</span>&nbsp;]"""

#
# Misc.
#
ACT_HTML = """<span class="act">'%s'</span>"""
