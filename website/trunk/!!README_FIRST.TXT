Notice!
-------
All these files must be in UNIX format. Meaning that a newline only consists
of the single "0x0A" character, and not the DOS format of two characters
"0x0D 0x0A".

This is because the .PHP3 files, once uploaded to dynamic.gamespy.com/~quark,
will result in parsing-errors if they are in DOS format, since the web-server
'dynamic.gamespy.com' is a UNIX box.


What is this?
-------------
These are the .PHP3 and .HTML files which builds the website of QuArK.

Note that this CVS-Repository module "website", should NOT contain any
image-files (.GIF, .PNG, .JPG, etc.), mainly because of their binary nature
ain't suited for CVS version control. Let the image-files be kept on
PlanetQuake's webserver, in the \quark\pics directory.

Note too, that I haven't placed the '.\news\newsYYYYMMDD.txt' files, in this
CVS-repository either, but if it is needed they can easily be CVS version
controlled as well.


How do I modify these files?
----------------------------
Start by using CVS (WinCVS) to checkout the 'website' module, and then after
that, do regular updates of the module. Just like you do with the other QuArK
CVS modules; 'source', 'runtime', 'infobase', 'qrksoftg' and 'utils'.

Be aware of possible CVS conflicts, when doing CVS updates. You need to fix
these conflicts yourself, before continuing modifying the files!

Modify the .PHP3 or .HTML files as needed, and if possible test them on your
own personal web-server which should be configured to run PHP server-site
scripts (find out more about PHP at the site http://www.php.net)

Now CVS commit your changes to the files, so they will be available to others
at the CVS-repository.

You then need to FTP-upload the altered files to the QuArK website, located
at the dynamic.gamespy.com FTP-server/ASP+PHP-webserver, using your account
for that FTP-server. Note that PlanetQuake/Gamespy Industries asks to place
image-files on the ftp.planetquake.com FTP-server/web-server, so do that.

Note! Don't upload the 'CVS' directory to the dynamic.gamespy.com FTP-server.
It is of no use there. Only CVS (WinCVS) uses the 'CVS' directory on your
local machine, and it does not contain anyrelevant information for others.


/Decker (2003-02-25)
