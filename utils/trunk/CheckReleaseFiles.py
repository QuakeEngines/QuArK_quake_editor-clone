import os

dir = "Z:\\workspace\\Copy of runtime"

if os.path.exists(os.path.join(dir, ".svn")):
	print("WARNING: SVN directory found: %s" % (os.path.join(dir, ".svn"), ))

if not os.path.exists(os.path.join(dir, "Setup.qrk")):
	print("WARNING: Setup.qrk file found: %s" % (os.path.join(dir, "Setup.qrk"), ))

if not os.path.exists(os.path.join(dir, "help")):
	print("WARNING: Help directory missing: %s" % (os.path.join(dir, "help"), ))

if not os.path.exists(os.path.join(dir, "QuArK.exe")):
	print("WARNING: QuArK executable missing: %s" % (os.path.join(dir, "QuArK.exe"), ))

for filename in os.listdir(os.path.join(dir, "plugins")):
	full_filename = os.path.join(dir, "plugins", filename)
	if os.path.isdir(full_filename):
		print("WARNING: Unexpected directory: %s" % (full_filename, ))
		continue
	if filename == ".svn":
		print("WARNING: SVN directory found: %s" % (full_filename, ))
		continue
	if full_filename.endswith(".pyc"):
		print("WARNING: Compiled Python file found: %s" % (full_filename, ))
		continue
	#@

for filename in os.listdir(os.path.join(dir, "quarkpy")):
	full_filename = os.path.join(dir, "quarkpy", filename)
	if os.path.isdir(full_filename):
		print("WARNING: Unexpected directory: %s" % (full_filename, ))
		continue
	if filename == ".svn":
		print("WARNING: SVN directory found: %s" % (full_filename, ))
		continue
	if full_filename.endswith(".pyc"):
		print("WARNING: Compiled Python file found: %s" % (full_filename, ))
		continue
	#@
