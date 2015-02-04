#!/usr/bin/python2
import os
print "<!DOCTYPE HTML><html><head></head><body>\n"
print "<table>\n"
for dirname, dirnames, filenames in os.walk('.'):
    for filename in filenames:
        if filename.strip()[-3:]=="pdf":
            print "<tr><td></td><td></td><td><a href='"+os.path.join(dirname, filename)+"'>"+os.path.join(dirname, filename).strip()[2:]+"</td><td></td><td></td></tr>\n"
    # editing the 'dirnames' list will stop os.walk() from recursing into there.
    if '.git' in dirnames:
        # don't go into any .git directories.
        dirnames.remove('.git')
    if 'tex' in dirnames:
     	dirnames.remove('tex')
print "</table>"
print "</body></html>"
