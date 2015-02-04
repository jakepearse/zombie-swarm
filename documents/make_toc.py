import os
print "<!DOCTYPE HTML><html><head></head><body><ol>\n"
for dirname, dirnames, filenames in os.walk('.'):
    for filename in filenames:
        if filename[:3]=="pdf":
            print "<li><a href='"+os.path.join(dirname, filename)+"'>"+os.path.join(dirname, filename).strip()[2:]+"</li>\n"
    # editing the 'dirnames' list will stop os.walk() from recursing into there.
    if '.git' in dirnames:
        # don't go into any .git directories.
        dirnames.remove('.git')
print "</ol></body></html>"
