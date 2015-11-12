from subprocess import check_output
from os import system

(p4, p3, p2, p1) = check_output(["wp","post","list","--field=ID"]).split()

system("wp post term add %s post_tag tag1 && wp post term add %s post_tag tag1 && wp post term add %s post_tag tag2 && wp post term add %s post_tag tag1 && wp post term add %s category cat1" % (p1, p2, p2, p3, p1))
