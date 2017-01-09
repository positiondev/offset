from subprocess import check_output
from os import system

system ("wp post delete 1")

system("wp post create --post_title='A first post' --post_status=publish --post_date='2014-10-01 07:00:00' --post_content=\"This is the content\" --post_author=1")
system("wp post create --post_title='A second post' --post_status=publish --post_date='2014-10-02 07:00:00' --post_content=\"This is the second post content\" --post_author=1 ")
system("wp post create --post_title='A third post' --post_status=publish --post_date='2014-10-10 07:00:00' --post_content=\"This is the third post content\" --post_author=1 ")
system("wp post create --post_title='A fourth post' --post_status=publish --post_date='2014-10-15 07:00:00' --post_content=\"This is the fourth post content\" --post_author=1 ")
system("wp post create --post_title='A sports post' --post_status=publish --post_date='2014-10-20 07:00:00' --post_content=\"This is the sports post content\" --post_author=1")
system("wp option update permalink_structure '/%year%/%monthnum%/%postname%/' ")
system("wp plugin activate --all")
system("wp user update 1 --display_name='Ira Rubel' --first_name='Ira' --last_name='Rubel' ")
system("wp eval-file create_password.php")

(p5, p4, p3, p2, p1) = check_output(["wp","post","list","--field=ID"]).split()

system("wp post term add %s post_tag tag1" % p1)
system("wp post term add %s post_tag tag1" % p2)
system("wp post term add %s post_tag tag2" % p2)
system("wp post term add %s post_tag tag1" % p3)
system("wp post term add %s category cat1" % p1)
system("wp post term add %s departments sports" % p5)
