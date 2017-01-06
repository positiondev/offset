## Note: We use version 1 of the api (1.2.3 specifically), until they actually support everything in version 2.

For testing, you should install the `wp-cli`, which allows us to have
a development version of a wordpress server running.

You need to have php installed to do this.

```
curl -O https://raw.githubusercontent.com/wp-cli/builds/gh-pages/phar/wp-cli.phar
```

Move it to somewhere in your PATH and make it executable.

Next, create a database and user with access to it. The names should
be the following, or else you'll have to change the config file within
the `wp` directory.

```
$ mysql -u root -p
[enter password]
mysql> CREATE DATABASE offset_test;
mysql> CREATE USER 'offset'@'localhost' IDENTIFIED by '111';
mysql> GRANT ALL PRIVILEGES ON *.* TO 'offset'@'localhost' WITH GRANT OPTION;
```

Now change into the `wp` directory and finish the install and start the server with:

```
$ wp core config --dbname=offset_test --dbuser=offset --dbpass=111
$ wp core install --admin_user=offset --admin_password=111 --url=localhost --title="Offset Test" --admin_email="dbp@positiondev.com"
$ wp server --port=5555
```

Set the permalink structure, activate the plugins, set the name of the admin, and set the application password:

```
wp option update permalink_structure '/%year%/%monthnum%/%postname%/'
wp plugin activate --all
wp user update 1 --display_name="Ira Rubel" --first_name="Ira" --last_name="Rubel"
wp eval-file create_password.php
```

To test that it is working,
run the following command (requires the `jq` utility, which you can
install on macs with `brew install jq`):

```
curl http://localhost:5555/wp-json/ | jq
```

Which should print out a bunch of json.


Now clear the default and insert the needed test posts:

```
wp post delete 1
wp post delete 2
wp post create --post_title='A first post' --post_status=publish --post_date='2014-10-01 07:00:00' --post_content="This is the content" --post_author=1
wp post create --post_title='A second post' --post_status=publish --post_date='2014-10-02 07:00:00' --post_content="This is the second post content" --post_author=1
wp post create --post_title='A third post' --post_status=publish --post_date='2014-10-10 07:00:00' --post_content="This is the third post content" --post_author=1
wp post create --post_title='A fourth post' --post_status=publish --post_date='2014-10-15 07:00:00' --post_content="This is the fourth post content" --post_author=1
wp post create --post_type=page --post_title='A first page' --post_status=publish --post_date='2014-10-15 07:00:00' --post_content="This is the first page content" --post_author=1
```

Now set the tags and categories:

```
python add_tags.py
```



**NOTE(dbp 2015-11-08):**

**The following may be out of date.**

## Requirements

For this to work, you need to have running, on your wordpress server,
the WP-API plugin (called JSON REST API in the plugin search; at
github.com/WP-API/WP-API ) and their Basic-Auth plugin (not sure if it's
on the plugin database, but at github.com/WP-API/Basic-Auth).

And, since certain, needed, filter options are only available to
logged in users, we use HTTP Basic Authentication for all requests. So
you need both to have credentials in the snaplet config file (username and
password respectively), and you should REALLY only use SSL to connect to
the wordpress server (as otherwise you'll be throwing credentials to
your wordpress site out into the world).

## Tests

Some tests are hitting a live site, jacobinmag.com (which is the
reason why this snaplet was developed). Even though all the data that
is being exposed through the snaplet is public, some query options
(like offset) are only available to logged in users, so we use HTTP
Basic authentication (and thus, should only be run over SSL) for all
connections. But, this means that those tests aren't going to work (as
you need valid username / password settings in a test.cfg file in the
top level of this repo). Perhaps at some point I'll figure out a
reasonably self contained way to get a local wordpress system running
(hmmm... probably later rather than sooner), and through that could
have live tests that aren't limited in this way.


## Documentation

`<wpPosts>` - This tag accepts the following attributes:

`num` - should be an integer. the number of posts per page. Defaults to 20.

`page` - should be an integer. This is the current page (`1` is the first one) worth of posts.

`limit` - should be an integer, and this restricts the number of posts
that come back in the current page. Note that if you haven't set
`page`, changing this is equivalent to changing `num`. If you have set
`page`, then the first `page` full pages (each of size `num`) are
skipped, and then the first `limit` posts are returned. Defaults to
20.

`offset` - should be an integer, and this affects how many posts are
skipped in the current page. If you don't set the `page`, then this is
just the number of posts that are skipped before the first `limit`
posts are returned, but if you have set `page`, the first `page` full
pages of posts (each of size `num`) will be skipped, then an
additional `offset` posts will be skipped, and finally, `limit` posts
will be returned.


`<wpPostByPermalink>` - This tag expects to have the url be `/YYYY/MM/SLUG`, and finds
the post accordingly.

`<wpNoPostDuplicates/>` - This is a side-effect only tag, that causes, from this point
in the page forward, no duplicate posts to be returned from `<wpPosts/>`. This can make
certain layouts easier to express, rather than figuring out exactly how to combine the
various numeric arguments to avoid duplication.
