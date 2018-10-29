# Development setup

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


Now clear the default and insert the needed test posts, as well as adding tags:

```
python setup_posts.py
```

## Requirements

(TODO: Explain the WordPress plugins that are needed to make this work)

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
