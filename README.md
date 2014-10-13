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
