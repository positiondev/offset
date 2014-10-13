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
