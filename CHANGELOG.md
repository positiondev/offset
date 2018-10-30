# Changelog

## October 29 2018
 * Add Splice for a version of `wpPosts` called `wpPostsAggregate`
 * `wpPostsAggregate` will allow access to some information from the headers, like how many posts and pages of posts there are.
 * Unlike `wpPosts`, the posts are wrapped in `wpPostsItem`. 
 * There's also a separate `wpPostsMeta` that currently has the following splices:
    * `wpTotalPages` - displays how many pages of results are there from this query
    * `wpHasMorePages` - shows child markup if there are more pages (uses the post query from `wpPostsAggregate`'s attributes)
    * `wpNoMorePages` - shows child markup if there are no more pages
 * Eventually `wpPostsMeta` may give access to all the headers, but I have implemented that.
 * Add `wpCustomAggregrate` which works just same as `wpPostsAggregate`.

## Changelog bankruptcy

## August 22 2017
 * Feeds need to specify if they're using the default author field (an id) or inline guest authors fields.

## July 20 2017
 * Changed Splice creation so that "false" fields don't render (this is because WP uses "text or false" as a sort of Maybe type, as far as I can tell -- if the field is entered, it's text, if not, it's "false".)

## Changelog bankruptcy

## February 27 2017
 * Add "PN" and "PM" for custom-parsed fields with objects and lists

## February 9 2017

 * Add "format" attribute to `wpDate` in `wpPosts`
 * Change "format_out" to just "format" in `wpCustomDate`

## Old changelog

* 0.1.1.2 - Add missing Misc module for test suite, expose modules for test suite.
* 0.1.1.1 - Update for GHC 7.10.
* 0.1.1.0 - Initial release.
