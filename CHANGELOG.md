# Changelog

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
