=== Custom Post Types and Custom Fields creator - WCK ===

Contributors: cozmoslabs, reflectionmedia, madalin.ungureanu, sareiodata, adispiac
Donate link: http://www.cozmoslabs.com/wordpress-creation-kit/
Tags: custom fields, custom field, wordpress custom fields, custom post type, custom post types, post types, repeater fields, meta box, metabox, custom taxonomy, custom fields creator, post meta

Requires at least: 3.1
Tested up to: 4.7
Stable tag: 2.0.5
License: GPLv2 or later
License URI: http://www.gnu.org/licenses/gpl-2.0.html

A must have tool for creating custom fields, custom post types and taxonomies, fast and without any programming knowledge.


== Description ==

**Like this plugin?** Consider leaving a [5 star review](https://wordpress.org/support/view/plugin-reviews/wck-custom-fields-and-custom-post-types-creator?filter=5).

**WordPress Creation Kit** consists of three tools that can help you create and maintain custom post types, custom taxonomies and most importantly, custom fields and metaboxes for your posts, pages or CPT's.

**WCK Custom Fields Creator** offers an UI for setting up custom meta boxes with custom fields for your posts, pages or custom post types. Uses standard custom fields to store data.

**WCK Custom Post Type Creator** facilitates creating custom post types by providing an UI for most of the arguments of register_post_type() function.

**WCK Taxonomy Creator** allows you to easily create and edit custom taxonomies for WordPress without any programming knowledge. It provides an UI for most of the arguments of register_taxonomy() function.

[youtube http://www.youtube.com/watch?v=_ueYKlP_i7w]

= Custom Fields =
* Custom fields types: WYSIWYG editor, upload, text, textarea, select, checkbox, radio, number, HTML, time-picker, phone, currency select, color picker, heading
* Easy to create custom fields for any post type.
* Support for **Repeater Fields** and **Repeater Groups** of custom fields.
* Drag and Drop to sort the Repeater Fields.
* Support for all input custom fields: text, textarea, select, checkbox, radio.
* Image / File upload supported via the WordPress Media Uploader.
* Possibility to target only certain page-templates, target certain custom post types and even unique ID's.
* All data handling is done with Ajax
* Data is saved as postmeta

= Custom Post Types and Taxonomy =
* Create and edit Custom Post Types from the Admin UI
* Advanced Labeling Options
* Attach built in or custom taxonomies to post types
* Create and edit Custom Taxonomy from the Admin UI
* Attach the taxonomy to built in or custom post types

= WCK PRO =
  The [WCK PRO version](http://www.cozmoslabs.com/wck-custom-fields-custom-post-types-plugin/?utm_source=wp.org&utm_medium=pb-description-page&utm_campaign=WCKFree) offers:
  
* **Swift Templates** - Build your front-end templates directly from the WordPress admin UI, without writing any PHP code. Easily display registered custom post types, custom fields and taxonomies in your current theme.
* Front-end Posting - form builder for content creation and editing
* Options Page Creator - create option pages for your theme or your plugin
* More field types: Date-picker, Country Select, User Select, CPT Select
* Premium Email Support for your project
  
 [See complete list of PRO features](http://www.cozmoslabs.com/wck-custom-fields-custom-post-types-plugin/?utm_source=wp.org&utm_medium=pb-description-page&utm_campaign=WCKFree)

= Website =
http://www.cozmoslabs.com/wck-custom-fields-custom-post-types-plugin/

= Announcement Post and Video =
http://www.cozmoslabs.com/3747-wordpress-creation-kit-a-sparkling-new-custom-field-taxonomy-and-post-type-creator/

== Installation ==

1. Upload the wordpress-creation-kit folder to the '/wp-content/plugins/' directory
1. Activate the plugin through the 'Plugins' menu in WordPress
1. Then navigate to WCK => Custom Fields Creator tab and start creating your custom fields, or navigate to WCK => Post Type Creator tab and start creating your custom post types or navigate to WCK => Taxonomy Creator tab and start creating your taxonomies.

== Frequently Asked Questions ==

= How do I display my custom fields in the front end? =

Let's consider we have a meta box with the following arguments:
- Meta name: books
- Post Type: post
And we also have two fields defined:
- A text custom field with the Field Title: Book name
- And another text custom field with the Field Title: Author name

You will notice that slugs will automatically be created for the two text fields. For 'Book name' the slug will be 'book-name' and for 'Author name' the slug will be 'author-name'

Let's see what the code for displaying the meta box values in single.php of your theme would be:

`<?php $books = get_post_meta( $post->ID, 'books', true ); 
foreach( $books as $book){
	echo $book['book-name'] . '<br/>';
	echo $book['author-name'] . '<br/>';
}?>`

So as you can see the Meta Name 'books' is used as the $key parameter of the function get_post_meta() and the slugs of the text fields are used as keys for the resulting array. Basically CFC stores the entries as custom fields in a multidimensional array. In our case the array would be:

`<?php array( array( "book-name" => "The Hitchhiker's Guide To The Galaxy", "author-name" => "Douglas Adams" ),  array( "book-name" => "Ender's Game", "author-name" => "Orson Scott Card" ) );?>`

This is true even for single entries.

= How to query by post type in the front-end? =

You can create new queries to display posts from a specific post type. This is done via the 'post_type' parameter to a WP_Query.

Example:

`<?php $args = array( 'post_type' => 'product', 'posts_per_page' => 10 );
$loop = new WP_Query( $args );
while ( $loop->have_posts() ) : $loop->the_post();
	the_title();
	echo '<div class="entry-content">';
	the_content();
	echo '</div>';
endwhile;?>`

This simply loops through the latest 10 product posts and displays the title and content of them. 

= How do I list the taxonomies in the front-end? =

If you want to have a custom list in your theme, then you can pass the taxonomy name into the the_terms() function in the Loop, like so:

`<?php the_terms( $post->ID, 'people', 'People: ', ', ', ' ' ); ?>`

That displays the list of People attached to each post.

= How do I query by taxonomy in the front-end? =

Creating a taxonomy generally automatically creates a special query variable using WP_Query class, which we can use to retrieve posts based on. For example, to pull a list of posts that have 'Bob' as a 'person' taxomony in them, we will use:

`<?php $query = new WP_Query( array( 'person' => 'bob' ) ); ?>`

==Screenshots==
1. Creating custom post types and taxonomies
2. Creating custom fields and meta boxes
3. Custom Fields Creator - list of Meta boxes
4. Meta box with custom fields
5. Defined custom fields
6. Custom Fields Creator - Meta box arguments
7. Post Type Creator UI
8. Post Type Creator UI and listing
9. Taxonomy Creator UI
10. Taxonomy listing

== Changelog ==
= 2.0.5 =
* Changes to the unserialized fields: we can now handle fields from other sources
* Improvements to javascript speed in the admin interface

= 2.0.4 =
* Added sortable taxonomy admin column support for Taxonomy Creator
* Added show_in_quick_edit argument support for Taxonomy Creator

= 2.0.3 =
* Fixed some issues with the unserialized fields conversion
* Changed per batch variable from 100 to 30 to try to reduce timeouts on sites with a lot of entries

= 2.0.2 = 
* Fixed an issue with the unserialized conversion page when fields had same names

= 2.0.1 =
* Fixed issue with Custom Fields Creator when fields had the same name as the meta name

= 2.0.0 =
* We now save every custom field in it's own separate post meta with an editable meta_key
* UI improvements to repeater sortable table

= 1.3.3 =
* Added date format option for Datepicker Field
* Fixed notices when multiple single boxes were present and the first one had a required error
* New menu icon

= 1.3.2 =
* Added Number field type
* Removed notice regarding post thumbnail on certain themes
* New branding to match website

= 1.3.1 =
* Fixed preview draft not showing the correct custom fields in certain conditions
* Fixed a fatal error that was happening in certain conditions when adding a new Custom Fields Creator Meta Box

= 1.3.0 =
* Security Fixes

= 1.2.9 =
* Added Lables field in Custom Fields Creator next to Options for checkboxes, selects and radios

= 1.2.8 =
* Added Phone field type
* Added HTML field type
* Added Time Picker field type
* Added Default Text for textarea field instead of Default Value

= 1.2.7 =
* Added Heading field type
* Added Colorpicker field type
* Added Currency field type
* Added number of rows and readonly options to the textarea field
* Added error notice for users with a php version lower than 5.3.0 on the settings page

= 1.2.6 =
* Small change in saving single metaboxes
* Fixed a possible conflict with ACF Pro

= 1.2.5 =
* Minor compatibility tweeks for WordPress 4.5
* Added new filter for registration errors:'wck_registration_errors'

= 1.2.4 =
* We now load the translation files from the theme first if they exist in the folder:local_wck_lang
* Now in Custom Fields Creator the Options field for selects,radios and checkboxes is required so you can't create those field without any options
* Single forms now keep their values when form throws alert required message so you don't have to input the values again

= 1.2.3 =
* Minor security improvements
* Added filter for the 'rewrite' argument in the Custom Taxonomy Creator: 'wck_ctc_register_taxonomy_rewrite_arg'
* Added hooks in WCK_Page_Creator api to allow extra content before and after metaboxes: 'wck_page_creator_before_meta_boxes' and 'wck_page_creator_after_meta_boxes'

= 1.2.2 =
* Added additional labels to Post Type Creator and Taxonomy Creator
* We now check the post type name to not have spaces, capital letters or hyphens
* When changing a custom post type name the existing posts get ported as well

= 1.2.1 =
* When renaming a taxonomy we now make sure the terms get ported as well

= 1.2.0 =
* We now display error message when meta name contains uppercase letters
* We now display error message when taxonomy name contains uppercase letters or spaces
* Security improvements
* Fixed issues with post thumbnail and themes that added thumbnail support for specific post types in Custom Post Types Creator
* Removed notice when WPML was active in certain cases

= 1.1.9 =
* Fixed typo from 'Chose' to 'Choose'

= 1.1.8 =
* We now allow  Custom Post Types and Custom Taxonomies to be available via REST API by adding 'show_in_rest' attribute

= 1.1.7 =
* Select field can now display lables when outputting values
* Minor security improvements
* We no longer get .js errors when a Select field has no options
* Added global filter for a form element output
* Fixed typo in Meta Box Creator

= 1.1.6 =
* We now run the Custom Post Type and Custom Taxonomy register function sooner on the init hook
* Aligned "Help" wit "WCK" in contextual help red button
* Fixed some issues with translations

= 1.1.5 =
* Fixed major issue that prevented publishing new metaboxes (CFC)
* Added a footer message asking users to leave a review if they enjoyed WCK

= 1.1.4 =
* Changed the way Single Forms are displayed and saved.
* Added 'slug' parameter to API and we use it so we can translate labels
* Added filter for taxonomy term name
* Added support for search in media library for the upload field
* Add support for the link in the listed upload fields
* Add support for link on image/icon that points to attachement page in backend
* Changed the order of the CKEDITOR.config.allowedContent = true to be above the call to initialized the textarea
* Now metaboxes or pages don't appear for users that shouldn't

= 1.1.3 =
* Wysiwyg editor fields no longer strips html tags
* Changes to WCK deactivate function so it doesn't throw notices

= 1.1.2 =
* Added filters which we can use to modify the text on metabox buttons in the backend (ex. Add Entry)
* Fixed a bug that when we had unserialized fields enabled and we deleted some items in the metabox they still remained in the database
* Fixed some PHP Warnings and Notices

= 1.1.1 =
* Now we can add the same metabox from CFC on multiple ids
* Added filter for the arguments passed to the register_taonomy() funtion when creating a Custom Taxonomy. ( "wck_ctc_register_taxonomy_args" )
* Fixed bug that was executing  shortcodes inside escaped shortcodes [[shortcode]]
* Fixed problem in CPTC that was setting the 'publicly_queryable' argument as true

= 1.1.0 =
* Added filter for the arguments passed to the register_post_type() funtion when creating a Custom Post Type. ( "wck_cptc_register_post_type_args" )
* Fixed the missing datepicker css 404 error. 
* Removed notices  
* Fixed "Attach upload to post" option for the upload field.

= 1.0.9 =
* Replaced wysiwyg editor from tinymce to ckeditor to fix compatibility issues with WordPress 3.9

= 1.0.8 =
* Upload Field now uses the media manager added in WP 3.5
* Now we prevent "Meta Field" and "Field Title" to be named "content" or "action" in Custom Fields Creator to prevent conflicts with existing WordPress Fields
* Fixed bug in Custom Fields Creator that didn't display "0" values
* Added Spanish translation ( thanks to Andrew Kurtis for providing the translation files )


= 1.0.7 =
* Small compatibility tweaks for WordPress 3.8

= 1.0.6 =
* WCK menu now only appears for Administrator role only
* Minor fixes and improvements

= 1.0.5 =
* Fixed error from 1.0.4 require_once

= 1.0.4 =
* Added Custom Fields Api
* Added option to enable/disable WCK tools(CFC, CPTC, FEP...) that you want/don't want to use 
* Labels of required custom fields turn red when empty 
* Added in Custom Taxonomy Creator support for show_admin_column argument that allows automatic creation of taxonomy columns on associated post-types
* Improved visibility of WCK Help tab
* We no longer get js error when deregistering wysiwig init script

= 1.0.3 =
* Removed all notices and warnings from the code

= 1.0.2 =
* Fixed bug when arguments contained UTF8 characters ( like hebrew, chirilic... )
* Fixed Sortable field in Custom Fields Creator that wasn't clickable

= 1.0.1 =
* Fixed Menu Position argument for Custom Post Type Creator.
* Added filter for default_value.
* Fixed Template Select dropdown for Custom Fields Creator.
* Fixed a bug in Custom Fields Creator that prevented Options field in the process of creating custom fields from appearing.