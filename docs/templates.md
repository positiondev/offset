# Offset templates

Offset templates are written in [Larceny](https://github.com/positiondev/larceny). You can learn how to write
templates from the Larceny [documentation](https://github.com/positiondev/larceny/blob/master/docs/templates.md).

Offset provides "substitutions" you can use in your Larceny templates.

 * [Posts](#posts)
   * [Listing posts](#wpposts)
   * [Displaying a single post](#wppostbypermalink)
 * [Pages](#pages)
 * [Dates](#dates)
 * [Arbitrary custom data](#arbitrary-custom-data)


## Posts

### `<wpPosts>`

Use this substitution to pull a list of posts.

#### Tags
You can use HTML attributes to filter the posts by any taxonomy. This includes
tags and categories which are usually enabled by default in Wordpress and also
any custom taxonomies you've configured. Separate multiple taxonomy members by
commas and use `+` and `-` to include exclude multiple terms.

##### Example

* Wayland Template

	```
	<wpPosts tags="+fun,-boring" categories="playgrounds">
	  <h3><wpTitle \></h3>
	</wpPosts>
	```

* Rendered HTML
  
  Assuming there are 3 posts that have the tag `fun`, do not have the tag `boring`, and have the category `playgrounds`.

   ```
   <h3>You won't believe these fun playgrounds!</h3>
   <h3>The best slides in America</h3>
   <h3>10 safe-but-fun swingsets for toddlers</h3>
   ```

#### Paging 

You use the "num" attribute to specify the number of posts per page. Defaults to 20.

The "page" is an integer to specify the current page (1 is the first one) worth of posts.

##### Example

```
<wpPosts page="3" num="10">
  <wpTitle />
</wpPosts>
```

This will skip ahead 20 posts and display the next 10.

#### Limiting the number of posts listed

The "limit" attribute restricts the number of posts that come back in the
current page. Note that if you haven't set the page, changing this is equivalent
to changing "num". If you _have_ set the page, then "limit" limits how many posts
of that page are returned. Defaults to 20.

##### Example 

```
<wpPosts limit="10"">
  <wpTitle />
</wpPosts>
```

This will display only 10 posts.

#### Offset 

The "offset" attribute affects how many posts are skipped in the current page.
If you don't set the page, then this is just the number of posts that are
skipped before the first posts are returned. If you have set the page, then the
posts will skip to the current page, then an additional number of posts will be
skipped using the "offset".

##### Example 

```
<wpPosts page="3" num="10" offset="5">
  <wpTitle />
</wpPosts>
```

This will skip 20 posts (to get to the third page), then skip five more posts
for the offset, then display the next 10 posts.

### `<wp>`

Wrap your templates in the `wp` tag so that multiple post lookups can happen at
the same time -- this will speed up rendering.

#### Example

This makes three calls to the WordPress API:

```
<wpPosts tag="featured"></wpPosts>

<wpPosts tag="secondary"></wpPosts>

<wpPosts limit="10"></wpPosts>
```

This makes only one call to the API, so is much faster:

```
<wp>
  <wpPosts tag="featured"></wpPosts>

  <wpPosts tag="secondary"></wpPosts>

  <wpPosts limit="10"></wpPosts>
</wp>

```

### `<wpNoPostDuplicates />`

Insert this tag before a bunch of `wpPosts` tags to make sure that no duplicates
are shown on the rendered page. You can use it so that multiple post listings 
don't show the same posts.

#### Example

```
<wp>
  <div id="featured">
    <wpPosts tag="featured"><wpTitle /></wpPosts>
  </div>

  <div id="secondary">
    <wpPosts limit="10"><wpTitle /></wpPosts>
  </div>
</wp>
```

If there are three posts, "Post 1", "Post 2", and "Post 3", and "Post 1" is a
featured post, then rendering this template will result in:

```
<div id="featured">
  Post 1
</div>

<div id="secondary">
  Post 1
  Post 2
  Post 3
</div>  
```

Post 1 appears twice! But, if you add `<wpNoPostDuplicates />`:

```
<wp>
  <wpNoPostDuplicates />
  <div id="featured">
    <wpPosts tag="featured"><wpTitle /></wpPosts>
  </div>

  <div id="secondary">
    <wpPosts limit="10"><wpTitle /></wpPosts>
  </div>
</wp>
```

This results in:

```
<div id="featured">
  Post 1
</div>

<div id="secondary">
  Post 2
  Post 3
</div>  
```

### `<wpPostByPermalink>`

This tag will retrieve the post that corresponds to the current request URL. 

#### Example

Say this post exists in WordPress:

```
Title: A great post
Slug: a-great-post

Body: Wow, this is an excellent post.

```

A visitor goes to the URL: `https://myblog.com/2016/10/great-post`. The template the server uses is `post.tpl` and it looks like this:

```
<wpPostByPermalink>
  <h3><wpTitle /><h3>
  
  <main>
    <wpContent />
  </main>
</wpPostByPermalink>
```

That will result in this rendered HTML:

```
<h3>A great post</h3>

<main>
  Wow, this is an excellent post.
</main>
```

## Pages

### `<wpPage>`

Use this substitution to pull a specific page's content into your template.

```
<wpPage name="slug-of-wordpress-page" />
```

## Dates

### `<wpDate>`

Inside `<wpPosts>`, you can use these substitutions to display dates. 

#### Example

```
<wpPosts>
  <h3><wpTitle /></h3>

  <p>Published: <wpDate><wpMonth />/<wpDay />/<wpYear /></wpDate></p>
</wpPosts>  
```

renders to:

```
<h3>A Post</h3>

<p>Published: 10/20/2017</p>
```

### `<wpCustomDate>`

Use this substitution for more control over how WordPress renders dates.

##### Example 1

* Wordpress Date

```
{
  ...
  "post_date": "2016-11-10 09:23:44",
  ...
}
```

* Wayland Template

We are assuming that `wpPostDate` is a field that's made available by an earlier
tag like `wpCustom` or `wpPosts` etc. If the date format is different from
WordPress's default, you can supply a 
[format string](https://hackage.haskell.org/package/time-1.7.0.1/docs/Data-Time-Format.html#v:formatTime) 
with the "wp_Format" attribute.

```
<wpCustomDate date="${wpPostDate}" wp_format=""%Y-%m-%d %H:%M:%S">
  <wpDay />~<wpMonth />~<wpYear />
</wpCustomDate>
```

* Rendered HTML

```
10~11~2016
```

##### Example 2

You can also supply a format for the day, month, year, or entire date by using
the format string in the "format" attribute of the `wpDay`, `wpMonth`, `wpYear`,
or `wpDate` tags.

* Wayland Template 

```
<wpCustomDate date="${wpPostDate}">
  <wpDate format="%B %-d, %0Y"/>
</wpCustomDate>
```

* Rendered HTML

```
October 11, 2016
```

## Arbitrary Custom Data

### `<wpCustom>`

Use this substitution to pull custom data from the Wordpress JSON API into your Wayland
templates.

You will need to know the structure of the JSON data that Wordpress uses. A
great tool for exploring this is [Postman](https://www.getpostman.com/).

Once you know what JSON data you'd like to use from Wordpress, you can use
`wpCustom` to query it. You'll need to know the structure of the fields that are
returned and the endpoint to query. Here are some examples of JSON and the
corresponding `wpCustom` email.

#### Example 1

* JSON
  * **GET** `http://www.your-wordpress-url.com/wp-json/wp/v2/posts`

	```
	[
		{
			"title": "A wonderful post",
			"author": {
				 	     "first_name": "Leonard",
				 	     "last_name": "Nimoy"
			          }
			"content": "Star Trek is just great!"
		},
		{
			"title": "An unbiased post",
			"author": {
				 	     "first_name": "John",
				 	     "last_name": "Boyega"
			          }
			"content": "Star Wars is better than Star Trek"
		}
	]  
	```
* Wayland Template

  ```
  <wpCustom endpoint="/wp/v2/posts">
    <p>
      <h2><wpTitle /></h2>
      <h3>Author: <wpAuthor><wpFirstName /> <wpLastName /></wpAuthor></h2>
      <p>
        <wpContent />
      </p>
    </p>
  </wpCustom>
  ```
* Rendered HTML

  ```
  <p>
  	<h2>A wonderful post</p>
  	<h3>Author: Leonard Nimoy</h3>
  	<p>
  		Star Trek is just great!
  	</p>
  </p>
    <p>
  	<h2>An unbiased post</p>
  	<h3>Author: John Boeyga</h3>
  	<p>
  		Star Was is better than Star Trek!
  	</p>
  </p>
  
  ```

#### Example 2

* JSON
  * **GET** `http://www.your-wordpress-url.com/wp-json/jacobin/featured-content/editors-picks`
  
	``` 
	[
	  {
	    "ID": 7335,
	    "post_date": "2013-04-26 10:11:52",
	    "post_title": "\"Guns Not Bombs\"",
	    "authors": [
	      {
	        "id": 37837
	      }
	    ]
	  },
	  {
	    "ID": 6392,
	    "post_date": "2013-03-04 13:59:52",
	    "post_title": "\"Like\" Feminism",
	    "authors": []
	  },
	  {
	    "ID": 17178,
	    "post_date": "2014-12-18 08:16:35",
	    "post_title": "\"Remember Me as a Revolutionary Communist\"",
	    "authors": [
	      {
	        "id": 37733
	      }
	    ]
	  },
	  {
	    "ID": 5460,
	    "post_date": "2012-12-13 03:45:43",
	    "post_title": "\"The Shift From the Unions\"",
	    "authors": []
	  },
	  {
	    "ID": 18481,
	    "post_author": "7710",
	    "post_date": "2015-01-05 12:13:36",
	    "post_title": "\"There Was No Question of Anybody Walking Out\"",
	    },
	    "authors": [
	      {
	        "id": 37422,
	      }
	    ]
	  }
	]
	```
  * **GET** `http://www.your-wordpress-url.com/wp-json/wp/v2/posts/{id}`

	```
	{
	  "id": 234,
	  "slug": "article-slug",
	  ...,
	  "related_articles": [
	  {
	    "id": 45279,
	    "title": {
	      "rendered": "Related Article Title"
	    },
	    "excerpt": {
	      "rendered": "Excerpt of article"
	    }
	  },
	  {
	    "id": 3483,
	    "title": {
	      "rendered": "Another Related Article Title"
	    },
	    "excerpt": {
	      "rendered": "Excerpt of another article"
	    }
	  },
	}
	```
  
* Wayland Template

  ```
  <h2>Editor's Picks</h2>

  <ul>
    <wpCustom endpoint="jacobin/featured-content/editors-picks">
      <li>
        <p><wpPostDate /></p>
        <p><wpPostTitle /></p>
        <p>
          <wpCustom endpoint="wp/v2/posts/${wpID}">
            <wpRelatedArticles>
              <wpTitle>
                <wpRendered />
              </wpTitle>
              <wpExcerpt>
                <wpRendered />
              </wpExcerpt>
            </wpRelatedArticles>
          </wpCustom>
        </p>
      </li>
    </wpCustom>
  </ul>
  ```
* Rendered HTML

  ```
    <h2>Editor's Picks</h2>

	<ul>
	      <li>
	        <p>2013-04-26 10:11:52</p>
	        <p>"Guns Not Bombs"</p>
	        <p>
				Related Article Title
				Excerpt of article			
				Another Related Article Title
				Excerpt of another article
	        </p>
	      </li>
	    
	      <li>
	        <p>2015-01-05 12:13:36</p>
	        <p>""Like" Feminism"</p>
	        <p>
				Related Article Title
				Excerpt of article			
				Another Related Article Title
				Excerpt of another article
	        </p>
	      </li>
	      
	      etc..
	      etc..
	</ul>  
  ```  


