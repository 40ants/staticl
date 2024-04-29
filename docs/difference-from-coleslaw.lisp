
(defvar *templates-difference*
  "
Staticl config is constructed from lisp function calls and you can benefit from IDE's code completion.

The first important variable `config` was renamed to `site`. Secondly, variables `post` and `index` were renamed to `content`. That is it - content of any page, be it a post or a generic page is available as `content` variable inside the template.

## Changes in navigation

The `site.sitenav` list was renamed to `site.navigation.items`. Also items inside the navigation now have a `title` slot instead of `name` and also navigation menu can contain submenus, but this requires a special support from the theme. If an item has a slot `item`, then it is a submenu. Themes ported from the Coleslaw do not support this submenues.

## Index pages

For index pages a list of items was also moved and now instead of `index.content` a `content.items` should be used.

### Index objects

For objects in `content.items` attribute `obj.text` was renamed to `obj.excerpt`. It is a HTML, so `noAutoescape` filter should be applied (as you did in Coleslaw themes too).

### Index by tag

Coleslaw always rendered pages where posts are grouped by tags. But with Staticl you have to include TAGS-INDEX function call into the site's pipeline. Without this step, tag objects will not have a \"url\" slot and template might be ready to render tags without the URL.

## Other field renames

- `pubdate -> site.pubdate`
- `obj.date -> obj.created_at`
- `post.date -> content.created_at`
- `post.text -> content.html`
- `tags -> content.tags`
- `prev -> content.prev`
- `next -> content.next`


## Working with dates

For templates base on Closure Template, StatiCL defines these filters:

* date - formats the a timestamp in this as YYYY-MM-DD
* datetime - formats a timestamp as YYYY-MM-DD HH:MM

To define additional filters, inherit your template class from CLOSURE-TEMPLATE and define a method for REGISTER-USER-FILTERS generic-function.

## URLs

Templates in Coleslaw used {$site.domain}/ as a prefix to each URL. With StatiCL all URLs are formatted in advance before
variables are passed to the template and you don't have to concatenate string to get a proper URL in a template.

Also, note, that instead of making 1.html and symlinking to it from index.html for post indices, StatiCL just generates first page as index.html and other pages as 2.html, 3.html, etc..


## Pages layout

Coleslaw uses a subfolder `pages/` to keep content of all site pages and put them to the root of the site. Staticl does not implement this logic - it generates an output page with the same path as an original file. For example, if previousl with Coleslaw you put `/pages/about.post` to get `/about.html` page, with Staticl you write `/about.post` source file to generate `/about.html` or `/about/index.html` (depending on clean urls setting).


## Additional formats

* (ql:quickload :staticl/format/spinneret)

")
