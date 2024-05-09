(uiop:define-package #:staticl-docs/making-a-static-site
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax))
(in-package #:staticl-docs/making-a-static-site)


(in-readtable pythonic-string-syntax)


(defsection @making-a-site (:title "Making a Static Site"
                            :ignore-words ("YAML"
                                           "IDE"
                                           "URL"
                                           "CSS"
                                           "HTML"
                                           "REPL"
                                           "JS"
                                           "TODO"
                                           "RSS"))
  """
In this tutorial, we will create our first static website using Static. The site will have pages "Home", "About the site", "Services" and a blog with posts. The blog posts will be collected in an RSS feed, and all pages will be presented in a file sitemap.xml . At the end, we will connect the Disqus comment system to the posts for feedback from visitors. Thanks to these additional features, the site will be more user-friendly and attractive to users.

## Initializing static for the site

For a quick start, `StatiCL` allows you to create the structure of a future site using a simple command. You can call it via Lisp REPL:

```lisp
CL-USER> (staticl:new-site "/tmp/my-site"
                           "My Lisp Site"
                           "https://about-lisp.org")
#P"/tmp/my-site/"
```

Or you can call the same command from the command line if you installed `StatiCL` using `Roswell` - just open a terminal and enter this command to create the initial site config:

```lisp
$ staticl --verbose new-site -o /tmp/my-site 'My Lisp Site' https://about-lisp.org
Site's content was written to: /tmp/my-site/
```

Let's look at what happened as a result:

```bash
$ tree -a /tmp/blah
/tmp/blah
├── .staticlrc
├── about.page
├── blog
│   ├── first.post
│   └── second.post
└── index.page
```

Here we see the site config `.staticlrc`, two regular pages: `index.page` and `about.page`, as well as two blog posts. And here are the settings generated for us:

```
(asdf:load-system "staticl/format/spinneret")
    
(site "My Lisp Site"
      :description "A site description."
      :url "https://about-lisp.org"
      :navigation (menu (item "Blog" "/blog/")
                        (item "About" "/about/"))
      :pipeline (list (load-content)
                      (prev-next-links)
                      (paginated-index :target-path "blog/")
                      (rss :target-path "blog/rss.xml")
                      (atom :target-path "blog/atom.xml")
                      (tags-index :target-path "tags/")
                      (sitemap))
      :theme "readable")
```

To create a site, the STATICL/USER-PACKAGE:SITE function is used, which passes the site title, its description, the `URL` for publication, links for navigation and a description of the content processing pipeline. The pipeline plays a key role in the formation of the site, since the final result depends on it. To load the `staticl/format/spinneret` dependency, you need to call the `asdf:load-system` function. This module adds support for the `Spinneret` format, an example of which can be found in the `about.post` file. Now let's look at the contents of the pipeline.

The first call in the pipeline is STATICL/USER-PACKAGE:LOAD-CONTENT function. It is responsible for downloading content from files with the `post` and `page` extensions. Next comes the STATICL/USER-PACKAGE:PREV-NEXT-LINKS block, it links the "post" type content together, due to which the `Previous` and `Next` links appear on the blog pages. The STATICL/USER-PACKAGE:PAGINATED-INDEX block is responsible for creating pages on which blog posts are grouped into `N` pieces. Here, in the example, the number of posts per page is not specified, but the STATICL/USER-PACKAGE:PAGINATED-INDEX function can accept the `:PAGE-SIZE` argument, as well as some other arguments. The great news is that when you edit such a config in an editor that supports working with Common Lisp, the IDE will tell you what the signature of each function is and what parameters it can have. Try to do this with a static site generator that uses the YAML format for configuration!

Next we have two calls of STATICL/USER-PACKAGE:RSS function and STATICL/USER-PACKAGE:ATOM function. They are similar in that they give a feed output from the latest blog posts. Well, the STATICL/USER-PACKAGE:SITEMAP function generates a file `sitemap.xml ` and includes all the content that was created in the previous stages.

## Generating a static website

Now that we have some content, let's make an HTML website out of it. To do this, run the following command in REPL:

```lisp
CL-USER> (staticl:generate :root-dir "/tmp/my-site"
                           :stage-dir "/tmp/result")
#P"/tmp/result/"
```

Or on the command line:

```bash
$ staticl -v generate -s /tmp/my-site -o /tmp/result
Site was written to: /tmp/result/
```

This command created several HTML files in the `/tmp/result/` directory, and also put the necessary CSS and JS files there:

```bash
$ tree /tmp/result
/tmp/result
├── about
│   └── index.html
├── blog
│   ├── atom.xml
│   ├── first
│   │   └── index.html
│   ├── index.html
│   ├── rss.xml
│   └── second
│       └── index.html
├── css
│   ├── bootstrap.min.css
│   └── custom.css
├── img
│   ├── cc-by-sa.png
│   ├── glyphicons-halflings-white.png
│   ├── glyphicons-halflings.png
│   └── staticl-logo-small.webp
├── index.html
├── js
│   └── bootstrap.min.js
├── sitemap.xml
└── tags
    ├── bar
    │   └── index.html
    ├── example
    │   └── index.html
    └── foo
        └── index.html
```

Now we need to somehow open the site in the browser. If you just open the file in the browser `index.html `, then he will load it without styles, and other things may not work either, since the page opened in this way will not have a domain. To fully open the site, we need to launch a web server. Previously, I would have done this using `python`. This is how you can distribute static from a local directory using `python` and its `http.server` module:

```bash
$ cd /tmp/result
$ python3 -m http.server
Serving HTTP on :: port 8000 (http://[::]:8000/) ...
```

However, we can do better – a web server is already built into `StatiCL`. Moreover, it can track changes in your site's files and update the site pages opened in the browser. To start this local web server, use the following command in REPL:

```lisp
CL-USER> (staticl:serve :root-dir #P"/tmp/my-site"
                        :stage-dir #P"/tmp/result")
 [21:35:55] staticl/utils utils.lisp (top level form find-class-by-name) -
  Searching class by name STATICL/UTILS::NAME: "closure-template"
  [21:35:55] staticl/server server.lisp (top level form serve) -
  Starting Clack server to serve site from /tmp/result/
Hunchentoot server is started.
Listening on localhost:8001.
```

Or from the command line:

```bash
$ staticl serve -s /tmp/my-site -o /tmp/result
  [21:39:48] staticl/server server.lisp (top level form serve) -
  Starting Clack server to serve site from /tmp/result/
Hunchentoot server is started.
Listening on localhost:8001.
```

As you can see, the `serve` command is very similar to `generate`, and accepts similar parameters, because it needs not only to distribute static, but also to generate it. In addition, the `serve` command will try to open the site in the browser. Look at it:

![Image](https://storage.yandexcloud.net/40ants-public/staticl/docs/tutorial/site-example.webp)

If you click on the `Blog` link, a page with a list of posts will open. This part of the pipeline is responsible for its generation: `(paginated-index :target-path #P"blog/")'. The page looks like this:

![Image](https://storage.yandexcloud.net/40ants-public/staticl/docs/tutorial/first-page.webp)

On the index page, `StatiCL` displays only the introduction if the source file of the post separates it from the main part with the line `<!--more-->`.

If you click on one of the posts, it will open in its entirety:

![Image](https://storage.yandexcloud.net/40ants-public/staticl/docs/tutorial/second-post.webp)

Pay attention to the "Next" link in the lower right corner. All blog posts are linked to each other and this is also done thanks to a separate pipeline block: `(prev-next-links)'. This pipeline block adds metadata to each post, which is then available in the template. If you remove it, the links will disappear from the pages.

## How to add comments using Disqus


Like any content creator, you will definitely want to communicate with your readers. The easiest way to do this is to connect dynamic comments to our static blog. There are many services that provide such comments – for example, Disqus, Commento, Remark42, etc..

At the moment, `StatiCL` only supports Disqus, but it's easy to write a plugin for any other comment system.

Let's add comments to our website! All you need to do is register with Disqus, get a short site name, and add another pipeline block to the config. Let's say I registered the site name `example` in Disqus, and then I need to add the `(disqus "example") block to our pipeline:


```lisp
:pipeline (list (load-content)
                (prev-next-links)
                (paginated-index :target-path #P"blog/")
                (rss :target-path "blog/rss.xml")
                (atom :target-path "blog/atom.xml")
                (tags-index :target-path "tags/")
                (sitemap)
                (disqus "example"))
```

This is how the post page with comments will look like:

![Image](https://storage.yandexcloud.net/40ants-public/staticl/docs/tutorial/post-with-comments.webp)

## Content filtering

Now let's assume that we don't want to connect comments to all posts, but only to those where there is no `no-comments` tag. To do this, we can filter the content using the STATICL/USER-PACKAGE:FILTER block. This block accepts a number of parameters, as well as other pipeline blocks, which will receive only those content elements that have passed the filter. This is how the filter will look like, which will apply STATICL/USER-PACKAGE:DISQUS function only to content that does not have the `no-comments` tag:

```lisp
(filter (:tags "no-comments"
         :invert t)
        (disqus "example"))
```

Now add the `no-comments` tag to the `blog/first.post` file and make sure that comments are not displayed on the page of this post. They are on the page of the second post.

In the following tutorials, we will figure out how to create themes for your static website and learn how to add the necessary functionality using plugins.
"""
  )
