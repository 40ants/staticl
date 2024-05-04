<a id="x-28STATICL-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# staticl - Flexible static site generator.

<a id="staticl-asdf-system-details"></a>

## STATICL ASDF System Details

* Version: 0.1.0
* Description: Flexible and customizable static site generator with a lot of plugins!
* Licence: Unlicense
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Homepage: [https://40ants.com/staticl/][0cf8]
* Bug tracker: [https://github.com/40ants/staticl/issues][5825]
* Source control: [GIT][2594]
* Depends on: [3bmd][cc3e], [3bmd-ext-code-blocks][d94a], [alexandria][8236], [cl-fad][1059], [cl-ppcre][49b9], [cl-sitemaps][dfc0], [closer-mop][61a4], [closure-template][2700], [feeder][0b13], [fuzzy-dates][ed60], [local-time][46a1], [log4cl][7f8b], [quri][2103], [serapeum][c41d], [str][ef7f], [utilities.print-items][52a0]

[![](https://github-actions.40ants.com/40ants/staticl/matrix.svg?only=ci.run-tests)][ba40]

![](http://quickdocs.org/badge/staticl.svg)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :staticl)
```
<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

`StatiCL` is a static site generator. `StatiCL` as a modular architecture and is suitable for any kind of a site
be it a blog or a site with pages not included in the feeds. This project was created to overcome limitations of the Coleslaw.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40PROCESSING-PIPELINE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Processing Pipeline

First of all, a `STATICL:SITE` object is created and filled with options from `.staticlrc` file.

Then [`staticl:stage`][c5b7] function calls `STATICL/CONTENT:READ-CONTENT` generic-function which returns a list
of [`staticl/content:content`][a8cd] objects. On the next stage, initial list of content objects are passed to a
generic-function [`staticl/content:preprocess`][d70a] called with a preprocessor returned by a
generic-function `STATICL/PLUGINS:SITE-PLUGINS` as a first argument. Each preprocessor may
return additional [`staticl/content:content`][a8cd] objects such as index pages, `RSS` or `ATOM` feeds, sitemaps etc.

When all content was preprocessed, a generic-function [`staticl/content:write-content`][c4c2] is called
on each [`staticl/content:content`][a8cd] object and a `SITE` object. Content objects are having a format slot,
so internally [`staticl/content:write-content`][c4c2] generic-function creates an object of corresponding format class
or takes it from the cache and then calls [`staticl/content:write-content-to-stream`][96e3] using this format object.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL

<a id="x-28-23A-28-287-29-20BASE-CHAR-20-2E-20-22STATICL-22-29-20PACKAGE-29"></a>

#### [package](592c) `staticl`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-3ASTAGE-20FUNCTION-29"></a>

##### [function](c8d0) `staticl:stage` &KEY (ROOT-DIR \*DEFAULT-PATHNAME-DEFAULTS\*) (STAGE-DIR (MERGE-PATHNAMES (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "stage"))
                 (UIOP/PATHNAME:ENSURE-DIRECTORY-PATHNAME ROOT-DIR)))

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCLEAN-URLS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/CLEAN-URLS

<a id="x-28-23A-28-2818-29-20BASE-CHAR-20-2E-20-22STATICL-2FCLEAN-URLS-22-29-20PACKAGE-29"></a>

#### [package](0b1b) `staticl/clean-urls`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCLEAN-URLS-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28STATICL-2FCLEAN-URLS-3ATRANSFORM-FILENAME-20GENERIC-FUNCTION-29"></a>

##### [generic-function](5ca0) `staticl/clean-urls:transform-filename` site filename

Converts the pathname to the form that should be used to write content to the disk.

If the site has the clean-urls setting enabled, then the filename like some/page.html will be converted
to some/page/index.html. If clean-urls is not enabled, the pathname will remain unchanged.

<a id="x-28STATICL-2FCLEAN-URLS-3ATRANSFORM-URL-20GENERIC-FUNCTION-29"></a>

##### [generic-function](1e0e) `staticl/clean-urls:transform-url` site url

Converts the `URL` to the form that should be used on the site.

If the site has the clean-urls setting enabled, then the `URL` like /some/page.html will be converted
to /some/page/. If clean-urls is not enabled, the `URL` will remain unchanged.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/CONTENT

<a id="x-28-23A-28-2815-29-20BASE-CHAR-20-2E-20-22STATICL-2FCONTENT-22-29-20PACKAGE-29"></a>

#### [package](72a2) `staticl/content`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCONTENT-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-24CONTENT-FROM-FILE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CONTENT-FROM-FILE

<a id="x-28STATICL-2FCONTENT-3ACONTENT-FROM-FILE-20CLASS-29"></a>

###### [class](f1f2) `staticl/content:content-from-file` (content-with-title-mixin content-with-tags-mixin content)

**Readers**

<a id="x-28STATICL-2FCONTENT-3ACONTENT-CREATED-AT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-3ACONTENT-FROM-FILE-29-29"></a>

###### [reader](c43f) `staticl/content:content-created-at` (content-from-file) (:created-at)

<a id="x-28STATICL-2FCONTENT-3ACONTENT-EXCERPT-SEPARATOR-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-3ACONTENT-FROM-FILE-29-29"></a>

###### [reader](971f) `staticl/content:content-excerpt-separator` (content-from-file) (:excerpt)

<a id="x-28STATICL-2FCONTENT-3ACONTENT-FILE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-3ACONTENT-FROM-FILE-29-29"></a>

###### [reader](a0c4) `staticl/content:content-file` (content-from-file) (:file)

Absolute pathname to the file read from disk or `NIL` for content objects which have no source file, like `RSS` feeds.

<a id="x-28STATICL-2FCONTENT-3ACONTENT-FORMAT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-3ACONTENT-FROM-FILE-29-29"></a>

###### [reader](5090) `staticl/content:content-format` (content-from-file) (:format)

<a id="x-28STATICL-2FCONTENT-3ACONTENT-TEMPLATE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-3ACONTENT-FROM-FILE-29-29"></a>

###### [reader](34b8) `staticl/content:content-template` (content-from-file) (:template)

<a id="x-28STATICL-2FCONTENT-3ACONTENT-TEXT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-3ACONTENT-FROM-FILE-29-29"></a>

###### [reader](bdeb) `staticl/content:content-text` (content-from-file) (:text)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-24CONTENT-TYPE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CONTENT-TYPE

<a id="x-28STATICL-2FCONTENT-3ACONTENT-TYPE-20CLASS-29"></a>

###### [class](4c56) `staticl/content:content-type` ()

**Readers**

<a id="x-28STATICL-2FCONTENT-3ACONTENT-CLASS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-3ACONTENT-TYPE-29-29"></a>

###### [reader](94f6) `staticl/content:content-class` (content-type) (:content-class)

<a id="x-28STATICL-2FCONTENT-3ACONTENT-FILE-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-3ACONTENT-TYPE-29-29"></a>

###### [reader](8e61) `staticl/content:content-file-type` (content-type) (:type)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-24CONTENT-WITH-TAGS-MIXIN-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CONTENT-WITH-TAGS-MIXIN

<a id="x-28STATICL-2FCONTENT-3ACONTENT-WITH-TAGS-MIXIN-20CLASS-29"></a>

###### [class](1a5e) `staticl/content:content-with-tags-mixin` ()

**Readers**

<a id="x-28STATICL-2FCONTENT-3ACONTENT-TAGS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-3ACONTENT-WITH-TAGS-MIXIN-29-29"></a>

###### [reader](0a5a) `staticl/content:content-tags` (content-with-tags-mixin) (:tags)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-24CONTENT-WITH-TITLE-MIXIN-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CONTENT-WITH-TITLE-MIXIN

<a id="x-28STATICL-2FCONTENT-3ACONTENT-WITH-TITLE-MIXIN-20CLASS-29"></a>

###### [class](aa5f) `staticl/content:content-with-title-mixin` ()

**Readers**

<a id="x-28STATICL-2FCONTENT-3ACONTENT-TITLE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-3ACONTENT-WITH-TITLE-MIXIN-29-29"></a>

###### [reader](310a) `staticl/content:content-title` (content-with-title-mixin) (:title)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-24CONTENT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### CONTENT

<a id="x-28STATICL-2FCONTENT-3ACONTENT-20CLASS-29"></a>

###### [class](cf13) `staticl/content:content` (print-items-mixin)

**Readers**

<a id="x-28STATICL-2FCONTENT-3ACONTENT-METADATA-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-3ACONTENT-29-29"></a>

###### [reader](af10) `staticl/content:content-metadata` (content) (= (make-hash-table :test 'equal))

A hash with additional fields specified in the file's header.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCONTENT-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28STATICL-2FCONTENT-3AGET-TARGET-FILENAME-20GENERIC-FUNCTION-29"></a>

##### [generic-function](ad75) `staticl/content:get-target-filename` site content stage-dir

Should return an absolute pathname to a file where this content item should be rendered.

<a id="x-28STATICL-2FCONTENT-3APREPROCESS-20GENERIC-FUNCTION-29"></a>

##### [generic-function](69df) `staticl/content:preprocess` site plugin content-objects

Returns an additional list content objects such as `RSS` feeds or sitemaps.

<a id="x-28STATICL-2FCONTENT-3AREAD-CONTENT-FROM-DISK-20GENERIC-FUNCTION-29"></a>

##### [generic-function](956b) `staticl/content:read-content-from-disk` site content-type &key exclude

Returns a list of [`content`][a8cd] objects corresponding to a given content type.

`EXCLUDE` argument is a list of pathname prefixes to ignore. Pathnames should be given relative to the root dir of the site.

<a id="x-28STATICL-2FCONTENT-3AREAD-CONTENTS-20GENERIC-FUNCTION-29"></a>

##### [generic-function](61c2) `staticl/content:read-contents` site &key exclude

Returns a list of [`content`][a8cd] objects loaded from files.

`EXCLUDE` argument is a list of pathname prefixes to ignore. Pathnames should be given relative to the root dir of the site.

<a id="x-28STATICL-2FCONTENT-3ASUPPORTED-CONTENT-TYPES-20GENERIC-FUNCTION-29"></a>

##### [generic-function](8a1c) `staticl/content:supported-content-types` site

Returns a list of [`content-type`][d535] objects.

<a id="x-28STATICL-2FCONTENT-3AWRITE-CONTENT-20GENERIC-FUNCTION-29"></a>

##### [generic-function](c9c3) `staticl/content:write-content` site content stage-dir

Writes `CONTENT` object to the `STAGE-DIR`.

<a id="x-28STATICL-2FCONTENT-3AWRITE-CONTENT-TO-STREAM-20GENERIC-FUNCTION-29"></a>

##### [generic-function](7e3d) `staticl/content:write-content-to-stream` site content stream

Writes `CONTENT` object to the `STREAM` using given `FORMAT`.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCONTENT-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FCONTENT-3ASET-METADATA-20FUNCTION-29"></a>

##### [function](98ba) `staticl/content:set-metadata` content key value &key override-slot

Changes metadata dictionary by adding a new item with key `KEY`.

Key should be a string and it is automatically downcased.

Note, this way, you can override content's object slots.
To prevent accidential override, function will raise an error
in case if a slot named `KEY` exists in the object `CONTENT`.
To force override provide `OVERRIDE-SLOT` argument.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-PIPELINE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/CONTENT-PIPELINE

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22STATICL-2FCONTENT-PIPELINE-22-29-20PACKAGE-29"></a>

#### [package](a4ef) `staticl/content-pipeline`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCONTENT-PIPELINE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-PIPELINE-24LOAD-CONTENT-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### LOAD-CONTENT

<a id="x-28STATICL-2FCONTENT-PIPELINE-3ALOAD-CONTENT-20CLASS-29"></a>

###### [class](a939) `staticl/content-pipeline:load-content` ()

**Readers**

<a id="x-28STATICL-2FCONTENT-PIPELINE-3AEXCLUDE-PATTERNS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FCONTENT-PIPELINE-3ALOAD-CONTENT-29-29"></a>

###### [reader](9413) `staticl/content-pipeline:exclude-patterns` (load-content) (:exclude)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCONTENT-PIPELINE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FCONTENT-PIPELINE-3ALOAD-CONTENT-20FUNCTION-29"></a>

##### [function](1558) `staticl/content-pipeline:load-content` &KEY (EXCLUDE (LIST ".qlot"))

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-2FHTML-CONTENT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/CONTENT/HTML-CONTENT

<a id="x-28-23A-28-2828-29-20BASE-CHAR-20-2E-20-22STATICL-2FCONTENT-2FHTML-CONTENT-22-29-20PACKAGE-29"></a>

#### [package](8d92) `staticl/content/html-content`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCONTENT-2FHTML-CONTENT-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28STATICL-2FCONTENT-2FHTML-CONTENT-3ACONTENT-HTML-20GENERIC-FUNCTION-29"></a>

##### [generic-function](8bcb) `staticl/content/html-content:content-html` content

Returns a content as `HTML` string.

<a id="x-28STATICL-2FCONTENT-2FHTML-CONTENT-3ACONTENT-HTML-EXCERPT-20GENERIC-FUNCTION-29"></a>

##### [generic-function](50ed) `staticl/content/html-content:content-html-excerpt` content

Returns an excerpt of full content as `HTML` string.

<a id="x-28STATICL-2FCONTENT-2FHTML-CONTENT-3AHAS-MORE-CONTENT-P-20GENERIC-FUNCTION-29"></a>

##### [generic-function](0a1a) `staticl/content/html-content:has-more-content-p` content

Returns T if there is more content than was returned by [`content-html-excerpt`][3bdd] generic-function.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-2FPAGE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/CONTENT/PAGE

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22STATICL-2FCONTENT-2FPAGE-22-29-20PACKAGE-29"></a>

#### [package](f4ab) `staticl/content/page`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCONTENT-2FPAGE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-2FPAGE-24PAGE-TYPE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### PAGE-TYPE

<a id="x-28STATICL-2FCONTENT-2FPAGE-3APAGE-TYPE-20CLASS-29"></a>

###### [class](62ea) `staticl/content/page:page-type` (content-type)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-2FPAGE-24PAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### PAGE

<a id="x-28STATICL-2FCONTENT-2FPAGE-3APAGE-20CLASS-29"></a>

###### [class](01d8) `staticl/content/page:page` (content-from-file)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-2FPOST-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/CONTENT/POST

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22STATICL-2FCONTENT-2FPOST-22-29-20PACKAGE-29"></a>

#### [package](7edc) `staticl/content/post`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCONTENT-2FPOST-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-2FPOST-24POST-TYPE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### POST-TYPE

<a id="x-28STATICL-2FCONTENT-2FPOST-3APOST-TYPE-20CLASS-29"></a>

###### [class](7706) `staticl/content/post:post-type` (content-type)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-2FPOST-24POST-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### POST

<a id="x-28STATICL-2FCONTENT-2FPOST-3APOST-20CLASS-29"></a>

###### [class](62e1) `staticl/content/post:post` (content-from-file)

This is the class for a page which will not be included into the feed and indices.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCONTENT-2FPOST-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FCONTENT-2FPOST-3APOSTP-20FUNCTION-29"></a>

##### [function](1dff) `staticl/content/post:postp` content-item

Returns T if given object is a content of [`post`][9964] class.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCONTENT-2FREADER-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/CONTENT/READER

<a id="x-28-23A-28-2822-29-20BASE-CHAR-20-2E-20-22STATICL-2FCONTENT-2FREADER-22-29-20PACKAGE-29"></a>

#### [package](7230) `staticl/content/reader`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCONTENT-2FREADER-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FCONTENT-2FREADER-3AREAD-CONTENT-FILE-20FUNCTION-29"></a>

##### [function](0ca9) `staticl/content/reader:read-content-file` file &key (separator \*default-metadata-separator\*)

Returns a plist of metadata from `FILE` with `:TEXT` holding the content going after the `SEPARATOR`.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FCURRENT-ROOT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/CURRENT-ROOT

<a id="x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-22STATICL-2FCURRENT-ROOT-22-29-20PACKAGE-29"></a>

#### [package](37c7) `staticl/current-root`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCURRENT-ROOT-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FCURRENT-ROOT-3ACURRENT-ROOT-20FUNCTION-29"></a>

##### [function](866e) `staticl/current-root:current-root`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FCURRENT-ROOT-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28STATICL-2FCURRENT-ROOT-3AWITH-CURRENT-ROOT-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](5a11) `staticl/current-root:with-current-root` (root) &body body

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FFEEDS-2FATOM-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/FEEDS/ATOM

<a id="x-28-23A-28-2818-29-20BASE-CHAR-20-2E-20-22STATICL-2FFEEDS-2FATOM-22-29-20PACKAGE-29"></a>

#### [package](e364) `staticl/feeds/atom`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FFEEDS-2FATOM-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FFEEDS-2FATOM-24ATOM-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### ATOM

<a id="x-28STATICL-2FFEEDS-2FATOM-3AATOM-20CLASS-29"></a>

###### [class](6475) `staticl/feeds/atom:atom` (feed)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FFEEDS-2FATOM-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FFEEDS-2FATOM-3AATOM-20FUNCTION-29"></a>

##### [function](9c7f) `staticl/feeds/atom:atom` &KEY (TARGET-PATH #P"atom.xml")

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FFEEDS-2FRSS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/FEEDS/RSS

<a id="x-28-23A-28-2817-29-20BASE-CHAR-20-2E-20-22STATICL-2FFEEDS-2FRSS-22-29-20PACKAGE-29"></a>

#### [package](94af) `staticl/feeds/rss`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FFEEDS-2FRSS-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FFEEDS-2FRSS-24RSS-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### RSS

<a id="x-28STATICL-2FFEEDS-2FRSS-3ARSS-20CLASS-29"></a>

###### [class](d13d) `staticl/feeds/rss:rss` (feed)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FFEEDS-2FRSS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FFEEDS-2FRSS-3ARSS-20FUNCTION-29"></a>

##### [function](8f10) `staticl/feeds/rss:rss` &KEY (TARGET-PATH #P"rss.xml")

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FFILTER-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/FILTER

<a id="x-28-23A-28-2814-29-20BASE-CHAR-20-2E-20-22STATICL-2FFILTER-22-29-20PACKAGE-29"></a>

#### [package](07a9) `staticl/filter`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FFILTER-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FFILTER-24FILTER-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### FILTER

<a id="x-28STATICL-2FFILTER-3AFILTER-20CLASS-29"></a>

###### [class](fe46) `staticl/filter:filter` ()

**Readers**

<a id="x-28STATICL-2FFILTER-3AFILTER-FN-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FFILTER-3AFILTER-29-29"></a>

###### [reader](c67c) `staticl/filter:filter-fn` (filter) (:filter-fn)

<a id="x-28STATICL-2FFILTER-3APIPELINE-ITEMS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FFILTER-3AFILTER-29-29"></a>

###### [reader](2eb7) `staticl/filter:pipeline-items` (filter) (:pipeline)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FFILTER-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28STATICL-2FFILTER-3AFILTER-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](34bc) `staticl/filter:filter` (&key path invert) &rest pipeline

Filters input content objects and processes them using steps given as a body.

Arguments:

* `PATH`: if given result will contain only items read from the given path.
* `INVERT`: inverts effect of the filter.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FFORMAT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/FORMAT

<a id="x-28-23A-28-2814-29-20BASE-CHAR-20-2E-20-22STATICL-2FFORMAT-22-29-20PACKAGE-29"></a>

#### [package](adbc) `staticl/format`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FFORMAT-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28STATICL-2FFORMAT-3ATO-HTML-20GENERIC-FUNCTION-29"></a>

##### [generic-function](7935) `staticl/format:to-html` text format

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FINDEX-2FBASE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/INDEX/BASE

<a id="x-28-23A-28-2818-29-20BASE-CHAR-20-2E-20-22STATICL-2FINDEX-2FBASE-22-29-20PACKAGE-29"></a>

#### [package](fd34) `staticl/index/base`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FINDEX-2FBASE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FINDEX-2FBASE-24BASE-INDEX-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### BASE-INDEX

<a id="x-28STATICL-2FINDEX-2FBASE-3ABASE-INDEX-20CLASS-29"></a>

###### [class](2201) `staticl/index/base:base-index` ()

**Readers**

<a id="x-28STATICL-2FCONTENT-3ACONTENT-TEMPLATE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FINDEX-2FBASE-3ABASE-INDEX-29-29"></a>

###### [reader](f408) `staticl/content:content-template` (base-index) (:template)

<a id="x-28STATICL-2FINDEX-2FBASE-3AINDEX-TARGET-PATH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FINDEX-2FBASE-3ABASE-INDEX-29-29"></a>

###### [reader](52a2) `staticl/index/base:index-target-path` (base-index) (:target-path)

Relative pathname to a directory where all pages will be generated.

<a id="x-28STATICL-2FINDEX-2FBASE-3APAGE-SIZE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FINDEX-2FBASE-3ABASE-INDEX-29-29"></a>

###### [reader](5241) `staticl/index/base:page-size` (base-index) (:page-size)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FINDEX-2FBASE-24INDEX-PAGE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### INDEX-PAGE

<a id="x-28STATICL-2FINDEX-2FBASE-3AINDEX-PAGE-20CLASS-29"></a>

###### [class](f94b) `staticl/index/base:index-page` (content)

**Readers**

<a id="x-28STATICL-2FCONTENT-3ACONTENT-TEMPLATE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FINDEX-2FBASE-3AINDEX-PAGE-29-29"></a>

###### [reader](07a2) `staticl/content:content-template` (index-page) (:template)

<a id="x-28STATICL-2FINDEX-2FBASE-3ANEXT-PAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FINDEX-2FBASE-3AINDEX-PAGE-29-29"></a>

###### [reader](521b) `staticl/index/base:next-page` (index-page) (:next-page)

<a id="x-28STATICL-2FINDEX-2FBASE-3APAGE-ITEMS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FINDEX-2FBASE-3AINDEX-PAGE-29-29"></a>

###### [reader](4209) `staticl/index/base:page-items` (index-page) (:items)

<a id="x-28STATICL-2FINDEX-2FBASE-3APAGE-TARGET-PATH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FINDEX-2FBASE-3AINDEX-PAGE-29-29"></a>

###### [reader](cad5) `staticl/index/base:page-target-path` (index-page) (:target-path)

Relative pathname to a file with page content.

<a id="x-28STATICL-2FINDEX-2FBASE-3APAGE-TITLE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FINDEX-2FBASE-3AINDEX-PAGE-29-29"></a>

###### [reader](c10b) `staticl/index/base:page-title` (index-page) (:title)

A title of the page.

<a id="x-28STATICL-2FINDEX-2FBASE-3APREV-PAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FINDEX-2FBASE-3AINDEX-PAGE-29-29"></a>

###### [reader](7e84) `staticl/index/base:prev-page` (index-page) (:prev-page)

**Accessors**

<a id="x-28STATICL-2FINDEX-2FBASE-3ANEXT-PAGE-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20STATICL-2FINDEX-2FBASE-3AINDEX-PAGE-29-29"></a>

###### [accessor](521b) `staticl/index/base:next-page` (index-page) (:next-page)

<a id="x-28STATICL-2FINDEX-2FBASE-3APREV-PAGE-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20STATICL-2FINDEX-2FBASE-3AINDEX-PAGE-29-29"></a>

###### [accessor](7e84) `staticl/index/base:prev-page` (index-page) (:prev-page)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FINDEX-2FPAGINATED-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/INDEX/PAGINATED

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-22STATICL-2FINDEX-2FPAGINATED-22-29-20PACKAGE-29"></a>

#### [package](5c41) `staticl/index/paginated`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FINDEX-2FPAGINATED-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FINDEX-2FPAGINATED-24PAGINATED-INDEX-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### PAGINATED-INDEX

<a id="x-28STATICL-2FINDEX-2FPAGINATED-3APAGINATED-INDEX-20CLASS-29"></a>

###### [class](e451) `staticl/index/paginated:paginated-index` (base-index)

**Readers**

<a id="x-28STATICL-2FINDEX-2FPAGINATED-3APAGE-FILENAME-FN-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FINDEX-2FPAGINATED-3APAGINATED-INDEX-29-29"></a>

###### [reader](e97f) `staticl/index/paginated:page-filename-fn` (paginated-index) (:page-filename-fn)

A callback to change page titles.

Accepts single argument - a page number and should return a pathname relative to the site's root.
By default, it returns index.html for the first page and page-2.html, page-3.html for others.

If site has "clean urls" setting enabled, then additional transformation to the pathname will be
applied automatically.

<a id="x-28STATICL-2FINDEX-2FPAGINATED-3APAGE-TITLE-FN-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FINDEX-2FPAGINATED-3APAGINATED-INDEX-29-29"></a>

###### [reader](a4a1) `staticl/index/paginated:page-title-fn` (paginated-index) (:page-title-fn)

A callback to change page titles.

Accepts single argument - a page number and should return a string.

For example, here is how you can translate page title into a russian:

```lisp
(paginated-index :target-path #P"ru/"
                 :page-title-fn (lambda (num)
                                  (fmt "Страница ~A" num)))
```
<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FINDEX-2FPAGINATED-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FINDEX-2FPAGINATED-3APAGINATED-INDEX-20FUNCTION-29"></a>

##### [function](4b28) `staticl/index/paginated:paginated-index` &rest initargs &key target-path page-size template page-title-fn page-filename-fn

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FLINKS-2FLINK-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/LINKS/LINK

<a id="x-28-23A-28-2818-29-20BASE-CHAR-20-2E-20-22STATICL-2FLINKS-2FLINK-22-29-20PACKAGE-29"></a>

#### [package](9fff) `staticl/links/link`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FLINKS-2FLINK-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FLINKS-2FLINK-24LINK-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### LINK

<a id="x-28STATICL-2FLINKS-2FLINK-3ALINK-20CLASS-29"></a>

###### [class](2f55) `staticl/links/link:link` ()

**Readers**

<a id="x-28STATICL-2FLINKS-2FLINK-3ALINK-CONTENT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FLINKS-2FLINK-3ALINK-29-29"></a>

###### [reader](08b0) `staticl/links/link:link-content` (link) (:content)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FLINKS-2FLINK-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FLINKS-2FLINK-3ALINK-20FUNCTION-29"></a>

##### [function](1d57) `staticl/links/link:link` content

Creates a link to the given content piece.

When such object is passed to the template, it is resolved to a
page `URL` and title.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FLINKS-2FPREV-NEXT-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/LINKS/PREV-NEXT

<a id="x-28-23A-28-2823-29-20BASE-CHAR-20-2E-20-22STATICL-2FLINKS-2FPREV-NEXT-22-29-20PACKAGE-29"></a>

#### [package](bfda) `staticl/links/prev-next`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FLINKS-2FPREV-NEXT-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FLINKS-2FPREV-NEXT-24PREV-NEXT-LINKS-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### PREV-NEXT-LINKS

<a id="x-28STATICL-2FLINKS-2FPREV-NEXT-3APREV-NEXT-LINKS-20CLASS-29"></a>

###### [class](b64b) `staticl/links/prev-next:prev-next-links` ()

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FLINKS-2FPREV-NEXT-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FLINKS-2FPREV-NEXT-3APREV-NEXT-LINKS-20FUNCTION-29"></a>

##### [function](810a) `staticl/links/prev-next:prev-next-links`

Creates a links between pages.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FNAVIGATION-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/NAVIGATION

<a id="x-28-23A-28-2818-29-20BASE-CHAR-20-2E-20-22STATICL-2FNAVIGATION-22-29-20PACKAGE-29"></a>

#### [package](7ef9) `staticl/navigation`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FNAVIGATION-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FNAVIGATION-24ITEM-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### ITEM

<a id="x-28STATICL-2FNAVIGATION-3AITEM-20CLASS-29"></a>

###### [class](e7ae) `staticl/navigation:item` ()

**Readers**

<a id="x-28STATICL-2FNAVIGATION-3AMENU-ITEM-TITLE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FNAVIGATION-3AITEM-29-29"></a>

###### [reader](74a0) `staticl/navigation:menu-item-title` (item) (:title)

<a id="x-28STATICL-2FNAVIGATION-3AMENU-ITEM-URL-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FNAVIGATION-3AITEM-29-29"></a>

###### [reader](0c86) `staticl/navigation:menu-item-url` (item) (:url)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FNAVIGATION-24MENU-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### MENU

<a id="x-28STATICL-2FNAVIGATION-3AMENU-20CLASS-29"></a>

###### [class](3021) `staticl/navigation:menu` ()

**Readers**

<a id="x-28STATICL-2FNAVIGATION-3AMENU-ITEMS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FNAVIGATION-3AMENU-29-29"></a>

###### [reader](03ac) `staticl/navigation:menu-items` (menu) (:items)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FNAVIGATION-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FNAVIGATION-3AITEM-20FUNCTION-29"></a>

##### [function](e07d) `staticl/navigation:item` title url

<a id="x-28STATICL-2FNAVIGATION-3AMENU-20FUNCTION-29"></a>

##### [function](affb) `staticl/navigation:menu` &rest items

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FPIPELINE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/PIPELINE

<a id="x-28-23A-28-2816-29-20BASE-CHAR-20-2E-20-22STATICL-2FPIPELINE-22-29-20PACKAGE-29"></a>

#### [package](a489) `staticl/pipeline`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FPIPELINE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28STATICL-2FPIPELINE-3APROCESS-ITEMS-20GENERIC-FUNCTION-29"></a>

##### [generic-function](ccdc) `staticl/pipeline:process-items` site pipeline-node content-items

A method for this generic function should process `CONTENT-ITEMS` - a list of conten items
produced by a previous pipeline nodes.

During the execution, method can call [`produce-item`][45ad] or [`remove-item`][4ba7] functions to add a new content
or to remove some content item.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FPIPELINE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FPIPELINE-3AEXECUTE-PIPELINE-20FUNCTION-29"></a>

##### [function](0a84) `staticl/pipeline:execute-pipeline` site

<a id="x-28STATICL-2FPIPELINE-3APRODUCE-ITEM-20FUNCTION-29"></a>

##### [function](f3fa) `staticl/pipeline:produce-item` item

<a id="x-28STATICL-2FPIPELINE-3AREMOVE-ITEM-20FUNCTION-29"></a>

##### [function](1d91) `staticl/pipeline:remove-item` item

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FPLUGIN-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/PLUGIN

<a id="x-28-23A-28-2814-29-20BASE-CHAR-20-2E-20-22STATICL-2FPLUGIN-22-29-20PACKAGE-29"></a>

#### [package](169a) `staticl/plugin`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FPLUGIN-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FPLUGIN-24PLUGIN-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### PLUGIN

<a id="x-28STATICL-2FPLUGIN-3APLUGIN-20CLASS-29"></a>

###### [class](f63f) `staticl/plugin:plugin` ()

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FPLUGIN-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FPLUGIN-3AMAKE-PLUGIN-20FUNCTION-29"></a>

##### [function](c3a8) `staticl/plugin:make-plugin` name &rest initargs

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FRSYNC-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/RSYNC

<a id="x-28-23A-28-2813-29-20BASE-CHAR-20-2E-20-22STATICL-2FRSYNC-22-29-20PACKAGE-29"></a>

#### [package](44b1) `staticl/rsync`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FRSYNC-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FRSYNC-24RSYNC-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### RSYNC

<a id="x-28STATICL-2FRSYNC-3ARSYNC-20CLASS-29"></a>

###### [class](4994) `staticl/rsync:rsync` ()

**Readers**

<a id="x-28STATICL-2FRSYNC-3ARSYNC-HOST-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FRSYNC-3ARSYNC-29-29"></a>

###### [reader](2b42) `staticl/rsync:rsync-host` (rsync) (:host)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FRSYNC-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FRSYNC-3ARSYNC-20FUNCTION-29"></a>

##### [function](431f) `staticl/rsync:rsync` host

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FSITE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/SITE

<a id="x-28-23A-28-2812-29-20BASE-CHAR-20-2E-20-22STATICL-2FSITE-22-29-20PACKAGE-29"></a>

#### [package](a037) `staticl/site`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FSITE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FSITE-24SITE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### SITE

<a id="x-28STATICL-2FSITE-3ASITE-20CLASS-29"></a>

###### [class](4f3b) `staticl/site:site` ()

**Readers**

<a id="x-28STATICL-2FSITE-3ACLEAN-URLS-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FSITE-3ASITE-29-29"></a>

###### [reader](f77b) `staticl/site:clean-urls-p` (site) (:clean-urls)

Generate some-page/index.html instead of some-page.html to make `URL`s look like https://my-site.com/some-page/ instead of https://my-site.com/some-page.html

<a id="x-28STATICL-2FSITE-3ASITE-CHARSET-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FSITE-3ASITE-29-29"></a>

###### [reader](080d) `staticl/site:site-charset` (site) (:charset)

Site's charset. By default it is `UTF-8`.

<a id="x-28STATICL-2FSITE-3ASITE-CONTENT-ROOT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FSITE-3ASITE-29-29"></a>

###### [reader](cd08) `staticl/site:site-content-root` (site) (:root)

A directory pathname where .staticlrc file can be found.

<a id="x-28STATICL-2FSITE-3ASITE-DESCRIPTION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FSITE-3ASITE-29-29"></a>

###### [reader](742b) `staticl/site:site-description` (site) (:title)

Site's description.

<a id="x-28STATICL-2FSITE-3ASITE-NAVIGATION-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FSITE-3ASITE-29-29"></a>

###### [reader](c6a1) `staticl/site:site-navigation` (site) (:navigation)

Site's navigation.

<a id="x-28STATICL-2FSITE-3ASITE-PIPELINE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FSITE-3ASITE-29-29"></a>

###### [reader](f87d) `staticl/site:site-pipeline` (site) (:pipeline)

A list of pipline nodes

<a id="x-28STATICL-2FSITE-3ASITE-THEME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FSITE-3ASITE-29-29"></a>

###### [reader](c3b2) `staticl/site:site-theme` (site) (:theme)

A theme object for the site.

<a id="x-28STATICL-2FSITE-3ASITE-TITLE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FSITE-3ASITE-29-29"></a>

###### [reader](4ef2) `staticl/site:site-title` (site) (:title)

Site's title.

<a id="x-28STATICL-2FSITE-3ASITE-URL-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FSITE-3ASITE-29-29"></a>

###### [reader](98d2) `staticl/site:site-url` (site) (:url)

Site's `URL`.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FSITE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FSITE-3AMAKE-SITE-20FUNCTION-29"></a>

##### [function](0112) `staticl/site:make-site` root

<a id="x-28STATICL-2FSITE-3ASITE-20FUNCTION-29"></a>

##### [function](ed38) `staticl/site:site` title &rest args

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FTAG-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/TAG

<a id="x-28-23A-28-2811-29-20BASE-CHAR-20-2E-20-22STATICL-2FTAG-22-29-20PACKAGE-29"></a>

#### [package](d90f) `staticl/tag`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FTAG-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FTAG-24TAG-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### TAG

<a id="x-28STATICL-2FTAG-3ATAG-20CLASS-29"></a>

###### [class](fca4) `staticl/tag:tag` ()

**Readers**

<a id="x-28STATICL-2FTAG-3ATAG-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FTAG-3ATAG-29-29"></a>

###### [reader](3dfa) `staticl/tag:tag-name` (tag) (:name)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FTHEME-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/THEME

<a id="x-28-23A-28-2813-29-20BASE-CHAR-20-2E-20-22STATICL-2FTHEME-22-29-20PACKAGE-29"></a>

#### [package](a484) `staticl/theme`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FTHEME-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FTHEME-24THEME-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### THEME

<a id="x-28STATICL-2FTHEME-3ATHEME-20CLASS-29"></a>

###### [class](60b1) `staticl/theme:theme` (print-items-mixin)

**Readers**

<a id="x-28STATICL-2FTHEME-3ATHEME-PATH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20STATICL-2FTHEME-3ATHEME-29-29"></a>

###### [reader](db4b) `staticl/theme:theme-path` (theme) (:path)

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FTHEME-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28STATICL-2FTHEME-3ACOPY-STATIC-20GENERIC-FUNCTION-29"></a>

##### [generic-function](6417) `staticl/theme:copy-static` theme stage-dir

Copies static files such as `CSS`, `JS`, images into the `STAGE-DIR`.

Usually it is enough to define a method for [`list-static`][412e] generic-function.

<a id="x-28STATICL-2FTHEME-3ALIST-STATIC-20GENERIC-FUNCTION-29"></a>

##### [generic-function](5fc1) `staticl/theme:list-static` theme

Returns a list of static files such as `CSS`, `JS`, images.

Each list item should be a list of two items where first
item is an absolute pathname and second is a pathname relative
to the root of the site.

<a id="x-28STATICL-2FTHEME-3ARENDER-20GENERIC-FUNCTION-29"></a>

##### [generic-function](1699) `staticl/theme:render` theme template-name vars stream

Renders fills template named `TEMPLATE-NAME` with given `VARS` and renders into a given `STREAM`.

* `NAME` argument is a string.
* `VARS` argument is a hash table with string keys.

<a id="x-28STATICL-2FTHEME-3ATEMPLATE-VARS-20GENERIC-FUNCTION-29"></a>

##### [generic-function](4fb7) `staticl/theme:template-vars` site object &key hash

Fills a hash-table given as `HASH` argument with variables for filling a template.

If hash is `NIL`, then a new hash-table should be allocated with `EQUAL` `:TEST` argument.

Returned hash-table will be used for rendering a template for an `OBJECT`.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FURL-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/URL

<a id="x-28-23A-28-2811-29-20BASE-CHAR-20-2E-20-22STATICL-2FURL-22-29-20PACKAGE-29"></a>

#### [package](2cb0) `staticl/url`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FURL-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28STATICL-2FURL-3AOBJECT-URL-20GENERIC-FUNCTION-29"></a>

##### [generic-function](3922) `staticl/url:object-url` site obj &key full &allow-other-keys

Returns a full object `URL`.
A method should return an relative `URL`, but if case if `FULL` argument was given,
the full url with schema and domain will be returned.

Note a call to this method should happen in a context of the [`with-base-url`][888b] macro,
because it is always return a path from the site's root even if `FULL` is not given
(in this case return only the path without a domain).

You may wonder: "Why does we bother to return a path without a domain?"
It is much easier to service such static site locally for debugging purpose, because
you don't have to setup a web server and dns resolver.

Actually you will need to use `FULL` argument only in a rare case when you really need
and absolute `URL`, for example in an `RSS` feed.

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FURL-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28STATICL-2FURL-3AWITH-BASE-URL-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](0fe2) `staticl/url:with-base-url` (url) &body body

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-40STATICL-2FUTILS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### STATICL/UTILS

<a id="x-28-23A-28-2813-29-20BASE-CHAR-20-2E-20-22STATICL-2FUTILS-22-29-20PACKAGE-29"></a>

#### [package](9ca9) `staticl/utils`

<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FUTILS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28STATICL-2FUTILS-3AABSOLUTE-URL-P-20FUNCTION-29"></a>

##### [function](b98e) `staticl/utils:absolute-url-p` url

<a id="x-28STATICL-2FUTILS-3AASSERT-ABSOLUTE-URL-20FUNCTION-29"></a>

##### [function](5a5f) `staticl/utils:assert-absolute-url` url

<a id="x-28STATICL-2FUTILS-3ANORMALIZE-PLIST-20FUNCTION-29"></a>

##### [function](3c28) `staticl/utils:normalize-plist` plist &rest normalizers-plist &key &allow-other-keys

Returns a new list where each value is replaced with results of call of normalizing functions.

For example:

```
CL-USER> (normalize-plist '(:foo "Bar" :blah 123)
                          :foo (lambda (value)
                                 (alexandria:make-keyword (string-upcase value))))
(LIST :FOO :BAR :BLAH 123)
```
<a id="x-28STATICL-DOCS-2FINDEX-3A-3A-7C-40STATICL-2FUTILS-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28STATICL-2FUTILS-3ADO-FILES-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](d944) `staticl/utils:do-files` (filename root-path &key file-type) &body body

For each file under `ROOT-PATH`, run `BODY`. If `FILE-TYPE` is provided, only run
`BODY` on files that match the given extension.


[0cf8]: https://40ants.com/staticl/
[3bdd]: https://40ants.com/staticl/#x-28STATICL-2FCONTENT-2FHTML-CONTENT-3ACONTENT-HTML-EXCERPT-20GENERIC-FUNCTION-29
[9964]: https://40ants.com/staticl/#x-28STATICL-2FCONTENT-2FPOST-3APOST-20CLASS-29
[a8cd]: https://40ants.com/staticl/#x-28STATICL-2FCONTENT-3ACONTENT-20CLASS-29
[d535]: https://40ants.com/staticl/#x-28STATICL-2FCONTENT-3ACONTENT-TYPE-20CLASS-29
[d70a]: https://40ants.com/staticl/#x-28STATICL-2FCONTENT-3APREPROCESS-20GENERIC-FUNCTION-29
[c4c2]: https://40ants.com/staticl/#x-28STATICL-2FCONTENT-3AWRITE-CONTENT-20GENERIC-FUNCTION-29
[96e3]: https://40ants.com/staticl/#x-28STATICL-2FCONTENT-3AWRITE-CONTENT-TO-STREAM-20GENERIC-FUNCTION-29
[45ad]: https://40ants.com/staticl/#x-28STATICL-2FPIPELINE-3APRODUCE-ITEM-20FUNCTION-29
[4ba7]: https://40ants.com/staticl/#x-28STATICL-2FPIPELINE-3AREMOVE-ITEM-20FUNCTION-29
[412e]: https://40ants.com/staticl/#x-28STATICL-2FTHEME-3ALIST-STATIC-20GENERIC-FUNCTION-29
[888b]: https://40ants.com/staticl/#x-28STATICL-2FURL-3AWITH-BASE-URL-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[c5b7]: https://40ants.com/staticl/#x-28STATICL-3ASTAGE-20FUNCTION-29
[2594]: https://github.com/40ants/staticl
[ba40]: https://github.com/40ants/staticl/actions
[0b1b]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/clean-urls.lisp#L1
[1e0e]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/clean-urls.lisp#L50
[5ca0]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/clean-urls.lisp#L63
[a4ef]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content-pipeline.lisp#L1
[a939]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content-pipeline.lisp#L15
[9413]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content-pipeline.lisp#L16
[1558]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content-pipeline.lisp#L23
[72a2]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L1
[aa5f]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L104
[310a]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L105
[1a5e]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L114
[0a5a]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L115
[f1f2]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L122
[5090]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L125
[34b8]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L128
[c43f]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L131
[a0c4]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L134
[bdeb]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L141
[971f]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L144
[8a1c]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L191
[956b]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L230
[61c2]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L250
[c9c3]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L261
[ad75]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L273
[7e3d]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L299
[69df]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L313
[98ba]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L402
[4c56]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L78
[8e61]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L79
[94f6]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L82
[cf13]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L97
[af10]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content.lisp#L98
[8d92]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/html-content.lisp#L1
[50ed]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/html-content.lisp#L12
[0a1a]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/html-content.lisp#L15
[8bcb]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/html-content.lisp#L9
[f4ab]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/page.lisp#L1
[01d8]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/page.lisp#L11
[62ea]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/page.lisp#L18
[7edc]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/post.lisp#L1
[62e1]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/post.lisp#L14
[7706]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/post.lisp#L22
[1dff]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/post.lisp#L32
[7230]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/reader.lisp#L1
[0ca9]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/content/reader.lisp#L48
[592c]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/core.lisp#L1
[c8d0]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/core.lisp#L32
[37c7]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/current-root.lisp#L1
[866e]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/current-root.lisp#L21
[5a11]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/current-root.lisp#L35
[e364]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/feeds/atom.lisp#L1
[6475]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/feeds/atom.lisp#L11
[9c7f]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/feeds/atom.lisp#L17
[94af]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/feeds/rss.lisp#L1
[d13d]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/feeds/rss.lisp#L10
[8f10]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/feeds/rss.lisp#L16
[07a9]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/filter.lisp#L1
[fe46]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/filter.lisp#L24
[c67c]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/filter.lisp#L25
[2eb7]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/filter.lisp#L28
[34bc]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/filter.lisp#L35
[adbc]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/format.lisp#L1
[7935]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/format.lisp#L11
[fd34]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L1
[2201]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L39
[52a2]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L40
[5241]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L44
[f408]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L47
[f94b]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L56
[cad5]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L57
[c10b]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L61
[4209]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L65
[7e84]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L68
[521b]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L71
[07a2]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/base.lisp#L74
[5c41]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/paginated.lisp#L1
[e451]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/paginated.lisp#L48
[e97f]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/paginated.lisp#L49
[a4a1]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/paginated.lisp#L59
[4b28]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/index/paginated.lisp#L79
[9fff]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/links/link.lisp#L1
[2f55]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/links/link.lisp#L19
[08b0]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/links/link.lisp#L20
[1d57]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/links/link.lisp#L28
[bfda]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/links/prev-next.lisp#L1
[b64b]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/links/prev-next.lisp#L17
[810a]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/links/prev-next.lisp#L21
[7ef9]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/navigation.lisp#L1
[e7ae]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/navigation.lisp#L14
[0c86]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/navigation.lisp#L15
[74a0]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/navigation.lisp#L18
[e07d]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/navigation.lisp#L26
[3021]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/navigation.lisp#L32
[03ac]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/navigation.lisp#L33
[affb]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/navigation.lisp#L41
[a489]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/pipeline.lisp#L1
[ccdc]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/pipeline.lisp#L18
[f3fa]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/pipeline.lisp#L27
[1d91]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/pipeline.lisp#L33
[0a84]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/pipeline.lisp#L40
[169a]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/plugin.lisp#L1
[f63f]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/plugin.lisp#L12
[c3a8]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/plugin.lisp#L19
[44b1]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/rsync.lisp#L1
[431f]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/rsync.lisp#L16
[4994]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/rsync.lisp#L8
[2b42]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/rsync.lisp#L9
[a037]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L1
[0112]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L132
[4f3b]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L44
[cd08]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L45
[4ef2]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L49
[742b]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L53
[c6a1]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L57
[080d]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L61
[98d2]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L65
[f77b]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L69
[c3b2]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L73
[f87d]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L77
[ed38]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/site.lisp#L91
[d90f]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/tag.lisp#L1
[fca4]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/tag.lisp#L14
[3dfa]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/tag.lisp#L15
[a484]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/theme.lisp#L1
[6417]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/theme.lisp#L106
[60b1]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/theme.lisp#L23
[db4b]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/theme.lisp#L24
[4fb7]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/theme.lisp#L33
[1699]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/theme.lisp#L41
[5fc1]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/theme.lisp#L98
[2cb0]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/url.lisp#L1
[3922]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/url.lisp#L25
[0fe2]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/url.lisp#L66
[9ca9]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/utils.lisp#L1
[b98e]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/utils.lisp#L164
[5a5f]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/utils.lisp#L174
[d944]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/utils.lisp#L76
[3c28]: https://github.com/40ants/staticl/blob/0a7e04f8442e26e8e18a3a83a891f43344fd0e33/src/utils.lisp#L98
[5825]: https://github.com/40ants/staticl/issues
[cc3e]: https://quickdocs.org/3bmd
[d94a]: https://quickdocs.org/3bmd-ext-code-blocks
[8236]: https://quickdocs.org/alexandria
[1059]: https://quickdocs.org/cl-fad
[49b9]: https://quickdocs.org/cl-ppcre
[dfc0]: https://quickdocs.org/cl-sitemaps
[61a4]: https://quickdocs.org/closer-mop
[2700]: https://quickdocs.org/closure-template
[0b13]: https://quickdocs.org/feeder
[ed60]: https://quickdocs.org/fuzzy-dates
[46a1]: https://quickdocs.org/local-time
[7f8b]: https://quickdocs.org/log4cl
[2103]: https://quickdocs.org/quri
[c41d]: https://quickdocs.org/serapeum
[ef7f]: https://quickdocs.org/str
[52a0]: https://quickdocs.org/utilities.print-items

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
