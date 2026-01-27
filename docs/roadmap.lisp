(uiop:define-package #:staticl-docs/roadmap
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax))
(in-package #:staticl-docs/roadmap)


(in-readtable pythonic-string-syntax)

(defsection @roadmap (:title "Roadmap"
                      :ignore-words ("CSS"
                                     "JS"
                                     "HTML"
                                     "RSS"
                                     "CLI"
                                     "SEO"
                                     "CDN"
                                     "API"
                                     "OG"
                                     "FTP"
                                     "S3"
                                     "i18n"
                                     "YouTube"
                                     "GitHub"
                                     "Coleslaw"
                                     "WebP"
                                     "URL"
                                     "URLs"))
  """
This document outlines the planned development direction for StatiCL. The roadmap is organized into phases, though items may be implemented in a different order based on community needs and contributions.

# Phase 1: Stabilization and Quality

## Testing

The current test coverage is minimal. We need to:

* Add comprehensive unit tests for core functionality (content reading, pipeline execution, URL generation)
* Create integration tests for the complete pipeline
* Add tests for each plugin and content format
* Set up test fixtures with example sites

## Documentation

* Write a guide on creating custom themes
* Write a guide on creating plugins and custom pipeline nodes
* Create a cookbook with common recipes and patterns
* Document the template variable reference for theme authors

# Phase 2: Performance

## Incremental Builds

Currently, StatiCL rebuilds the entire site on every change. For large sites, this can be slow. We need to implement:

* Dependency tracking between content files
* Rebuild only changed pages and their dependents
* Cache parsing and rendering results
* Persist cache between builds

## Parallel Processing

* Process independent content items in parallel
* Optimize pipeline for multi-threaded execution
* Add configuration options for parallelism level

# Phase 3: Extended Functionality

## New Plugins

Port remaining Coleslaw plugins and add new ones:

* `analytics` - Google Analytics, Yandex.Metrika, Plausible, Umami
* `comments` - Utterances, Giscus (GitHub-based comments), Isso (self-hosted)
* `search` - Client-side search using Lunr.js or Pagefind
* `toc` - Auto-generate table of contents for long posts
* `reading-time` - Estimate reading time for posts
* `related-posts` - Show related posts based on tags or content similarity
* `social-cards` - Generate OG images for social media sharing
* `syntax-highlight` - Configurable code syntax highlighting with theme support
* `image-gallery` - Lightbox galleries for images
* `footnotes` - Enhanced footnote support

## Asset Pipeline

* CSS/JS minification and bundling
* Image optimization (WebP conversion, responsive images, lazy loading)
* Asset fingerprinting for cache busting
* Source maps for debugging

## Content Workflow

* Draft system (`draft: true` in frontmatter to exclude from build)
* Scheduled posts (publish date in the future)
* Content templates for creating new posts/pages
* Content validation and link checking

# Phase 4: Internationalization

## Multi-language Support

This is a key differentiator for StatiCL:

* i18n support for themes and navigation menus
* Automatic language switcher generation
* Proper `hreflang` tags for SEO
* Separate RSS/Atom feeds per language
* Locale-aware date formatting
* Translation workflow helpers

# Phase 5: Developer Experience

## CLI Improvements

Enhance the Roswell script with more commands:

* `staticl new post "Title"` - Create a new post from template
* `staticl new page "Title"` - Create a new page from template
* `staticl check` - Validate links, images, and content
* `staticl deploy <target>` - Deploy to various platforms
* `staticl clean` - Remove generated files

## Deployment Integrations

* GitHub Pages integration with GitHub Actions workflow
* Netlify configuration and deploy script
* Vercel configuration
* S3/CloudFront deployment
* FTP deployment (in addition to existing rsync)
* Cloudflare Pages support

## Additional Content Formats

* Org-mode support (popular in the Lisp/Emacs community)
* AsciiDoc format
* reStructuredText format
* Jupyter notebook rendering

# Phase 6: Themes

## New Themes

* Minimalist themes (Terminal-style, Tufte CSS inspired)
* Documentation-focused themes (suitable for project docs)
* Portfolio themes (for showcasing projects)
* Dark mode support and theme switcher

## Theme Features

* Built-in dark/light mode toggle
* Responsive design improvements
* Accessibility improvements (ARIA labels, skip links)
* Print-friendly styles

# Phase 7: Community and Promotion

* Create video tutorials and demos for YouTube
* Build a gallery of sites powered by StatiCL
* Write comparison articles with Hugo, Jekyll, Zola
* Publish articles on Lisp community resources
* Create starter templates for common use cases

# Contributing

Contributions are welcome! If you're interested in working on any of these items, please:

1. Open an issue to discuss the feature
2. Check if there's already work in progress
3. Submit a pull request with tests and documentation

See the [GitHub repository](https://github.com/40ants/staticl) for more details.
"""
  )
