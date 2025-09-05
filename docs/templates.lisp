(uiop:define-package #:staticl-docs/templates
  (:use #:cl)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:staticl/user-package
                #:site))
(in-package #:staticl-docs/templates)


(in-readtable pythonic-string-syntax)

(defsection @templates (:title "Templates"
                        :ignore-words ("HTML"
                                       "CSS"
                                       "URL"
                                       "VARS"
                                       "STATICLRC"))
  (@variables section))


(defsection @variables (:title "Variables")
  """
Templates in StatiCL can access various variables that contain information about the site, content, and custom data. One of the powerful features is the ability to define custom site variables that can be used throughout your templates.

## Site Variables

Site variables are custom key-value pairs that you can define in your site configuration using the VARS argument. These variables become available in templates under the `$site` namespace and allow you to customize your site's appearance and behavior without modifying template files directly.

### Defining Site Variables

Site variables are defined in your `.staticlrc` configuration file using the VARS argument. The value should be a hash-table having keys of type `string`:

```lisp
(site "My Site"
      :description "A great static site"
      :url "https://example.com/"
      ;; Custom site variables
      :vars (serapeum:dict "copyright_from" 2024
                           "copyright_to" (nth-value 5
                                                     (get-decoded-time))
                           "hide_staticl_logo" t
                           "custom_message" "Welcome to my site!"))
```

In this example:
- `copyright_from` sets the starting year for copyright notice
- `copyright_to` dynamically gets the current year
- `hide_staticl_logo` is a boolean flag to control logo visibility
- `custom_message` provides a custom text string

### Using Variables in Templates

Once defined, these variables can be accessed in your templates using the `$site.variable_name` syntax. Here are some examples:

**Copyright Information:**

```html
{if $site.copyright_from}
  {$site.copyright_from}
  {if $site.copyright_to}&nbsp;-&nbsp;{$site.copyright_to}{/if}
{/if}
```

This template code will render something like "2024 - 2025" if both copyright years are defined.

**Conditional Content:**

```html
{if not $site.hide_staticl_logo}
<a id="staticl-logo" href="https://github.com/40ants/staticl">
  <img src="/img/staticl-logo-small.webp" style="height: 32px" alt="StatiCL logo" />
</a>
{/if}
```

This allows you to conditionally show or hide the StatiCL logo based on your site configuration.

**Custom Text:**
```html
<h1>{$site.custom_message}</h1>
```

## Built-in Site Variables

Besides custom variables, StatiCL provides several built-in variables accessible under `$site`:

- `$site.title` - Site title.
- `$site.description` - Site description.
- `$site.url` - Site URL
- `$site.pubdate` - Local-time timestamp of the moment when site was rendered.
- `$site.charset` - Character encoding
- `$site.navigation` - Navigation menu structure

## Content Variables

Templates also have access to content-specific variables under `$content`:

- `$content.title` - Content title
- `$content.created_at` - Date when content was published
- `$content.html` - Full content rendered to HTML
- `$content.url` - Content URL
- `$content.injections` - Code injections for head, before/after content

These variables make it easy to create dynamic, customizable templates while keeping your configuration separate from your template code.
""")
