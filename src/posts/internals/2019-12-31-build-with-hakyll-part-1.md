---
title: How this page is generated - Part 01
subtitle: Introduction to Hakyll
tags: internal, technical, haskell
description: This site is build with Hakyll, here I explain how.
image: https://images.unsplash.com/photo-1576020363294-ab5dca00b6f8?w=1000
image-credits: |
    Picture taken by <a style="background-color:black;color:white;text-decoration:none;padding:4px 6px;font-family:-apple-system, BlinkMacSystemFont, &quot;San Francisco&quot;, &quot;Helvetica Neue&quot;, Helvetica, Ubuntu, Roboto, Noto, &quot;Segoe UI&quot;, Arial, sans-serif;font-size:12px;font-weight:bold;line-height:1.2;display:inline-block;border-radius:3px" href="https://unsplash.com/@mrsimonfischer?utm_medium=referral&amp;utm_campaign=photographer-credit&amp;utm_content=creditBadge" target="_blank" rel="noopener noreferrer" title="Download free do whatever you want high-resolution photos from Szymon Fischer"><span style="display:inline-block;padding:2px 3px"><svg xmlns="http://www.w3.org/2000/svg" style="height:12px;width:auto;position:relative;vertical-align:middle;top:-2px;fill:white" viewBox="0 0 32 32"><title>unsplash-logo</title><path d="M10 9V0h12v9H10zm12 5h10v18H0V14h10v9h12v-9z"></path></svg></span><span style="display:inline-block;padding:2px 3px">Szymon Fischer</span></a> on Unsplash
...


# Hakyll: The Basics

Hakyll is tool to build static webpages and written in Haskell borrowing part of its name from it and the other from the renown static site engine [Jekyll](https://jekyllrb.com/). The fundamental difference is that instead of beeing a standalone program, Hakyll is implementented as a library offering a rich DSL to define the contents of a website. The DSL can be used to include static assets, compile pages using templates and even generate whole pages by itself i.e. RSS and ATOM feeds.
As Hakyll is written in Haskell combining it with the great text conversion tool [pandoc](https://pandoc.org/) is a low hanging fruit. The combination of both allows compiling from several different file formats including (an extended version of) Markdown, [RST](http://docutils.sourceforge.net/docs/ref/rst/introduction.html) and Emacs [Org-Mode](http://orgmode.org/).

# The inner workings of this website

:::{.note}
*I won't go into all the things used in this article, instead there will be individual posts going into more detail about different elements*
:::

- Part 01: Hakyll basics and context *(this article)* 
- Part 02: *(WIP)*

## Part 01

### Setup

Setting up Hakyll in general using stack or cabal is already documented extensively. My take on this project involves using [nix](https://nixos.org/nix/) as dependency/package manager. How I set up this blog using nix will also be discussed another time.

### Basics

In this article I would like to introduce the basic concepts of Hakyll in an applied way.

The heart of this page is [`generator/Site.hs`](https://github.com/ysndr/blog/blob/release/generator/Site.hs).

At it's bare minimum a basic Hakyll generator looks like this:

``` haskell
config :: Configuration
config = defaultConfiguration {
    -- config overrides such as the path to the page content source
    -- and compiled item's destination  
}

-- A context extending the default context by a date field
postCtx :: Tags -> Tags -> Context String
postCtx tags category = dateField "date" "%B %e, %Y"
                     <> defaultContext 

main :: IO ()
main = hakyllWith config $ do
    match "posts/**.md" $ do
        route $ setExtension "html"
        compile
            $   pandocCompiler
            -- applies the post template
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            -- embeds the rendered post into the page template 
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
    
    create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/**"
                let postCtx = postCtx tags categories
                let archiveCtx = listField "posts" postCtx (return posts)
                            <> constField "title" "Archive"
                            <> customBaseContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls

```

This will render all markdown files under `posts/` to html files under our build route and setup an archive site listing all posts.

In this snipped we see several things lets go through them individually:

1. config
2. matching routes
3. compiling with pandoc
4. templates
5. contexts

#### config

``` haskell
config :: Configuration
config = defaultConfiguration { }
```

This sets up the runtime configuration of hakyll itself. With it we can override among others the folder in which Hakyll searches for its content and where the result should be stored. All available options can be found inside the [documentation](https://jaspervdj.be/hakyll/reference/Hakyll-Core-Configuration.html)

#### Matching routes
``` haskell
match "posts/**.md" $ do
    route $ setExtension "html"
    compile
        $   pandocCompiler
        -- applies the post template
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        -- embeds the rendered post into the page template 
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
```
In order to create a page from an existing source we use the match function. It takes a `FilePath` and a function telling it what to do with the file. `route` will then map the matched items to the paths created by `setExtension` which unsurprisingly only changes the resulting files extension to `html`. There are a few other functions that can be used here (basically all functions that return `Routes`). Those can be used to do more advanced route editing such as creating fancy paths using `index.html`^[[rohanjain.in/hakyll-clean-urls/](rohanjain.in/hakyll-clean-urls/)].

But what does this function actually do in particular?
1. It searches for all markdown files under the `posts/` directory.
2. Defines their final route
3. Compiles them to html using pandoc
4. Embedds them into a post template
5. Embedds raw posts into the default page template
6. And finally relativizes urls, which is a fancy function that keeps track of the resources referenced locally and keeps their links up to date.

##### Compiling with Pandoc

In the snippet above the default `pandocCompiler` function is used to read the content of the file and transform it into HTML using Hakyll's default options for pandoc. Aside `pandocCompiler` there are a few more low level functions available that allow deeperr customization in the regards of which pandoc templates are used, which extensions activated and so forth. There are also `pandocCompilerWithTransform` and `pandocCompilerWithTransformM` that allow editing the intermediate parsed content pandoc uses internally. At this point rich postprocessing can be applied, just alike the usuall pandoc filters. The only grain is that existing pandoc filters (i.e. [pandocfilters](https://github.com/jgm/pandocfilters) or [panflute](https://github.com/sergiocorreia/panflute)) cannot be easily applied with Hakyll.

#### Creating Routes

Additionally to matching exitsing files and compiling them, one can also generate fully independent files using the `create` function.

``` haskell
create ["archive.html"] $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll "posts/**"
        let postCtx = postCtx tags categories
        let archiveCtx = listField "posts" postCtx (return posts)
                    <> constField "title" "Archive"
                    <> customBaseContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls
```

This creates a files `archive.html` which is b uild using the compile function that basically wraps an `Item a` in the `Compiler` monad. The corresponding item is created using `makeItem` that itroduces an empty String Item that is enriched first using the archive template and subsequently the default page template.

Notice the use of `loadAll` that makes the set of all posts availlable inside he compile scope. Most importantly though are the both contexts, especially the `archiveCtx` that makes the posts available to the template engine as a list of `postCtx`s. 

### Contexts

Contexts contain the meta information that is available to the templating engine when building an `Item`. Thus allowing the usage of the computed value in the template files.
A context holds a number of fields which are contexts as well.
Each fields value is created for ever compilation item (this is every item the context will be applied on, usually individual posts).

Take for example the following field definition:

``` haskell
peekField
    :: String           -- ^ Key to use
    -> Int              -- ^ length to peak
    -> Snapshot         -- ^ Snapshot to load
    -> Context String   -- ^ Resulting context
peekField key length snapshot = field key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    return (peak body)
    
    where peak = T.unpack . T.unwords . take length . T.words . T.pack
```

This is a very simple field once created to serve as my own version of a teaser field. As you can see a `field` is crated from a key and a function mapping an item to a string. In this case the item's body is extracted from a [snapshot](https://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html#snapshots) of the item's content. Then `length` words are taken and returned. (If you are in the actual need of a teaser field I would advice you though to use the [`teaserField`](https://jaspervdj.be/hakyll/reference/Hakyll-Web-Template-Context.html#v:teaserField)).

Yet what this example demonstate is the integral importance of `Item`s in Hakyll.

#### Items

`Item`s are a simple datatypes that wrap a `body` of some type `a` (usually `String`) and an identifier. 

Most of Hakyll's functions that deal with content are working with `Item`s. As seen above `Item`s can also be stored in snapshots and retrieved later on.
Especially in `field`s `Item`s contain the content from which the desired `field` can be derived from. 


------
I dont mean to write an extensive documentation of all the concepts in Hakyll, an complete introductory tutorial as well as links to  other peoples takes can be found at Jasper's (the founder of Hakyll) [website](https://jaspervdj.be/hakyll/tutorials.html).


In a follow up article I would like to share a compilation of custom fields that I created or adapted from other blogs.
