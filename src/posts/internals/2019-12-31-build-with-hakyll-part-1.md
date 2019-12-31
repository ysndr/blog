---
title: How this page is generated - Part 01 [WIP]
subtitle: Introduction to Hakyll
tags: internal, technical, haskell, wip
description: This site is build with Hakyll, here I explain how.
image: https://images.unsplash.com/photo-1576020363294-ab5dca00b6f8?w=1000
image-credits: |
    Picture taken by <a style="background-color:black;color:white;text-decoration:none;padding:4px 6px;font-family:-apple-system, BlinkMacSystemFont, &quot;San Francisco&quot;, &quot;Helvetica Neue&quot;, Helvetica, Ubuntu, Roboto, Noto, &quot;Segoe UI&quot;, Arial, sans-serif;font-size:12px;font-weight:bold;line-height:1.2;display:inline-block;border-radius:3px" href="https://unsplash.com/@mrsimonfischer?utm_medium=referral&amp;utm_campaign=photographer-credit&amp;utm_content=creditBadge" target="_blank" rel="noopener noreferrer" title="Download free do whatever you want high-resolution photos from Szymon Fischer"><span style="display:inline-block;padding:2px 3px"><svg xmlns="http://www.w3.org/2000/svg" style="height:12px;width:auto;position:relative;vertical-align:middle;top:-2px;fill:white" viewBox="0 0 32 32"><title>unsplash-logo</title><path d="M10 9V0h12v9H10zm12 5h10v18H0V14h10v9h12v-9z"></path></svg></span><span style="display:inline-block;padding:2px 3px">Szymon Fischer</span></a> on Unsplash
...


# Hakyll: The Basics

Hakyll is tool to build static webpages. Hakyll is written in Haskell lending part of its name from it and the other from the renown static site engine [Jekyll](https://jekyllrb.com/). The fundamental difference is htath instead of beeing a standalone program Hakyll is implementented as a library offering a rich DSL to define the contents of a website. The DSL can be used to include static assets, compile pages using templates and even generate whole pages by itsels i.e. RSS and ATOM feeds.
As Hakyll hakyll is written in Haskell combining it with the great text conversion tool [pandoc](https://pandoc.org/) is a low hanging fruit. The combination of both allows the compiliation from several different file formats including (an extended version of) Markdown, [RST](http://docutils.sourceforge.net/docs/ref/rst/introduction.html) and Emacs [Org-Mode](http://orgmode.org/).

# The inner workings of this website

:::{.note}
*I won't go into all the things used in this article, instead there will be individual posts going into more detail about different elements*
:::

- Part 01: Hakyll basic setuo and context *(this article)*
- Part 02: 

## Part 01

### Setup

The heart of this page is [`generator/Site.hs`](https://github.com/ysndr/blog/blob/release/generator/Site.hs).

In general a basic Hakyll generator looks like this

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
```

This will render all markdown files under `posts/` to html files under our build route.
`route $ setExtension "html"` means that the route and basename will be preserved and an html extension will be added.

In this snipped we see several things:

1. matching routes
2. compiling with pandoc
3. templates
4. contexts

An basic introduction to those concepts can be found at Jasper's (the founder of hakyll) [website](https://jaspervdj.be/hakyll/tutorials.html).

The main way to customize things is to add information available to the templates.

### Contexts

Contexts contain the meta information that is available to the templating engine when building an Item.
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

This is a very simple field once created to serve as my own version of a teaser field. As you can see a field is defined by a key and a function mapping an item to a string. In this case the item's body is extracted from a [snapshot](https://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html#snapshots) of the item's content. Then `length` words are taken and returned. (If you are in the actual need of a teaser field I would advice you though to use the [`teaserField`](https://jaspervdj.be/hakyll/reference/Hakyll-Web-Template-Context.html#v:teaserField)).

Another equally simple field is the readTimeField defined as

``` haskell
-- Field that naïvely determines the reading time 
-- by assuming an average of 200 words per minute of reading velocity and 
-- dividing the actual number of words by  this average
readTimeField :: String -> Snapshot -> Context String
readTimeField name snapshot = field name $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    let words = length (T.words . T.pack $ body)
    return $ show $ div words 200
```

------

That's it for this post. We've covered how to compile Markdown files to HTML and how to define simple custom fields.
While writing this artile I got distracted and implemented a table-of-contents field. That one and a few others will be covered in another article.
