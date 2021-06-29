---

title: Approaching nix flakes
subtitle: How to make a flake, **not** from scratch
date: 2021-06-30
tags:
description: Nix flakes are (will be) an importing building block of the wider nix ecosystem. Yet, there is little information about how to provide existing software using flakes, or crucially using nix. This article tries to give some answers to those questions.
image: https://unsplash.com/photos/P8PSZCtpxvU/download?w=1100
status: published
image-credits: |
    Image taken by <a style="background-color:black;color:white;text-decoration:none;padding:4px 6px;font-family:-apple-system, BlinkMacSystemFont, &quot;San Francisco&quot;, &quot;Helvetica Neue&quot;, Helvetica, Ubuntu, Roboto, Noto, &quot;Segoe UI&quot;, Arial, sans-serif;font-size:12px;font-weight:bold;line-height:1.2;display:inline-block;border-radius:3px" href="https://unsplash.com/photos/P8PSZCtpxvU" target="_blank" rel="noopener noreferrer" title="Download free do whatever you want high-resolution photos from Tijana Drndarski"><span style="display:inline-block;padding:2px 3px"><svg xmlns="http://www.w3.org/2000/svg" style="height:12px;width:auto;position:relative;vertical-align:middle;top:-2px;fill:white" viewBox="0 0 32 32"><title>unsplash-logo</title><path d="M10 9V0h12v9H10zm12 5h10v18H0V14h10v9h12v-9z"></path></svg></span><span style="display:inline-block;padding:2px 3px">Tijana Drndarski</span></a> on Unsplash

...



# Introduction

Nix Flakes are getting more and more popular. On GitHub alone more than 2000 repositories are providing a flake definition of some sort  <!--provide link--> . Although still an RFC<!--link-->, the nix foundation seeks to set the foundation for greater adaption. Some work is concerned with making nix flakes more discoverable. More precisely, I am working on bringing flake support to nixos-search<!--link-->. Yet this post is about something else more foundational! This year The first *Summer of Nix* is supposed to start in August, and will produce a list of new flakes implementing open source projects funded by the European Commission through NLnet and the NGI-0 initiative.<!--links, probably-->

I am working as a mentor for one group of participants in this project. Being a packager of NGI-0 packages before, and on boarding the first participants one issue became apparent: There is documentation about what are flakes and how to write them, yet there is little information about how to approach packaging more complicated, existing and critically, not nix based software?

:::{.note header="Defining Scope"}

In this article I want to speak mostly about how to prepare for and approach packaging, both for SoN participants and the broader community. I am aware that there are many pitfalls related to specific ecosystems. These are not part of this article but will be addressed in upcoming ones by me and perhaps other mentors and participants of SoN.

::: 

# Schema

:::{.help header="Proposed process schema"}



:::



# first considerations

When starting to package a new foreign project I would first check a few important things that could give a hint of what needs to be done and what might cause trouble. In the following I present a some pointers of what that might be. I assume the list to be growing over time as well.

## The Readme and installation instructions



## Main language

I believe this one will be somewhat intuitive. After all, knowing what language is packaged allows you get an idea about the state of tools that nix offers to help you. To give ypu a glimpse:

`package.json` (JavaScript)
    ~ Javascript projects either for the client side of things or in terms of a nodeJS project are a classical occurrence. There is decent tooling for those through [node2nix](https://github.com/svanderburg/node2nix). Node2nix can be used to generate a nix package for the the project or at least its dependencies from the `package.json` file.
    ~ Yet because JavaScript tends to be very dynamic, people often end up with non standard solutions or some that  make heavy use of build scripts. While this can be fine, one should closely inspect how the default build process works. Usually, build steps are defined in the `package.json` file under `"scripts.build"` which is the command that would be executed by `npm` when running `npm build`. There, sometimes external scripts are called which might require additional dependencies not considered by node2nix.
    ~ Speaking of node2nix, depending on the project, you might need to tweak the node version used as the default node version to date is node v12.
    ~ There is also [`yarn2nix`](https://github.com/Profpatsch/yarn2nix) for projects using yarn (yarn *workspaces* are sadly not supported yet)

Python
    ~ Python projects can make use of different tools depending of the python environment used. There is [`mach-nix`](https://github.com/DavHau/mach-nix) whcih serves the same purpose as node2nix in the JS world
    ~ for projects using [poetry](https://python-poetry.org/) there is [poetry2nix](https://github.com/nix-community/poetry2nix) making use of the greater version pinning abilities offered by the project.
    ~ Note that *`nixpkgs.python` resolves to  Python **2***^[https://discourse.nixos.org/t/should-python-point-to-python3/10096]

Haskell
    ~ Haskell is one of the best supported environments in nix. It has tight integration with Stack and Cabal and can build projects from either of those prject definition files natively using [`cabal2nix`](https://github.com/NixOS/cabal2nix) and the included `pkgs.haskellPackages.callCabal2nix`/`pkgs.haskellPackages.callStack2nix`
    - IOHK has also developed a complete but separate haskell stack for nix: [haskell.nix](https://github.com/input-output-hk/haskell.nix)
    ~ References:
        - https://bytes.zone/posts/callcabal2nix/
        - https://www.srid.ca/haskell-nix

Rust
    ~ Rust packages, are as well very well supported. This is also due to the excellent cargo package manager used by rust. Unsurprisingly, there is a conversion tool [`crate2nix`](https://github.com/kolloch/crate2nix)
    ~ Be aware about system dependencies that are not picked up automatically:
        - `openssl` will needed to be added to as a `buildInput` for many projects
        - macos some additional frameworks may be needed. The more regularly missing ones are: `libiconv`, `darwin.apple_sdk.frameworks.SystemConfiguration`, `darwin.apple_sdk.frameworks.CoreServices` and `darwin.apple_sdk.frameworks.Security`

`Makefile` (C/C++)
    ~ Plain C project might be almost the easiest to package. In the optimal case you will be done referencing the source, as the builder will actually call the GNU build machinery by default. You will still need to add all dependencies as `buildInputs`.
    ~ Finding out which libraries are required can be a tedious task. See below for some pointers to approach this task.


### What to flake

## Types of artifacts

:::{.help header="Proposed hierarchy"}


:::

## Pure builds

## Dependencies

# build systems / tools

## Dependencies (again)

## reimplementing in nix / splitting





# pitfalls

Mainly own experiences:

## *yarn* workspaces



# Nix dev tools

## nix develop / nix-shell





# Language specifics (teaser)

## Haskell package versions

- jailbreak



## Python packages



## NodeJS

## GNU C/C++ 
