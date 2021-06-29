---
title: Building a reproducible blog with Nix
subtitle: How I use Nix to manage the development and CI of this blog
tags: internal, technical, nix, haskell
description: Nix is a purely functional package manager that allows isolated development environments and builds. This blog uses it to provide a development environment and to build the blog in a GitHub Action
image: https://images.unsplash.com/photo-1565058290014-df9bd491a29c
status: published
image-credits: |
    Photo by <a style="background-color:black;color:white;text-decoration:none;padding:4px 6px;font-family:-apple-system, BlinkMacSystemFont, &quot;San Francisco&quot;, &quot;Helvetica Neue&quot;, Helvetica, Ubuntu, Roboto, Noto, &quot;Segoe UI&quot;, Arial, sans-serif;font-size:12px;font-weight:bold;line-height:1.2;display:inline-block;border-radius:3px" href="https://unsplash.com/@the_roaming_platypus?utm_medium=referral&amp;utm_campaign=photographer-credit&amp;utm_content=creditBadge" target="_blank" rel="noopener noreferrer" title="Download free do whatever you want high-resolution photos from timJ"><span style="display:inline-block;padding:2px 3px"><svg xmlns="http://www.w3.org/2000/svg" style="height:12px;width:auto;position:relative;vertical-align:middle;top:-2px;fill:white" viewBox="0 0 32 32"><title>unsplash-logo</title><path d="M10 9V0h12v9H10zm12 5h10v18H0V14h10v9h12v-9z"></path></svg></span><span style="display:inline-block;padding:2px 3px">timJ</span></a> on Unsplash
...

# Building a reproducible blog with Nix

This blog is hosted by [GitHub Pages](https://pages.github.com/). GitHub Pages, aside static HTML content, only supports building Jekyll pages natively. At some point a website like this one has to be **generated**, i.e. turned into static HTML content. This means Markdown has to be converted to HTML static pages (like the [homepage](/)), an [archive](/archive.html) has to be created, and [RSS](/rss.xml) and [ATOM](/atom.xml) feeds have to be produced. In this blog's case, this is done by a Haskell DSL called [Hakyll](https://jaspervdj.be/hakyll/). More details about that can be found in [prior articles](2019-12-31-built-with-hakyll-part-1.html).

In order to keep everything in the same place, I use [GitHub Actions](https://github.com/features/actions) as a mean to automatically build and publish this blog whenever I push updates or merge pull requests.

**Was this not an article about Nix?**

Yes indeed it is. In order to create reproducible builds of this blog I employ the [Nix](https://nixos.org/nix) package manager. But in order...

:::{.info}
Jump to [Nix on this blog](#nix-on-this-blog) if you know nix already.
:::

## What is Nix?

Nix is sort of a hybrid tool between a package manager and a build system. The Nix homepage states:

> Nix is a powerful package manager for Linux and other Unix systems that makes package management reliable and reproducible. It provides atomic upgrades and rollbacks, side-by-side installation of multiple versions of a package, multi-user package management and easy setup of build environments.

Sounds... good? What does all of that mean though?

Nix (the package manager) is built around the functional programming language `nix` (what a coincidence, ha?). A program, library, etc. is built by Nix as the output of a pure function. Runtime/buildtime dependencies go in mix with the sources of the package, are built in an isolated space, and output an executable or whatever you want to build. Such a function is called a **Derivation**.

Everything that should be built with Nix has to be described as such a function. In fact, the whole set of packages that is there is just [a giant collection](https://github.com/nixos/nixpkgs) of functions, each defined in their own files.

The isolated manner in which the packages are built also shows in the way they are managed. All packages are installed as such:

```
/nix/store/6fgjp1wsv1w44f890f6kvwywjnc32svr-zoxide-0.2.1/bin/zoxide
```

Lets take this apart:


`/nix/store/`
:   This is the nix store all packages are contained in this folder

`6fgjp1wsv1w44f890f6kvwywjnc32svr`
:   This is a hash of the producing function, it takes into account the function itself, and its inputs
:   It also easens binary caching.

`zoxide`
:   the package's name

`0.2.1`
:   the packaged version

`/bin/zoxide`
:   the actual executable
:   actually the file structure of packages is similar to what you would normally find in places like `/usr` or `~/.local/`


Yet, while that package might exists in the store, this does not mean it is *installed* or usable. --- Huh?

You see, the hash we have seen right now is there for a reason. It allows to have multiple versions of one and the same program at the same time, even with the same version. Normally all packages are built according to one specific state of the `nixpkgs` repo. `.nix` files get evaluated by writing a build instruction file (`.drv`). This resolves all dependencies (building them if they do not exist yet), prepares the package's source and combines those into a hash. If one instead builds it with a newer version where some dependency was updated or changed, not only this derivation hash changes, but it also happens to change the target derivation's hash, forcing it to be rebuilt using the new dependency. The same happens if sources change. As a result, many different versions of a package can coexist in the store.

:::{.note header=""}

As nix functions are pure, **wherever** one uses the same `nixpkgs` instance, the exact same inputs are used to create the exact same output. Also, it makes little to no difference on which machine the package is built. The binary will be the same(ish).

Actually, there is a [project](https://r13y.com/) tracking how many builds (of a vital subset of nix) are reproducible on two different machines.
:::

### How to install/use packages?

#### Global installation

There are different ways to achive this. Using nix locally, the easiest but least idiomatic would be:

```sh
$ nix-env -iA zoxide
```

This installs `zoxide`^[[`zoxide`](https://github.com/ajeetdsouza/zoxide) (a great (fast) replacement for `z`]) globally for the current user, by linking all of zoxides contents to a special directory (`~/.nix-profile/`). The `bin` contents of this directory are on the `$PATH` making the package's binary available to the user.

As mentioned this is not idiomatic Nix, which focuses on declerativity. If you want something installed, consider using [`home-manager`](https://github.com/rycee/home-manager) or add packages to your `configuration.nix` if you are on [NixOS](https://nixos.org/nixos).

#### Local usage

If you need a program but do not want to install it because you don't need it often and you don't want to clutter your `$PATH`, or it's a project specific tool you can make use of either `nix run` or `nix-shell`.

```sh
$ nix run nixpkgs.git
```

This will drop you into a Bash shell with git available.

```sh
$ nix-shell -p git
```

This will essentially do the same but can do much more.

- `nix-shell` accepts multiple packages

- can be used as a shebang ([see](https://gist.github.com/travisbhartwell/f972aab227306edfcfea))

- can drop you into development environments of a specific derivation ([see](https://nixos.org/nixos/nix-pills/developing-with-nix-shell.html))

- can be used to make development environments for a project.

Let's focus on the last point for this article.


## Nix on this blog

This blog uses a shell environment in which I have all the tools I need at hand. Let's look at the structure file by file.

### `default.nix`

By default, Nix commands such as `nix-build` read the [`default.nix`](https://github.com/ysndr/blog/default.nix) file in the directory they are executed from. In my projects, I use them to define and export everything that might be useful. This might include build tools, language environments, scripts or shells.

Generally my `default.nix` file looks like this:

```nix
{pkgs ? import (if pin == false then <nixpkgs> else pin) {},
 pin ? ./nixpkgs.nix, ... }:
with pkgs;
let

<some packages and configuration>

shell = mkShell {
​    name = "<name>";
​    buildInputs = [ <packages I want in my shell> ];
​    shellHook = '' # shell command to be executed when I enter the shell
​    '';
}
in
{
​    inherit shell;
​    inherit packageA, packageB, ...;
}
```

This exposes a shell environment for `nix-shell` and programs to be executed directy using `nix-build`.

So what about this blog's `default.nix`?

```nix
let
  all-hies = import (builtins.fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
{pkgs ? import (if pin == false then <nixpkgs> else pin) {},
 pin ? ./nixpkgs.nix, ... }:
with pkgs;

let
```

Aside from the default, I only priorly import `all-hies`, which is a  project that maintains the [Haskell Ide Engine](https://github.com/infinisil/all-hies) on Nix.

```nix
# -------------- Utils -------------

nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
​    pkgs=pkgs;
};

script = {...} @ args: nur.repos.ysndr.lib.wrap ({
  shell = true;
} // args);
```

This are utilities I might use. [NUR](https://github.com/nix-community/NUR) is the nix version of arch's AUR, albeit not nearly as active sadly. From my own collection, I use the `shell` script which helps me putting together runnable scripts.

```nix
# ------------- Haskell ------------

hie = all-hies.selection { selector = p: { inherit (p) ghc865; }; };
myHaskellPackages = haskell.packages.ghc865.extend( self: super: { });
```

These lines prepare the Haskell environment I use here. I do not override anything from GHC so this is a bit superflous.

```nix
# ------------ dist ---------------

thirdparty = linkFarm "thirdparty" [
  {
​    name = "uikit";
​    path = (fetchTarball "https://github.com/uikit/uikit/archive/v3.2.4.tar.gz") + "/src";
  }
];
```

I import all third party tools (only uikit currently) into the nix store.

```nix
# ------------- generator -----------

generator = myHaskellPackages.callCabal2nix "Site" ./generator {};

generator-with-thirdparty = generator.overrideAttrs(old: {
  nativeBuildInputs = old.nativeBuildInputs or [] ++ [makeWrapper];
  installPhase = old.installPhase + "\n" + ''
​    wrapProgram $out/bin/generator --set THIRDPARTY ${thirdparty}
  '';
});
```

Then, I define the `generator` as a derivation from its Cabal file and the corresponding "Site" executable key defined in `./generator/`. `generator-with-thirdparty` makes what I imported as thirdparty content available under the `$THIRDPARTY` environment variable.

:::{.note header=""}

`myHaskellPackages.callCabal2nix "Site" ./generator {};` is a great tool to quickly make haskell programms available through Nix.

Similar helpers also exist for Stack.

:::



```nix
# --------------- Commands ----------------

generate-website = script {

  name = "generate-website";
  paths = [generator-with-thirdparty git];
  script = ''
​    generator rebuild
  '';

};
```

`generate-website` is supposed to be a build command which, at this state, only runs `generator rebuild`.

```Nix
# ---------------- Shell ------------------

haskell-env = myHaskellPackages.ghcWithHoogle (
  hp: with hp; [ cabal-install ] ++ generator.buildInputs );

shell = { enable-hie ? false }: mkShell {
  name = "blog-env";
  buildInputs = [
​    # put packages here.

​    # generator
​    haskell-env

​    (lib.optional (enable-hie) hie) # optionally setup hie
  ];
  shellHook = ''

​    export THIRDPARTY="${thirdparty}"

​    export HIE_HOOGLE_DATABASE="${haskell-env}/share/doc/hoogle/default.hoo"
​    export NIX_GHC="${haskell-env}/bin/ghc"
​    export NIX_GHCPKG="${haskell-env}/bin/ghc-pkg"
​    export NIX_GHC_DOCDIR="${haskell-env}/share/doc/ghc/html"
​    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )

  '';
};
```

Finally, I bundle everything together to build an environment with `ghc` and a Hoogle index containing the generators dependencies as well as `cabal-install` **for reasons**.

The shell has this Haskell environment and optionally the `hie` executable exposed. It also sets some exports that hie needs to properly function as well as the `$THIRDPARTY` variable to build the blog:

```nix
in {

  inherit shell generator generate-website ;
  ci = {
​    compile = generate-website;
  };

}
```

All important parts are exported.

### `shell.nix` and `nixpkgs.nix`

The first file is used by nix-shell by default. All it does is call the shell attribute of the `default.nix` and controlling if hie is added, making use of the lazy nature of nix: hie will not get evaluated unless enabled.

```nix
{ pin ? null, enable-hie ? false } @ args:

(import ./default.nix args).shell { inherit enable-hie; }
```

The `nixpkgs.nix` file defines a common snapshot of the nixpkgs repo, and is therefore important to ensure everything works on other machines.

```nix
{}: import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/88d9f776091.tar.gz";
  sha256 = "sha256:0z8a0g69fmbbzi77jhvhwafv73dn5fg3gsr0q828lss6j5qpx995";
}) {}
```

:::{.note header=""}

Snapshots like these also allow rollbacks as they define which versions of dependencies get passed on.

:::

## Nix and GitHub Actions

Nix has some integration with GitHub Actions through [install-nix](https://github.com/marketplace/actions/install-nix), an action that **installs nix** •_•

With nix installed, I run `$(nix-build -A ci.compile --no-out-link)` to make Nix build the blog generator and rebuild the blog's content into `build/site`. This works because `nix-build --no-out-link` will just print the path of the resulting package to `stdout`, which in this case is only an executable script produced by the `script` function above. The next step is to take that content and push it to the deployment branch.

[See more...](https://github.com/ysndr/blog/blob/release/.github/workflows/main.yml)

:::{.note header=""}

I previously did even more with nix but specific GitHub Actions tend to do the job well enough.

:::

## Epilog

I see this has become more of a roundup about Nix. Nix is **huge** though.. and this article does not try to capture everything (obviously). From reading this, I hope you have a rough idea of what nix does and how it was applied here. If you knew Nix already, maybe you found something new or interesting among this pile of words and snippets. If you did not know Nix before, I hope this article was still of interest to you.

:::{.info header=""}

If you are hooked on the idea now, further fine grained introduction and resources are:

- The [Nix-Pills](https://nixos.org/nixos/nix-pills/)
- The **unofficial** [wiki](https://nixos.wiki/wiki/Nix)
- The nixpkgs [repo](https://github.com/nixos/nixpkgs). Trust me, if you plan to try out nix, bookmark this project and use the issue search.

:::

**Thank you for getting here!** I hope you enjoyed it and have learned something. If you have questions, improvements, or any other comment, do not hesitate to get in touch on [GitHub](https://github.com/ysndr/blog/issues/new).
