---
authors:
  - "Yannik Sander"
date: 2021-12-02
title: Hooking up with Git
subtitle: Managing git hooks with nix
tags: nix, snippets, 
description: Git hooks can be useful, tracking and managing them with Nix makes removes the barrier of using them.
image: https://images.unsplash.com/photo-1621298516851-047f72b40bd7?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1920
status: published
image-credits: |
    Made by Jamie Matociños on <a hrep=https://unsplash.com/photos/rW00Wu_CeYA>Unsplash</a>:
...

# Git hooks

Git hooks are very useful in theory e.g. to enforce style guidelines of code being pushed or doing arbitrary cleanup/analysis in response to various git events. Yet, if you are using nix, the way git hooks are set up and managed goes against the ideas of Nix. 

The presented approach does not solve the underlying issue of mutability but makes git-hooks more trackable and easily appliable.

# The big picture

Let me present a full-fledged example first, the functions are individually posted down in the [appendix](#appendix).

```nix
let
    installGitHooks = hookTypes:
        let mkHook = type: hooks: {
        hook = pkgs.writeShellScript type
        ''
            for hook in ${pkgs.symlinkJoin { name = "${type}-git-hooks"; paths = hooks; }}/bin/*; do
            $hook
            RESULT=$?
            if [ $RESULT != 0 ]; then
                echo "$hook returned non-zero: $RESULT, abort operation"
            exit $RESULT
            fi
            done
            echo "$INSTALLED_GIT_HOOKS $type"
            exit 0
        '';
        inherit type;
        };

        installHookScript = { type, hook }: ''
            if [[ -e .git/hooks/${type} ]]; then
                echo "Warn: ${type} hook already present, skipping"
            else
                ln -s ${hook} $PWD/.git/hooks/${type}
                INSTALLED_GIT_HOOKS+=(${type})
            fi
        '';
        in

        pkgs.writeShellScriptBin "install-git-hooks" 
        ''
            if [[ ! -d .git ]] || [[ ! -f flake.nix ]]; then
                echo "Invocate \`nix develop\` from the project root directory."
                exit 1
            fi

            if [[ -e .git/hooks/nix-installed-hooks ]]; then
                echo "Hooks already installed, reinstalling"
                ${uninstallGitHooks}/bin/${uninstallGitHooks.name}
            fi

            mkdir -p ./.git/hooks
            
            ${pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList (type: hooks: installHookScript (mkHook type hooks)) hookTypes )}

            echo "Installed git hooks: $INSTALLED_GIT_HOOKS"
            printf "%s\n" "''${INSTALLED_GIT_HOOKS[@]}" > .git/hooks/nix-installed-hooks
        '';

    uninstallGitHooks = pkgs.writeShellScriptBin "uninstall-git-hooks" 
        ''
            if [[ ! -e "$PWD/.git/hooks/nix-installed-hooks" ]]; then
            echo "Error: could find list of installed hooks."
            exit 1
            fi

            while read -r hook
            do
            echo "Uninstalling $hook"
            rm "$PWD/.git/hooks/$hook"
            done < "$PWD/.git/hooks/nix-installed-hooks"

            rm "$PWD/.git/hooks/nix-installed-hooks"
        '';
    
    rustFormatHook = pkgs.writeShellScriptBin "check-rust-format-hook"
        ''
            ${pkgs.rustfmt}/bin/rustfmt --check
            RESULT=$?
            [ $RESULT != 0 ] && echo "Please run \`cargo fmt\` before"
            exit $RESULT
        '';

    hookInstaller =  installGitHooks { pre-commit = [rustFormatHook]; } 
in 
    pkgs.mkShell {
        packages = [ (installGitHooks { pre-commit = [rustFormatHook];) } uninstallGitHooks ];
        inputsFrom = [ ];

        shellHook = ''
            echo "=== Development shell ==="
            echo "Info: Git hooks can be installed using \`install-git-hooks\`"
            # or run `install-git-hooks` automatically
        '';
    };

```

# Explanation

The above code defines functions to build installation and uninstallation commands for git hooks.

To run a hook, create a derivation with the hook (or hooks) for one event located in the `bin/` folder, i.e. using. `writeShellScriptBin` :

```nix
rustFormatHook = pkgs.writeShellScriptBin "check-rust-format-hook" ''
    ${pkgs.rustfmt}/bin/rustfmt --check
'';
```

Then create a `hookInstaller` by adding the derivation to a list of hooks for a specific event type:

```nix
hookInstaller =  installGitHooks { pre-commit = [rustFormatHook]; } 
```

This will run all commands under `\${rustFormatHook}/bin/*` for `pre-commit` events.

The event types can be arbitrary but have to comply with [actual git hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks) to be run.

Finally, add the installer (and optionally the uninstall-command) to your dev shell as input programs. You can choose to automatically run the installer at entrance to the shell as a `shellHook` or manually by the user.

```nix
pkgs.mkShell {
    packages = [ hookInstaller uninstallGitHooks ];
    inputsFrom = [ ];

    shellHook = ''
        echo "=== Development shell ==="
        echo "Info: Git hooks can be installed using \`install-git-hooks\`"
        # or run `install-git-hooks` automatically
    '';
};
```

# Clarification

I am aware that there is [much more advanced tools](https://github.com/cachix/pre-commit-hooks.nix) available with more advanced configuration systems etc. Yet, this approach is nix-native and sufficiently flexible for simple hook setups. It does still require you to write all hooks yourself but this way they can be tracked with nix and in theory make use of programs not even populated to the final environment.

Nonetheless, this might be of interest for someone and if so thanks for reading.

# Appendix

## Installer

```nix
installGitHooks = hookTypes:
    let mkHook = type: hooks: {
    hook = pkgs.writeShellScript type
    ''
        for hook in ${pkgs.symlinkJoin { name = "${type}-git-hooks"; paths = hooks; }}/bin/*; do
        $hook
        RESULT=$?
        if [ $RESULT != 0 ]; then
            echo "$hook returned non-zero: $RESULT, abort operation"
        exit $RESULT
        fi
        done
        echo "$INSTALLED_GIT_HOOKS $type"
        exit 0
    '';
    inherit type;
    };

    installHookScript = { type, hook }: ''
        if [[ -e .git/hooks/${type} ]]; then
            echo "Warn: ${type} hook already present, skipping"
        else
            ln -s ${hook} $PWD/.git/hooks/${type}
            INSTALLED_GIT_HOOKS+=(${type})
        fi
    '';
    in

    pkgs.writeShellScriptBin "install-git-hooks" 
    ''
        if [[ ! -d .git ]] || [[ ! -f flake.nix ]]; then
            echo "Invocate \`nix develop\` from the project root directory."
            exit 1
        fi

        if [[ -e .git/hooks/nix-installed-hooks ]]; then
            echo "Hooks already installed, reinstalling"
            ${uninstallGitHooks}/bin/${uninstallGitHooks.name}
        fi

        mkdir -p ./.git/hooks
        
        ${pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList (type: hooks: installHookScript (mkHook type hooks)) hookTypes )}

        echo "Installed git hooks: $INSTALLED_GIT_HOOKS"
        printf "%s\n" "''${INSTALLED_GIT_HOOKS[@]}" > .git/hooks/nix-installed-hooks
    '';
```

## Uninstaller

```nix
uninstallGitHooks = pkgs.writeShellScriptBin "uninstall-git-hooks" 
    ''
        if [[ ! -e "$PWD/.git/hooks/nix-installed-hooks" ]]; then
        echo "Error: could find list of installed hooks."
        exit 1
        fi

        while read -r hook
        do
        echo "Uninstalling $hook"
        rm "$PWD/.git/hooks/$hook"
        done < "$PWD/.git/hooks/nix-installed-hooks"

        rm "$PWD/.git/hooks/nix-installed-hooks"
    '';
```
