# This allows pinning by passing `--arg pin <pinfile.nix>.`
{ pin ? null, enable-hie ? false } @ args: 
(import ./default.nix args).shell { inherit enable-hie; }
