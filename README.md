# tldr

A fast client for tldr.

We strive to be the fastest tldr-client with no compromises regarding features.

This project is a fork of [tldr-hs](https://github.com/psibi/tldr-hs/).

## Installation

See Github releases: https://github.com/gutjuri/fast-tldr/releases

Or

1. [Install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. `stack install tldr`

## Usage

``` shellsession
$ tldr --help
tldr - Simplified and community-driven man pages

Usage: tldr [-v|--version] ((-u|--update) | [-p|--platform PLATFORM]
            [-L|--language LOCALE] COMMAND | (-a|--about))
  tldr Client program

Available options:
  -h,--help                Show this help text
  -v,--version             Show version
  -u,--update              Update offline cache of tldr pages
  -p,--platform PLATFORM   Prioritize specfic platform while searching. Valid
                           values include linux, osx, windows, sunos
  -L,--language LOCALE     Preferred language for the page returned
  COMMAND                  name of the command
  --auto-update-interval DAYS
                           Perform an automatic update if the cache is older
                           than DAYS
```

Or a much better example of the usage:

``` shellsession
$ tldr tldr
tldr
Simplified man pages. More information: https://tldr.sh.

 - Get typical usages of a command (hint: this is how you got here!):
   tldr {{command}}

 - Show the tar tldr page for linux:
   tldr -p {{linux}} {{tar}}

 - Get help for a git subcommand:
   tldr {{git checkout}}
```

On the first run, this programme caches all available tldr pages. 
Since the number of available tldr pages rises quickly, it is recommended to regularly update the cache. 
Such an update can be run manually with `tldr --update`. 
Users of this client can enable automatic updates by running it with the option `--auto-update-interval DAYS` specified.
The client will then check whether the cached version of the tldr pages is older than `DAYS` days and perform an update in that case.
To enable this functionality permanently, users can put the line `alias tldr="tldr --auto-update-interval DAYS"` in their shell configuration file (e.g. `.bashrc`, `.zshrc`) with the desired update interval specified.
