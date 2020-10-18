# fast-tldr

A fast client for tldr.

We strive to be the fastest tldr-client with no compromises regarding features.

This project is a fork of [tldr-hs](https://github.com/psibi/tldr-hs/).

## Installation

See Github releases: https://github.com/gutjuri/fast-tldr/releases

Or

1. [Install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. `git clone https://github.com/gutjuri/fast-tldr.git && cd fast-tldr`
2. `stack install`

## Usage

``` shellsession
$ tldr --help
Usage:
        tldr [options] COMMAND
        tldr [options]
Options:
-h --help                  Show help
-u --update                Update local cache
-L --language <lang>       Use <lang> instead of english
-p --platform <platform>   Use <platform> instead of the native platform
-a --auto-update-interval <days>
            Perform an automatic update if the cache is older than <days>
```

Or a much better example of the usage:

``` shellsession
$ tldr tldr
== tldr ==

Displays simple help pages for command-line tools, from the tldr-pages project.

More information: <https://tldr.sh>.

Get typical usages of a command (hint: this is how you got here!):
        tldr {{command}}

Show the tar tldr page for Linux:
        tldr -p {{linux}} {{tar}}

Get help for a git subcommand:
        tldr {{git-checkout}}

Update local pages (if the client supports caching):
        tldr -u
```

On the first run, this programme caches all available tldr pages. 
Since the number of available tldr pages rises quickly, it is recommended to regularly update the cache. 
Such an update can be run manually with `tldr --update`. 
Users of this client can enable automatic updates by running it with the option `--auto-update-interval DAYS` specified.
The client will then check whether the cached version of the tldr pages is older than `DAYS` days and perform an update in that case.
To enable this functionality permanently, users can put the line `alias tldr="tldr --auto-update-interval DAYS"` in their shell configuration file (e.g. `.bashrc`, `.zshrc`) with the desired update interval specified.
