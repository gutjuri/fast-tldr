# fast-tldr

A fast client for tldr.

We strive to be the fastest tldr-client with no compromises regarding features.

This project is a fork of [tldr-hs](https://github.com/psibi/tldr-hs/).

## Features

- Cache pages
- Configurable platform and language
- 

## Installation

See Github releases: https://github.com/gutjuri/fast-tldr/releases

Or

1. [Install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. `git clone https://github.com/gutjuri/fast-tldr.git && cd fast-tldr`
2. `stack install` or `stack install --local-bin-path .` 

## Usage

``` shellsession
$ tldr --help
Usage:
        tldr [options] COMMAND
        tldr [options]
Options:
-h --help                  Show help
-v,--version               Show version
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

## Benchmarks

I compared fast-tldr to a few other implementations using the `benchmark.sh` script. These are my results:

```shellsession
$ ./benchmark.sh
Benchmark #1: tldr tar
  Time (mean ± σ):      48.7 ms ±   8.2 ms    [User: 3.9 ms, System: 3.6 ms]
  Range (min … max):    38.3 ms …  77.0 ms    23 runs
 
Benchmark #2: ./tealdeer tar
  Time (mean ± σ):      51.3 ms ±   7.9 ms    [User: 6.3 ms, System: 4.8 ms]
  Range (min … max):    43.3 ms …  74.3 ms    20 runs
 
Benchmark #3: ./tldr-c-client tar
  Time (mean ± σ):      95.6 ms ±  12.9 ms    [User: 6.6 ms, System: 11.3 ms]
  Range (min … max):    80.5 ms … 131.9 ms    15 runs
 
Benchmark #4: ./tldr-bash-client tar
  Time (mean ± σ):      97.9 ms ±   8.2 ms    [User: 29.6 ms, System: 12.3 ms]
  Range (min … max):    80.3 ms … 108.3 ms    17 runs
 
Benchmark #5: ./tldr-go-client tar
  Time (mean ± σ):     141.8 ms ±   6.9 ms    [User: 97.4 ms, System: 16.3 ms]
  Range (min … max):   133.2 ms … 156.0 ms    13 runs
 
Summary
  'tldr tar' ran
    1.05 ± 0.24 times faster than './tealdeer tar'
    1.96 ± 0.42 times faster than './tldr-c-client tar'
    2.01 ± 0.38 times faster than './tldr-bash-client tar'
    2.91 ± 0.51 times faster than './tldr-go-client tar'
```

Compared clients:
- [fast-tldr](https://github.com/gutjuri/fast-tldr/)
- [Tealdeer](https://github.com/dbrgn/tealdeer/)
- [C client](https://github.com/tldr-pages/tldr-cpp-client)
- [Bash client](https://github.com/pepa65/tldr-bash-client)
- [Go client by k3mist](https://github.com/k3mist/tldr/)

As we can see, fast-tldr is the fastest client (although not by a large margin).