# hacker_news
Erlang Hacker News Aggregator

[![Hex.pm Version](https://img.shields.io/hexpm/v/hacker_news.svg)](https://hex.pm/packages/hacker_news)

[![Build Status](https://github.com/vkatsuba/hacker_news/workflows/CI/badge.svg)](https://github.com/vkatsuba/hacker_news/actions)

## Contents
* [Goals](#goals)
* [Build & Run](#build--run)
* [Dialyzer](#dialyzer)
* [Xref](#xref)
* [Run Tests](#run-tests)
* [Generate documentation](#generate-documentation)
* [Install hacker_news to project](#install-hacker_news-to-project-rebar3)
* [Support](#support)

# Goals
`hacker_news` aims to provide a simple aggregator for play with [Hacker News API](https://github.com/HackerNews/API).

# Build & Run
```sh
$ git clone https://github.com/vkatsuba/hacker_news.git
$ cd hacker_news
$ wget https://s3.amazonaws.com/rebar3/rebar3
$ chmod u+x ./rebar3
$ ./rebar3 shell
```

# Dialyzer
```sh
$ ./rebar3 dialyzer
```

# Xref
```sh
$ ./rebar3 xref
```

# Run Tests
```sh
$ ./rebar3 ct
```

# Generate documentation
```sh
$ ./rebar3 edoc
```

# Install `hacker_news` to project: [Rebar3](https://www.rebar3.org/)
* Edit file **rebar.config**:
```erlang
{deps, [
    {hacker_news, "0.0.1"},
]}.
```

# Support
v.katsuba.dev@gmail.com
