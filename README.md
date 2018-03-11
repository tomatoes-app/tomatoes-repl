# Tomatoes REPL

A REPL to interact with [Tomatoes
API](http://www.tomato.es/pages/api_reference).

## Build and install

Get [stack](https://haskellstack.org) and run:

```sh
$ stack install
```

## Run Tomatoes REPL

Include `$HOME/.local/bin` in your `PATH` and run:

```sh
$ tomatoes-repl
```

or from the project root directory run:

```sh
$ stack exec tomatoes-repl
```

## System requirements

Required executables:

* `mpg123` - to play sounds
* `notify-send` - to send desktop notifications
