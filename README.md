# machine-check

With machine-check you can write unit tests in Racket to verify if the state of your machine is what you expect.

# Description

If you use configuration management tools you might want to check if the state of your machine is exactly as you expected it after applying the state. For example after a Puppet, SaltStack or Ansible run it might be wise to check if you actually have the packages installed that you wanted to enforce, and if the files you templated exist in the places and with the content you expected.

While very minimal, this project aspires to provide similar functionality to what [pytest-testinfra](https://github.com/pytest-dev/pytest-testinfra) does in Python and [serverspec](https://serverspec.org/) does in Ruby.

# Usage

See the `examples` dir for an example of how to use this project. It comes down to placing your Racket unit tests in `machine-check/checks-to-perform` and running `machine-check`.

If you want a quick copy-paste-able example:
```
git clone https://github.com/vdloo/machine-check
cd machine-check
raco pkg install --deps search-auto
cp examples/simple/main.rkt.example checks-to-perform/main.rkt
# And finally, run the program
make run  # or ./main.rkt
```

# Development

To run the unit tests for `machine-check` (for the software, not the system unit tests which the software is for) run:
```
raco pkg install --deps search-auto
make test
```

# Documentation

To generate the documentation run:
```
make docs  # Output in docs/index.html
```

Or see it hosted here on github: [https://vdloo.github.io/machine-check/](https://vdloo.github.io/machine-check/).
