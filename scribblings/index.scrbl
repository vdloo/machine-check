#lang scribble/manual

@title{machine-check: Unit test your server configuration}
@author{Rick van de Loo}

@section{Introduction}

With machine-check you can write unit tests in Racket to verify if the state of your machine is what you expect.

See the code at @hyperlink["https://github.com/vdloo/machine-check"]{on github}.

@section{Description}

If you use configuration management tools you might want to check if the state of your machine is exactly as you expected it after applying the state. For example after a Puppet, SaltStack or Ansible run it might be wise to check if you actually have the packages installed that you wanted to enforce, and if the files you templated exist in the places and with the content you expected.

While very minimal, this project aspires to provide similar functionality to what @hyperlink["https://github.com/pytest-dev/pytest-testinfra"]{pytest-testinfra} does in Python and @hyperlink["https://serverspec.org/"]{serverspec} does in Ruby.

@section{Usage}

See the @hyperlink["https://github.com/vdloo/machine-check/tree/master/examples"]{examples dir} for an example of how to use this project. It comes down to placing your Racket unit tests in machine-check/checks-to-perform and running machine-check.

If you want a quick copy-paste-able example:

@verbatim|{
git clone https://github.com/vdloo/machine-check
cd machine-check
raco pkg install --deps search-auto
cp examples/simple/main.rkt.example checks-to-perform/main.rkt
# And finally, run the program
make run  # or ./main.rkt
}|

@section{Development}

To run the unit tests for machine-check (for the software, not the system unit tests which the software is for) run:

@verbatim|{
raco pkg install --deps search-auto
make test
}|

@section{Documentation}

To generate the documentation run:

@verbatim|{
make docs  # Output in docs/index.html
}|
