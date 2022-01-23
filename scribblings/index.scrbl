#lang scribble/manual

@title{machine-check: Unit test your server configuration with Racket}
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

@subsection{Writing tests}

This section contains some examples of what type of checks you can perform with machine-check. Any @racket[.rkt] file in @racket[checks-to-perform] will be loaded, and each of those files should provide a @racket[perform-<name>-checks] function.

@subsubsection{Testing if packages are installed}

Here are some examples for how to test if certain packages are installed on the machine.

To test if @hyperlink["https://packages.debian.org/search?keywords=xorg"]{xorg} is installed on a Debian system you could create a check in checks-to-perform/packages.rkt. After cloning the repository and cd-ing into the machine-check directory you could do so like:

@codeblock|{
rm -rf checks-to-perform
mkdir checks-to-perform
cat << 'EOF' > checks-to-perform/packages.rkt
#lang racket/base

(require "../machine-check/check-helpers.rkt")
(provide perform-packages-checks)

(define perform-packages-checks
  (λ ()
    (check-package-installed "xorg")))
EOF
}|

Note that it is significant that packages.rkt provides perform-packages-checks. Similarly a file named somethingelse.rkt would have to provide a function named perform-somethingelse-checks.

If we would the run this we'd get:
@codeblock|{
root@devenvdebian:~/machine-check# make run
rm -rf out
rm -rf compiled
rm -rf machine-check/compiled
rm -rf machine-check/test/compiled
racket main.rkt
--------------------
FAILURE
name:       check-false
location:   machine-check/check-helpers.rkt:83:5
params:     '(#t)
message:    "Package 'xorg' was not found installed"
--------------------
.make: *** [Makefile:15: run] Error 1
root@devenvdebian:~/machine-check# echo $?
2
}|

After installing xorg this would look like:

@codeblock|{
root@devenvdebian:~/machine-check# apt-get install xorg
root@devenvdebian:~/machine-check# make run
rm -rf out
rm -rf compiled
rm -rf machine-check/compiled
rm -rf machine-check/test/compiled
racket main.rkt
All tests pass!
root@devenvdebian:~/machine-check# echo $?
0
}|

But imagine that we'd like to also run our test-suite on an Archlinux machine for example when our configuration management is set up to work with multiple distributions. In that case just testing for xorg would not be good enough anymore because on Arch Linux that package has a different name (there it is named xorg-server, not xorg).

For that case we can use @hyperlink["https://github.com/vdloo/detect-os-release"]{detect-os-release}. With that we can conditionally test some package names if the Arch Linux os is detected, and some other on our Debian system.

@codeblock|{
rm -rf checks-to-perform
mkdir checks-to-perform
cat << 'EOF' > checks-to-perform/packages.rkt
#lang racket

(require "../machine-check/check-helpers.rkt")
(require detect-os-release)
(provide perform-packages-checks)

(define perform-packages-checks
  (λ ()
    ; Install desktop packages
    (let ((detected-os (detect-os))
          ; Package names to check on all distros
          (packages-to-check (list
    			      "chromium"
    			      "terminator"
    			      )))
      (check-packages-installed
        (if (equal? detected-os "arch")
          ; Archlinux packages
          (append packages-to-check
    	      (list
    	        "xorg-xinit"
    	        "xorg-server"
    	      ))
          ; Debian packages
          (append packages-to-check
    	      (list
    	        "xinit"
    	        "xorg"
    	      )))))))
EOF
}|

@subsubsection{Testing file mode and content}

If you do a lot of templating in your configuration management it might be smart to test files for their content. Also testing their access mode (permissions) can be convenient.

@codeblock|{
rm -rf checks-to-perform
mkdir checks-to-perform
cat << 'EOF' > checks-to-perform/files.rkt
#lang racket

(require "../machine-check/check-helpers.rkt")
(provide perform-files-checks)

(define perform-files-checks
  (λ ()
    (check-file-contains "/etc/ssh/sshd_config"
      "PasswordAuthentication no")
    ; Test that ssh_config is 644
    (check-file-mode "/etc/ssh/sshd_config" 420)))
EOF
}|

Take note that the permissions are checked using @racket[(file-or-directory-permissions path 'bits)] @hyperlink["https://docs.racket-lang.org/reference/Filesystem.html#%28def._%28%28quote._~23~25kernel%29._file-or-directory-permissions%29%29"]{from racket/file} which does not use the Unix filesystem three-digit octal value you might expect. Instead it is some other kind of platform-specific integer so it might be best to test it out before using it with @racket[check-file-mode].

@codeblock|{
$ stat -c "%a %n" /etc/ssh/sshd_config
644 /etc/ssh/sshd_config
$ racket
> (file-or-directory-permissions "/etc/ssh/sshd_config" 'bits)
420
}|

If the content is not as we expect in our @racket[/etc/ssh/sshd_config] the test would fail to indicate this:
@codeblock|{
$ racket main.rkt 
--------------------
FAILURE
name:       check-true
location:   machine-check/check-helpers.rkt:73:4
params:     '(#f)
message:
  "File /etc/ssh/sshd_config did not contain 'PasswordAuthentication no' like we expected"
--------------------
--------------------
FAILURE
name:       check-equal?
location:   machine-check/check-helpers.rkt:57:11
actual:     439
expected:   420
--------------------
$ echo $?
1
}|

@subsubsection{Conditional tests}

If you install the @racket[machine-check] test files in @racket[checks-to-perform] as part of you configuration management as well it might be nice to have only certain checks run on systems with a specific role. For example, if your configuration management installs different packages on different servers you do not want to test for those packages on the servers where they should not be installed. This could be solved by templating the @racket[checks-to-perform] files in the configuration management, but an alternative approach could be to do this based on a file on disk.

In SaltStack it is possible to see which states (roles) have been applied to this server. On the Salt minion you can see this by running:

@verbatim|{
salt-call state.show_states concurrent=true --out json
{
    "local": [
        "base",
        "someotherstate",
    ]
}
}|

We can output this to a file as part of our configuration management run so that later @racket[machine-check] can access this information:

@verbatim|{
salt-call state.show_states concurrent=true --out json | jq -r '.local | .[]' > /srv/applied_states
}|

Then in a check we can decide to skip testing of a certain package is installed if this server has not had a specific state applied to it.

@codeblock|{
rm -rf checks-to-perform
mkdir checks-to-perform
cat << 'EOF' > checks-to-perform/aptrepo.rkt
#lang racket

(require "../machine-check/check-helpers.rkt")
(provide perform-aptrepo-checks)

(define applied-states (file->lines "/srv/applied_states"))

(define perform-aptrepo-checks
  (if (member "aptrepo" applied-states)
    (λ ()
      (check-packages-installed
        (list "reprepro")))
    void))
EOF
}|

Then if the role has not been applied to that server those tests will be skipped:
@codeblock|{
$ grep aptrepo /srv/applied_states
$ racket main.rkt
All tests pass!
$ echo aptrepo >> /srv/applied_states
$ racket main.rkt 
--------------------
FAILURE
name:       check-false
location:   machine-check/check-helpers.rkt:83:5
params:     '(#t)
message:    "Package 'reprepro' was not found installed"
--------------------
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
