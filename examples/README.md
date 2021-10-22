# Examples

A directory with examples of how to structure your /srv/machine-check directory.

To use these examples do the following:
```
sudo mkdir -p /srv/machine-check
sudo chown youruser.youruser /srv/machine-check
ln -s examples/simple/main.rkt /srv/machine-check/main.rkt
```

Then run:
```
$ machine-check  # or ./main.rkt
--------------------
FAILURE
name:       check-false
location:   machine-check/machine-check.rkt:31:5
params:     '(#t)
message:    "Package 'somenotinstalledpackage' was not found installed"
```
