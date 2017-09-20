pict-abbrevs
===

Pict helpers.


Install
---

```
$ git clone https://github.com/bennn/pict-abbrevs
$ raco pkg install ./pict-abbrevs
```


raco pict
---

This package installs a "pict" raco command.
The command expects two or more arguments:

- the first argument must be the name of a `pict` function
- the other arguments must be filenames

### Example

To vertically append a few image files:

```
$ raco pict vl-append FILE.png ....
```

