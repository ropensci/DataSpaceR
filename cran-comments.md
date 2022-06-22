## Release summary

* This is a new release.
* Two new package features added.

## Test environments

* local R installation, R 4.1.1
* ubuntu 18.04 and macOS (on GitHub Actions), R 4.1.1
* win-builder (devel)

## `R CMD check` results

0 errors | 0 warning | 0 notes

## `devtools::check()` results

0 errors | 1 warning | 0 notes

❯ checking for unstated dependencies in examples ... OK
   WARNING
  ‘qpdf’ is needed for checks on size reduction of PDFs

## `devtools::check_win_devel()` results

These are valid URLs:

```
Found the following (possibly) invalid URLs:
  URL: https://dataspace.cavd.org
    From: DESCRIPTION
          inst/doc/DataSpaceR.html
          README.md
    Status: 401
    Message: Unauthorized
  URL: https://dataspace.cavd.org/
    From: README.md
    Status: 401
    Message: Unauthorized
  URL: https://dataspace.cavd.org/_webdav/static/%40files/documents/dataspacer_cheat_sheet.pdf
    From: README.md
    Status: 401
    Message: Unauthorized
```
