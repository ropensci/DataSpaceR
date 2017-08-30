# DataSpaceR 0.2.1

* Modified `getUserEmail()` to get email from netrc file.
* Updated license to GPL-3.

# DataSpaceR 0.2.0

* Added `getVariableInfo` method to `DataSpaceConnection` class.
* Changed variable names in `getAvailableDatasets` to lowercase.
* Added `treatmentArm` field to `DataSpaceConnection` class.
* Removed `getAvailableDatasets` method (now a private method).
* Changed the default connection from staging (`dataspace-staging.cavd.org`) to production (`dataspace.cavd.org`).
* Added an option to connect to the staging server.
* Renamed `write_netrc` and `check_netrc` to `writeNetrc` and `checkNetrc`.

# DataSpaceR 0.1.0

* Initialized the package with `connectDS()` and other basic functions.
* Added a test framework on `tests` to test the package.
* Added `.travis.yml` to build and check the package automatically. (not used yet)
* Added `codecov.yml` to track code coverage. (not used yet)
* Added `README.Rmd` to introduce the package.
* Added `NEWS.md` to track changes to the package.
* Added `_pkgdown.yml` and `/docs` to build a package website.
* Added a introductory vignette called `Intro_to_DataSpaceR.Rmd`.
