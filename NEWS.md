# DataSpaceR 0.6.3

* Prepare for CRAN submission

# DataSpaceR 0.6.2

* Write the netrc in temporary directory as default in `writeNetrc`. (CRAN requirement)

# DataSpaceR 0.6.1

* Remove LICENSE file for CRAN submission

# DataSpaceR 0.6.0

* rOpenSci submission: https://github.com/ropensci/software-review/issues/261

# DataSpaceR 0.5.2

* Modified `getDataset` method in `DataSpaceStudy` to take `mergeExtra` as an argument in order to merge extra information (demographics and treatment arm). #5

# DataSpaceR 0.5.1

* Adjusted the package to use the latest version of Rlabkey (v2.2) and httr packages.

# DataSpaceR 0.5.0

* Modified `DataSpaceConnection` and `DataSpaceStudy` classes to convert data.frame objects to data.table.
* Added a package startup message on the terms of use.
* Created `getGroup` method in `DataSpaceConnection` class and deprecated `groupId` in `getStudy` method.

# DataSpaceR 0.4.2

* Added `refresh` method for both connection and study classes.
* Added `studyInfo` field in study class.

# DataSpaceR 0.4.1

* Included additional columns (`short_name`, `type`, `status`, `stage`, `species`, `start_date`, `strategy`) in `availableStudies`.
* Updated the introductory vignette.

# DataSpaceR 0.4.0

* Added `availableGroups` field to `DataSpaceConnection` class.
* Modified `getStudy` method in `DataSpaceConnection` to take `groupId` as an argument in order to create a `DataSpaceStudy` object for a particular group.

# DataSpaceR 0.3.0

* Updated `DataSpaceConnection` class and `connectDS` constructor to be not study-specific.

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
