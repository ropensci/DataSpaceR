DataSpaceR
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/FredHutch/DataSpaceR.svg?branch=master)](https://travis-ci.org/FredHutch/DataSpaceR) [![Build status](https://ci.appveyor.com/api/projects/status/bmwyv5i32xr07bdr/branch/master?svg=true)](https://ci.appveyor.com/project/juyeongkim/dataspacer/branch/master) [![codecov](https://codecov.io/gh/FredHutch/DataSpaceR/branch/master/graph/badge.svg)](https://codecov.io/gh/FredHutch/DataSpaceR/branch/master)

A thin wrapper around [Rlabkey](https://cran.r-project.org/web/packages/Rlabkey/index.html) to access the [CAVD DataSpace](https://dataspace.cavd.org) database from R. This package simplifies access to the database for R programmers.

It takes advantage of the standardization of the database to hide all the [Rlabkey](https://cran.r-project.org/web/packages/Rlabkey/index.html) specific code away from the user. Study-specific datasets can be accessed via an object-oriented paradigm.

Installation
------------

The package can be downloaded here and installed like any other R packages or installed directly from github using [devtools](https://cran.r-project.org/web/packages/devtools/index.html) or [drat](https://cran.r-project.org/web/packages/drat/index.html).

### using `devtools` (development):

<https://github.com/FredHutch/DataSpaceR>

``` r
# install.packages("devtools")
devtools::install_github("FredHutch/DataSpaceR")
```

### using `drat` (stable):

<https://github.com/FredHutch/drat>

``` r
# install.packages("drat")
drat::addRepo("fredhutch")
install.packages("DataSpaceR")
```

Set netrc file
--------------

The database is accessed with the user's credentials. A `netrc` file storing login and password information is required.

Create netrc file in the computer running R.

-   On a UNIX system this file should be named `.netrc` (**dot**`netrc`)
-   On windows it sould be named `_netrc` (**underscore**`netrc`).
-   The file should be located in the users home directory and the permissions on the file should be unreadable for everybody except the owner. To determine home directory, run `Sys.getenv("HOME")` in R.

The following three lines must be included in the `.netrc` or `_netrc` file either separated by white space (spaces, tabs, or newlines) or commas.

    machine dataspace.cavd.org
    login myuser@domain.com
    password supersecretpassword

Multiple such blocks can exist in one file.

See [here](https://www.labkey.org/wiki/home/Documentation/page.view?name=netrc) for more information about `netrc`.

Usage
-----

The general idea is that the user creates an instance of an `DataSpaceConnection` class. The instance configures itself to connect to a specific study, and datasets can be retrieved by name.

### for example:

``` r
library(DataSpaceR)
study <- connectDS("cvd408")
study
#> DataSpaceR Connection to cvd408
#> URL: https://dataspace.cavd.org/CAVD/cvd408
#> User: jkim2345@fredhutch.org
#> Available datasets:
#>  Demographics
#>  NAb
class(study)
#> [1] "DataSpaceConnection" "R6"
```

`connectDS("cvd408")` will create an instance of `cvd408`. The user needs credentials stored in a `netrc` file to access the database.

### datasets can be listed by:

``` r
study$availableDatasets
#>           name                 label   n
#> 1 Demographics          Demographics  20
#> 2          NAb Neutralizing antibody 839
```

which will print names of available datasets.

### neutralizing antibody dataset (`NAb`) can be retreived by:

``` r
NAb <- study$getDataset("NAb")
dim(NAb)
#> [1] 839  28
colnames(NAb)
#>  [1] "ParticipantId"          "ParticipantVisit/Visit"
#>  [3] "visit_day"              "assay_identifier"      
#>  [5] "summary_level"          "specimen_type"         
#>  [7] "antigen"                "antigen_type"          
#>  [9] "virus"                  "virus_type"            
#> [11] "virus_insert_name"      "clade"                 
#> [13] "neutralization_tier"    "tier_clade_virus"      
#> [15] "target_cell"            "initial_dilution"      
#> [17] "titer_ic50"             "titer_ic80"            
#> [19] "response_call"          "nab_lab_source_key"    
#> [21] "lab_code"               "exp_assayid"           
#> [23] "titer_ID50"             "titer_ID80"            
#> [25] "nab_response_ID50"      "nab_response_ID80"     
#> [27] "slope"                  "vaccine_matched"
```

The package uses a [R6](https://cran.r-project.org/web/packages/R6/index.html) class to represent the connection to a study and get around some of R's copy-on-change behaviour.
