
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DataSpaceR <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R build
status](https://github.com/ropensci/DataSpaceR/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/DataSpaceR/actions)
[![codecov](https://codecov.io/gh/ropensci/DataSpaceR/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/DataSpaceR/branch/master)
[![CRAN
Status](https://www.r-pkg.org/badges/version/DataSpaceR)](https://cran.r-project.org/package=DataSpaceR)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/DataSpaceR)](https://www.rpackages.io/package/DataSpaceR)
[![monthly](https://cranlogs.r-pkg.org/badges/DataSpaceR)](https://www.rpackages.io/package/DataSpaceR)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://badges.ropensci.org/261_status.svg)](https://github.com/ropensci/software-review/issues/261)
<!-- badges: end -->

`DataSpaceR` is an R interface to [the CAVD
DataSpace](https://dataspace.cavd.org), a data sharing and discovery
tool that facilitates exploration of HIV immunological data from
pre-clinical and clinical HIV vaccine studies.

The package is intended for use by immunologists, bioinformaticians, and
statisticians in HIV vaccine research, or anyone interested in the
analysis of HIV immunological data across assays, studies, and time.

This package simplifies access to the database by taking advantage of
the standardization of the database to hide all the
[Rlabkey](https://cran.r-project.org/package=Rlabkey) specific code away
from the user, and it allows the users to access the study-specific
datasets via [an object-oriented
paradigm](https://cran.r-project.org/package=R6/readme/README.html).

## Examples & Documentation

For more detailed examples and detailed documentation, see [the
introductory
vignette](https://docs.ropensci.org/DataSpaceR/articles/DataSpaceR.html)
and [the pkgdown site](https://docs.ropensci.org/DataSpaceR/).

## Installation

Install from CRAN:

``` r
install.packages("DataSpaceR")
```

You can install the latest development version from
[GitHub](https://github.com/ropensci/DataSpaceR) with
[devtools](https://cran.r-project.org/package=devtools):

``` r
# install.packages("devtools")
devtools::install_github("ropensci/DataSpaceR")
```

## Register and set DataSpace credential

The database is accessed with the user’s credentials. A netrc file
storing login and password information is ***required***.

1.  [Create an account](https://dataspace.cavd.org/) and read the terms
    of use
2.  On your R console, create a netrc file using a function from
    `DataSpaceR`:

<!-- end list -->

``` r
library(DataSpaceR)
writeNetrc(
  login = "yourEmail@address.com", 
  password = "yourSecretPassword",
  netrcFile = "/your/home/directory/.netrc" # use getNetrcPath() to get the default path 
)
```

This will create a netrc file in your home directory.

***Alternatively***, you can manually create a netrc file in the
computer running R.

  - On Windows, this file should be named `_netrc`
  - On UNIX, it should be named `.netrc`
  - The file should be located in the user’s home directory, and the
    permissions on the file should be unreadable for everybody except
    the owner
  - To determine home directory, run `Sys.getenv("HOME")` in R

The following three lines must be included in the `.netrc` or `_netrc`
file either separated by white space (spaces, tabs, or newlines) or
commas. Multiple such blocks can exist in one file.

    machine dataspace.cavd.org
    login myuser@domain.com
    password supersecretpassword

See
[here](https://www.labkey.org/wiki/home/Documentation/page.view?name=netrc)
for more information about `netrc`.

## Usage

The general idea is that the user:

1.  creates an instance of `DataSpaceConnection` class via `connectDS`
2.  browses available studies and groups in the instance via
    `availableStudies` and `availableGroups`
3.  creates a connection to a specific study via `getStudy` or a group
    via `getGroup`
4.  retrieves datasets by name via `getDataset`

### for example:

``` r
library(DataSpaceR)
#> By exporting data from the CAVD DataSpace, you agree to be bound by the Terms of Use available on the CAVD DataSpace sign-in page at https://dataspace.cavd.org

con <- connectDS()
con
#> <DataSpaceConnection>
#>   URL: https://dataspace.cavd.org
#>   User: jkim2345@scharp.org
#>   Available studies: 260
#>     - 76 studies with data
#>     - 4994 subjects
#>     - 423195 data points
#>   Available groups: 6
#>   Available publications: 1403
#>     - 1 publications with data
```

`connectDS()` will create a connection to DataSpace.

### available studies can be listed by `availableStudies` field

``` r
knitr::kable(head(con$availableStudies))
```

| study\_name | short\_name                    | title                                                                                     | type               | status   | stage            | species            | start\_date | strategy                             | network | data\_availability                                |
| :---------- | :----------------------------- | :---------------------------------------------------------------------------------------- | :----------------- | :------- | :--------------- | :----------------- | :---------- | :----------------------------------- | :------ | :------------------------------------------------ |
| cvd232      | Parks\_RV\_232                 | Limiting Dose Vaginal SIVmac239 Challenge of RhCMV-SIV vaccinated Indian rhesus macaques. | Pre-Clinical NHP   | Inactive | Assays Completed | Rhesus macaque     | 2009-11-24  | Vector vaccines (viral or bacterial) | CAVD    | NA                                                |
| cvd234      | Zolla-Pazner\_Mab\_test1 Study | Zolla-Pazner\_Mab\_Test1                                                                  | Antibody Screening | Inactive | Assays Completed | Non-Organism Study | 2009-02-03  | Prophylactic neutralizing Ab         | CAVD    | NA                                                |
| cvd235      | mAbs potency                   | Weiss mAbs potency                                                                        | Antibody Screening | Inactive | Assays Completed | Non-Organism Study | 2008-08-21  | Prophylactic neutralizing Ab         | CAVD    | NA                                                |
| cvd236      | neutralization assays          | neutralization assays                                                                     | Antibody Screening | Active   | In Progress      | Non-Organism Study | 2009-02-03  | Prophylactic neutralizing Ab         | CAVD    | NA                                                |
| cvd238      | Gallo\_PA\_238                 | HIV-1 neutralization responses in chronically infected individuals                        | Antibody Screening | Inactive | Assays Completed | Non-Organism Study | 2009-01-08  | Prophylactic neutralizing Ab         | CAVD    | NA                                                |
| cvd239      | CAVIMC-015                     | Lehner\_Thorstensson\_Allovac                                                             | Pre-Clinical NHP   | Inactive | Assays Completed | Rhesus macaque     | 2009-01-08  | Protein & peptide vaccines           | CAVD    | This study has assay data (NAB) in the DataSpace. |

### available groups can be listed by `availableGroups` field

``` r
knitr::kable(con$availableGroups)
```

|  id | label                              | original\_label                    | description                                                                                                               | created\_by | shared |   n | studies                        |
| --: | :--------------------------------- | :--------------------------------- | :------------------------------------------------------------------------------------------------------------------------ | :---------- | :----- | --: | :----------------------------- |
| 216 | mice                               | mice                               | NA                                                                                                                        | readjk      | FALSE  |  75 | cvd468, cvd483, cvd316, cvd331 |
| 217 | CAVD 242                           | CAVD 242                           | This is a fake group for CAVD 242                                                                                         | readjk      | FALSE  |  30 | cvd242                         |
| 220 | NYVAC durability comparison        | NYVAC\_durability                  | Compare durability in 4 NHP studies using NYVAC-C (vP2010) and NYVAC-KC-gp140 (ZM96) products.                            | ehenrich    | TRUE   |  78 | cvd281, cvd434, cvd259, cvd277 |
| 224 | cvd338                             | cvd338                             | NA                                                                                                                        | readjk      | FALSE  |  36 | cvd338                         |
| 228 | HVTN 505 case control subjects     | HVTN 505 case control subjects     | Participants from HVTN 505 included in the case-control analysis                                                          | drienna     | TRUE   | 189 | vtn505                         |
| 230 | HVTN 505 polyfunctionality vs BAMA | HVTN 505 polyfunctionality vs BAMA | Compares ICS polyfunctionality (CD8+, Any Env) to BAMA mfi-delta (single Env antigen) in the HVTN 505 case control cohort | drienna     | TRUE   | 170 | vtn505                         |

***Note***: A group is a curated collection of participants from
filtering of treatments, products, studies, or species, and it is
created in [the DataSpace
App](https://dataspace.cavd.org/cds/CAVD/app.view).

Check out [the reference
page](https://docs.ropensci.org/DataSpaceR/reference/DataSpaceConnection.html)
of `DataSpaceConnection` for all available fields and methods.

### create an instance of `cvd408`

``` r
cvd408 <- con$getStudy("cvd408")
cvd408
#> <DataSpaceStudy>
#>   Study: cvd408
#>   URL: https://dataspace.cavd.org/CAVD/cvd408
#>   Available datasets:
#>     - BAMA
#>     - Demographics
#>     - ICS
#>     - NAb
#>   Available non-integrated datasets:
class(cvd408)
#> [1] "DataSpaceStudy" "R6"
```

### available datasets can be listed by `availableDatasets` field

``` r
knitr::kable(cvd408$availableDatasets)
```

| name         | label                           |    n | integrated |
| :----------- | :------------------------------ | ---: | :--------- |
| BAMA         | Binding Ab multiplex assay      | 1080 | TRUE       |
| Demographics | Demographics                    |   20 | TRUE       |
| ICS          | Intracellular Cytokine Staining | 3720 | TRUE       |
| NAb          | Neutralizing antibody           |  540 | TRUE       |

which will print names of available datasets.

### Neutralizing Antibody dataset (`NAb`) can be retrieved by:

``` r
NAb <- cvd408$getDataset("NAb")
dim(NAb)
#> [1] 540  29
colnames(NAb)
#>  [1] "ParticipantId"          "ParticipantVisit/Visit" "visit_day"             
#>  [4] "assay_identifier"       "summary_level"          "specimen_type"         
#>  [7] "antigen"                "antigen_type"           "virus"                 
#> [10] "virus_type"             "virus_insert_name"      "clade"                 
#> [13] "neutralization_tier"    "tier_clade_virus"       "target_cell"           
#> [16] "initial_dilution"       "titer_ic50"             "titer_ic80"            
#> [19] "response_call"          "nab_lab_source_key"     "lab_code"              
#> [22] "exp_assayid"            "titer_ID50"             "titer_ID80"            
#> [25] "nab_response_ID50"      "nab_response_ID80"      "slope"                 
#> [28] "vaccine_matched"        "study_prot"
```

Check out [the reference
page](https://docs.ropensci.org/DataSpaceR/reference/DataSpaceStudy.html)
of `DataSpaceStudy` for all available fields and methods.

***Note***: The package uses a
[R6](https://cran.r-project.org/package=R6) class to represent the
connection to a study and get around some of R’s copy-on-change
behavior.

## Meta

  - Please [report any issues or
    bugs](https://github.com/ropensci/DataSpaceR/issues).
  - License: GPL-3
  - Get citation information for `DataSpaceR` in R doing
    `citation(package = 'DataSpaceR')`
  - Please note that this project is released with a [Contributor Code
    of
    Conduct](https://github.com/ropensci/DataSpaceR/blob/master/CONDUCT.md).
    By participating in this project you agree to abide by its terms.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
