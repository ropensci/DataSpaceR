
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DataSpaceR <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->
[![R build status](https://github.com/ropensci/DataSpaceR/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/DataSpaceR/actions)
[![codecov](https://codecov.io/gh/ropensci/DataSpaceR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/DataSpaceR/branch/main)
[![CRAN Status](https://www.r-pkg.org/badges/version/DataSpaceR)](https://cran.r-project.org/package=DataSpaceR)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![](https://badges.ropensci.org/261_status.svg)](https://github.com/ropensci/software-review/issues/261)
<!-- badges: end -->

`DataSpaceR` is an R interface to [the CAVD
DataSpace](https://dataspace.cavd.org), a data sharing and discovery
tool that facilitates exploration of HIV immunological data from
pre-clinical and clinical HIV vaccine studies.

This package is intended for use by immunologists, bioinformaticians, and
statisticians in HIV vaccine research, or anyone interested in the
analysis of HIV immunological data across assays, studies, and time.

This package simplifies access to the database by taking advantage of
the standardization of the database to hide all the
[Rlabkey](https://cran.r-project.org/package=Rlabkey) specific code away
from the user, and it allows the users to access the study-specific
datasets via [an object-oriented
paradigm](https://cran.r-project.org/package=R6/readme/README.html).

## Examples & Documentation

For more detailed examples and detailed documentation, see [the pkgdown site](https://docs.ropensci.org/DataSpaceR/).

For a quick guide of how to use the API, see [the cheat
sheet](https://dataspace.cavd.org/_webdav/static/%40files/documents/dataspacer_cheat_sheet.pdf)
.

## Meta

-   Please [report any issues or
    bugs](https://github.com/ropensci/DataSpaceR/issues).
-   License: GPL-3
-   Get citation information for `DataSpaceR` in R doing
    `citation(package = 'DataSpaceR')`
-   Please note that this project is released with a [Contributor Code
    of
    Conduct](https://github.com/ropensci/DataSpaceR/blob/main/CONDUCT.md).
    By participating in this project you agree to abide by its terms.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
