# 

<br>

`DataSpaceR` is an R interface to [the CAVD
DataSpace](https://dataspace.cavd.org), a data sharing and discovery
tool that facilitates exploration of HIV immunological data from
pre-clinical and clinical HIV vaccine studies.

DataSpace provides data for several immunoassays from several studies:

* ADCP
* Binding Antibody Multiplex Assay (BAMA)
* Subject Demographics
* Fc Array
* IFNg Elispot
* Intracellular Cytokine Staining (ICS)
* Microarray Data
* Monoclonal Antibody Pharmacokinetics
* NAB non-standard data
* Neutralizing Antibody Assay (NAb)
* Neutralizing Monoclonal Antibody Assay
* Treatment assignments
* Viral load

This package is intended for use by immunologists, bioinformaticians, and
statisticians in HIV vaccine research, or anyone interested in the
analysis of HIV immunological data across assays, studies, and time.

This package simplifies access to the database by taking advantage of
the standardization of the database to hide all the
[Rlabkey](https://cran.r-project.org/package=Rlabkey) specific code away
from the user, and it allows the users to access the study-specific
datasets via [an object-oriented
paradigm](https://cran.r-project.org/package=R6/readme/README.html).
