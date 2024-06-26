# A novel approach to transitivity assessment revealed intransitivity to be common: an empirical study on 209 networks of treatments

## Description of the repository

The repository offers the typical structure of separate folders for data, and R (scripts to replicate the main and supplementary Tables and Figures).
* The _data_ folder includes one .RData file: _Overall Dissimilarities_Results_, which is a list of results from 214 datasets (found in [tracenma](https://CRAN.R-project.org/package=tracenma) database) using our proposed approach to transitivity evaluation via the [rnmamod](https://CRAN.R-project.org/package=rnmamod) R package. 
* The _R_ folder includes a script with a collection of necessary self-written functions (function.collection_function.R) to facilitate dataset management and 11 scripts to replicate the main and supplementary Figures and Tables. 
  * _obtain dissimilarities via rnmamod.R_ script calls the [tracenma](https://CRAN.R-project.org/package=tracenma) R package to get the database and applies the proposed approach to transitivity evaluation using the _comp_clustering_ function of the [rnmamod](https://CRAN.R-project.org/package=rnmamod) R package.
  * _function.collection_function.R_ includes functions to (i) capture and remove the
  dose-related characteristics (that were not considered in the article and led to reducing the
  original database from 217 to 214 datasets), (ii) remove characteristics with too many missing
  data based on the _comp_clustering_ function of the [rnmamod](https://CRAN.R-project.org/package=rnmamod) R package, and (iii) identify
  and remove datasets with less than four characteristics after the removal of the
  characteristics above (that led to the __final__ analysis database of 209 datasets).

After downloading/cloning the repo, the user can use the .Rproj file to source all code.

## Output 

Prerequisite R packages: [dplyr](https://CRAN.R-project.org/package=dplyr), 
[ggdist](https://CRAN.R-project.org/package=ggdist),
[ggplot2]( https://CRAN.R-project.org/package=ggplot2),
[ggpubr](https://cran.r-project.org/web/packages/ggpubr/),
[plyr](https://CRAN.R-project.org/package=plyr),
[reshape2](https://CRAN.R-project.org/package=reshape2),
[rnmamod](https://CRAN.R-project.org/package=rnmamod), and
[tracenma](https://CRAN.R-project.org/package=tracenma)

## Important note

Use the __version 1__ of [tracenma](https://CRAN.R-project.org/package=tracenma) (package source:  tracenma_0.1.0.zip) to reproduce the results of the submitted article.
