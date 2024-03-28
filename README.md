# A novel approach to transitivity assessment revealed intransitivity to be common: an empirical study on 214 networks of treatments

## Description of the repository

The repository offers the typical structure of separate folders for data, and R (scripts to replicate the main and supplementary Tables and Figures).
* The _data_ folder includes three .RData files: _Overall Dissimilarities_Results_, _TRACE-NMA Dataset_, and _index_reviews_. 
  * _Overall Dissimilarities_Results_ is a list of results from 217 datasets (total eligible from the [nmadb](https://CRAN.R-project.org/package=nmadb) database) using our proposed approach to transitivity evaluation. 
  * _TRACE-NMA Dataset_ is a list of 217 data-frames pertaining to the datasets (total eligible from the [nmadb](https://CRAN.R-project.org/package=nmadb) database) with the extracted study-level aggregate clinical and methodological characteristics. 
  * Lastly, _index_reviews_ is a data-frame with the PMID and other characteristics of the systematic reviews referring to the 217 datasets (total eligible from the [nmadb](https://CRAN.R-project.org/package=nmadb) database). 
* The _R_ folder includes a script with a collection of necessary self-written functions (function.collection_function.R) and 10 scripts to replicate the main and supplementary Figures and Tables. 
  * _function.collection_function.R_ include functions to (i) capture and remove the
  dose-related characteristics (that were not considered in the article and led to reducing the
  analysed database from 217 to 214 datasets), (ii) remove characteristics with too many missing
  data based on the _comp_clustering_ function of the `rnmamod` R package, and (iii) identify
  and remove datasets with less than four characteristics after the removal of the
  characteristics above (that led to the __final__ analysis database of 209 datasets).

After downloading/cloning the repo, the user can use the .Rproj file to source all code.

## Output 

Prerequisite R packages: [dplyr](https://CRAN.R-project.org/package=dplyr), 
[ggdist](https://CRAN.R-project.org/package=ggdist),
[gghalves](https://CRAN.R-project.org/package=gghalves),
[ggplot2]( https://CRAN.R-project.org/package=ggplot2),
[ggpubr](https://cran.r-project.org/web/packages/ggpubr/),
[plyr](https://CRAN.R-project.org/package=plyr),
[reshape2](https://CRAN.R-project.org/package=reshape2), and
[rnmamod](https://CRAN.R-project.org/package=rnmamod).

## Important note

The database can also be found in the `tracenma` R package. Since, `tracenma` will be constantly updated with new datasets and functions, the present repository was set-up for reproducibility purposes regarding the submitted article.