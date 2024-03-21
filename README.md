# A novel approach to transitivity assessment revealed intransitivity to be common: an empirical study on 214 networks of treatments

## Description of the repository

The repository offers the typical structure of separate folders for data, and R (scripts to replicate the main and supplementary Tables and Figures).
* The _data_ folder includes three .RData files: _Overall Dissimilarities_Results_, _TRACE-NMA Dataset_, and _index_reviews_. 
 * _Overall Dissimilarities_Results_ is a list of results from 214 datasets using our proposed approach to transitivity evaluation. 
 * _TRACE-NMA Dataset_ is a list of 214 data-frames pertaining to the analysed datasets with the extracted study-level aggregate clinical and methodological characteristics. 
 * Lastly, _index_reviews_ is a data-frame with the PMID and other characteristics of the systematic reviews referring to the 214 analysed datasets. 
* The _R_ folder includes a script with a collection of necessary self-written functions (function.collection_function.R) and 10 scripts to replicate the main and supplementary Figures and Tables. 

After downloading/cloning the repo, the user can use the .Rproj file to source all code.

To use the functions of our approach, please, load the development version of the __rnmamod__ R package:
```r
remotes::install_github("https://github.com/LoukiaSpin/rnmamod.git", force = TRUE)
```

## Output 
Prerequisite R packages: [rnmamod](https://CRAN.R-project.org/package=rnmamod), 
[ggplot2]( https://CRAN.R-project.org/package=ggplot2),
[reshape2](https://CRAN.R-project.org/package=reshape2),
[ggpubr](https://cran.r-project.org/web/packages/ggpubr/) 
[plyr](https://CRAN.R-project.org/package=plyr) 
[dplyr](https://CRAN.R-project.org/package=dplyr) 
[gghalves](https://CRAN.R-project.org/package=gghalves), and
[ggdist](https://CRAN.R-project.org/package=ggdist) 
