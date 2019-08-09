# PH-drug-related-killings

Code and data to analyze drug-related killings in the Philippines

This project was done by the [Stabile Center at Columbia University](https://journalism.columbia.edu/ms-investigative-specialization) in collaboration with the [Human Rights Data Analysis Group](https://hrdag.org).

These are the calculations for an article published in _The Atlantic_ (**link**). There are three [**tasks**](https://hrdag.org/2016/06/14/the-task-is-a-quantum-of-workflow/), written in [R](https://cran.r-project.org/) using the [tidyverse](https://www.tidyverse.org/packages/) approach, with a special call-out to [`here::here()`](https://cran.r-project.org/package=here). The estimation uses the [LCMCR](https://cran.r-project.org/package=LCMCR) package. Each task can be executed by (e.g.) `cd import && make clean && make`. 

The `import/` directory contains the raw data and initial preparation.

The `lcmcr/` directory does the estimates and generates the graphs.

The `write/` directory combines the estimates and graphs in an [`RMarkdown`](https://rmarkdown.rstudio.com/) document.

<!-- done --> 
