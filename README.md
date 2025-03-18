
<!-- badges: start -->

[![version](http://www.r-pkg.org/badges/version/metaextractoR)](https://CRAN.R-project.org/package=metaextractoR)
[![cranlogs](http://cranlogs.r-pkg.org/badges/metaextractoR)](https://CRAN.R-project.org/package=metaextractoR)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

<!-- badges: end -->

## Installation

To install the package:

``` r
remotes::install_github("danyangdai/metaextractoR")
```

Download and install Large Language Models (LLMs) for local usage
supported by `metaextractoR`. Run the code below in Terminal or Console
to install models:

``` r
ollama run llama3.1
```

``` r
llama run deepseek-r1:671b
```

``` r
llama run medllama2
```

``` r
llama run nuextract
```

``` r
llama run gemma2:2b
```

## Workflow

This package design fit in to the [Preferred Reporting Items for
Systematic reviews and Meta-Analyses
(PRISMA)](https://www.prisma-statement.org/) guideline for systematic
review and meta-analysis.

For systematic review and meta-analysis, users are encouraged to follow
the study protocol developed for the systematic review and
meta-analysis. This package will streamline the data extraction part in
your systematic review with the power of Large Language Models (LLMs)
while keeping a human in the loop for accuracy and oversight.

For documentation: see working paper [working paper]()

### Full-text extraction

Once full-text review has been completed, you should have a list of
articles included for the full-text extraction. Based on the pre-defined
data extraction element, create variables using
