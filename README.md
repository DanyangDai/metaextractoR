
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
ollama run your_choice_of_models
```

Explore your choice of models: [Ollama](https://ollama.com)

Currently this version of the package only supports local LLMs.

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

### 1. Set up extraction data

Once full-text review has been completed, you should have a list of
articles included for the full-text extraction. At this stage, the user
should have an csv file that contains a list of abstracts. If you are
using [Covidence](https://www.covidence.org), the csv file can be
downloaded from

Based on the pre-defined data extraction element, create variables using
the `add_predefined_vars` function.

Once you have created new columns, check the data and make sure that all
the
