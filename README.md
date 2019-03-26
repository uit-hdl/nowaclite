# NOWAC Lite
This is the light version of NOWAC R Package. Here you'll find only basic functions and general information about the package.

# Download, installation and usage
Installing the package using command line tools, e.g. the shell in RStudio.

```
require(devtools)
install_github('nsh23/nowaclite')
```

Using the package:
- Load the package `library(nowaclite)`
- type `?nowaclite` to get more information about available functions

# Data
We use git submodules to keep track of data (in `data/` and `data-raw`).
`data-raw` contains the raw-est form of a datasets we have managed to find. In
NOWAC (almost) all biological datasets have been generated at NTNU, while
questionnaire data are stored on a in-house server. For the biological datasets we have put
the raw output from the machines in the `data-raw` repository, while
analysis-ready `.RData` files are put in the `data` repository.

Note that the contents of the data and data-raw is not publicly available, as well as some R scripts closely connected to the data, so
`data/` and `data-raw` folders will be empty after cloning down the repository and datasets and documentation is not available in nowaclite.

# Structure
- `R/` contains the R source code.
- `data-raw` raw databasets.  This folder is populated on our server, but empty on github.
- `data` analysis-ready datasets.  This folder is populated on our server, but empty on github.
- `man/` used for nowaclite R package documentation.

# Guidelines
- Source code and documentation must be written in english.
- We follow [Hadley Wickham's R package guide](http://adv-r.had.co.nz).
- Datasets generated with our pipeline/r package must be complemented with an R script or R markdown file describing how the dataset was created.
