
## `MonitoR` package

*Under development*

------------------------------------------------------------------------

This package provides workflows in form of `R Markdown` templates for
processing monitoring data, for now mostly focusing on the analysis of
audio files obtained by deploying passive acoustic monitoring (PAM)
devices. The package relies on
[NocMigR2](github.com/mottensmann/NocMigR2). See details there.

To install the package, use
[devtools](https://github.com/r-lib/devtools):

``` r
devtools::install_github("mottensmann/MonitoR", dependencies = TRUE)
```

*`NocMigR2` depends on `warbleR` which is currently (as of 2024-07-26)
missing on CRAN. If the installation above fails try:*

``` r
devtools::install_github("maRce10/warbleR")
```

Load the package once installed:

``` r
library(MonitoR)
```

## Templates

`MonitoR` provides RMarkdown templates to set-up post-processing and
validation of audio data classified using
[BirdNET-Analyzer](https://github.com/kahst/BirdNET-Analyzer). Using
`RStudio` these are accessible via `File -> New File -> RMarkdown`

### birdnet

- 1)  Preprocessing of audio data (renaming to datetime, removing
      unessary file prefixes)
- 2)  Extracting results obtained by using a AI-based classifier with
      [BirdNET-Analyzer](https://github.com/kahst/BirdNET-Analyzer)
- 3)  Archiving manually verified records to a xlsx-database

### birdnetR

- 1)  Preprocessing of audio data (renaming to datetime, removing
      unessary file prefixes)
- 2)  Running [birdnetR](https://cran.r-project.org/package=birdnetR)
- 3)  Extracting results obtained by using a AI-based classifier with
      [birdnetR](https://cran.r-project.org/package=birdnetR)
- 4)  Archiving manually verified records to a xlsx-database

### nestcamera

- 1)  Preprocessing of photos (renaming to datetime and resorting)
