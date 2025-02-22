# Implicit and explicit attitudes toward people with physical disabilities in clinicians, rehabilitation assistants, and other occupations: material, data, and R script

## Overview
This repository contains data and an `R` script to compare explicit and implicit attitudes toward physical disability in clinicians, rehabilitation assistants, and individuals in other occupations using the [Physical Disability IAT dataset](https://doi.org/10.17605/OSF.IO/Y9HIQ), collected from 2022 to 2024 via the [Project Implicit](https://implicit.harvard.edu/implicit/selectatest.html) and made available on the Open Science Framework (OSF) under the CC0 1.0 Universal license.

## Data
The script expect SAV file that is subsequently imported by `R`.

## File
### Disability_IAT.public.2022-2024.sav.zip
This file contains data that needs to be unzipped before use. You can unzip it in the same directory as the R script or specify the file path in the script. 

## Prerequisites
### R version
4.4.1 or higher

### R Packages
The script assume/require the following packages to be installed in `R`:
- [`haven`](https://github.com/hadley/haven)
- [`here`](https://github.com/jennybc/here)
- [`dplyr`](https://github.com/tidyverse/dplyr)
- [`tidyr`](https://github.com/tidyverse/tidyr)
- [`stringr`](https://github.com/tidyverse/stringr)
- [`effects`](https://github.com/cran/effects)
- [`TOSTER`](https://github.com/MHaug/TOSTER)
- [`ggplot2`](https://github.com/tidyverse/ggplot2)
- [`broom`](https://github.com/tidymodels/broom)
- [`viridis`](https://github.com/sjmgarnier/viridis)
- [`patchwork`](https://github.com/thomasp85/patchwork)
