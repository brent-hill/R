# R Package Repository
This is a public repository for my R packages. These are mainly utility and convenience packages written for myself and for students in the following courses at NDSU: EDUC 702, EDUC 873, EDUC 881, EDUC 882, and EDUC 885.

## Packages

- **describe/**: Contains the source files for the `describe` package.
  - `describe.Rproj`: RStudio project file.
  - `DESCRIPTION`: Metadata about the package.
  - `NAMESPACE`: Functions and datasets exported by the package.
  - `R/`: Directory containing R scripts.
  - `man/`: Directory containing manual files.

- **manage**
  - Currently in development

- **inference.tests**
  - Currently in development

## Installation

To install a package from this repository, use the following command in R:

```R
install.packages("<package.name>", repos = "https://brent-hill.github.io/R", type = "source")
```

Alternatively, you can add this repository (`https://brent-hill.github.io/R`) as a secondary repository in RStudio and use the "Install" button in the Packages tab.
