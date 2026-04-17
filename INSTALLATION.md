# Package Installation Guide

Your repository is now a complete R package! Here's how to use it:

## ðŸŽ‰ Your Package is Ready!

Repository: https://github.com/Bevann/my_common_functions

## Installation Methods

### Method 1: Install from GitHub (Recommended)

Anyone can now install your package directly from GitHub:

```r
# Install devtools if you don't have it
install.packages("devtools")

# Install from GitHub (repo name uses underscores)
devtools::install_github("Bevann/my_common_functions")

# Load the package (package name uses periods)
library(my.common.functions)
```

**Note:** The GitHub repository name and R package name differ due to naming restrictions:
- GitHub repo: `my_common_functions` (underscores)
- R package: `my.common.functions` (periods)

### Method 2: Install from Local Directory

```r
# From the parent directory
devtools::install("W:/wlap/kam/Workarea/BErnst/R_Scripts/my_common_functions")

# Or use this from anywhere
devtools::install_local("W:/wlap/kam/Workarea/BErnst/R_Scripts/my_common_functions")
```

## Using the Package

Once installed, you can use the functions directly:

```r
library(my.common.functions)

# Export to KML
EXPORT_SF_TO_THEMED_KML(
  sf_object = my_data,
  color_field = "category",
  label_field = "name",
  output_filename = "output.kmz"
)

# Create basemap
basemap <- CREATE_BASEMAP_PLOT(
  largest_data = study_area,
  ext_expansion = 0.1
)
```

## Getting Help

```r
# Package help
?my.common.functions

# Function help
?EXPORT_SF_TO_THEMED_KML
?CREATE_BASEMAP_PLOT
?EXPORT_GEOREFERENCED_PDF

# See all package functions
help(package = "my.common.functions")
```

## Development Workflow

### After Making Changes

If you modify the R files, you need to:

1. **Update documentation** (if you changed function parameters or added new functions):
```r
library(roxygen2)
setwd("W:/wlap/kam/Workarea/BErnst/R_Scripts/my_common_functions")
roxygen2::roxygenise()
```

2. **Reinstall the package**:
```r
devtools::install()
```

3. **Check for issues**:
```r
devtools::check()
```

4. **Commit and push to GitHub**:
```powershell
git add .
git commit -m "Description of changes"
git push
```

## Package Structure

```
my_common_functions/
â”œâ”€â”€ DESCRIPTION          # Package metadata
â”œâ”€â”€ NAMESPACE           # Auto-generated, don't edit manually
â”œâ”€â”€ LICENSE             # MIT License
â”œâ”€â”€ README.md           # Main documentation
â”œâ”€â”€ .Rbuildignore       # Files to ignore when building package
â”œâ”€â”€ .gitignore          # Files to ignore in git
â”œâ”€â”€ R/                  # R source code
â”‚   â”œâ”€â”€ kml_export_functions.R
â”‚   â”œâ”€â”€ georeferenced_map_functions.R
â”‚   â””â”€â”€ my_common_functions-package.R
â””â”€â”€ man/                # Auto-generated documentation (created on first build)
```

## What Changed?

### Package Structure Created
- âœ… DESCRIPTION file with metadata and dependencies
- âœ… NAMESPACE file for exported functions
- âœ… LICENSE file (MIT)
- âœ… R/ directory with documented source code
- âœ… .Rbuildignore for build configuration
- âœ… Roxygen2 documentation added to main functions

### Functions Exported
- `EXPORT_SF_TO_THEMED_KML()` - KML/KMZ export with theming
- `CREATE_BASEMAP_PLOT()` - Create basemap ggplot
- `EXPORT_GEOREFERENCED_PDF()` - Export to georeferenced PDF

### Installation Method
- Now installable with `devtools::install_github("Bevann/my_common_functions")`
- Functions available after `library(my.common.functions)`
- Help documentation accessible with `?function_name`

## Sharing with Others

Share the GitHub link: https://github.com/Bevann/my_common_functions

Others can install with:
```r
devtools::install_github("Bevann/my_common_functions")
```

## Next Steps (Optional)

### Add Tests
```r
usethis::use_testthat()
usethis::use_test("kml_export")
```

### Add Vignettes (Long-form Documentation)
```r
usethis::use_vignette("kml-export-guide")
```

### Submit to CRAN (If Making Public)
```r
devtools::check()  # Must pass with no errors
devtools::build()
# Then submit to CRAN
```

---

**Your package is now live and ready to use!** ðŸš€
