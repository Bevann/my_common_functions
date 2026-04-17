# Push to GitHub

After creating the repository at https://github.com/Bevann/my_common_functions, run:

```powershell
git push -u origin main
```

This will upload your package to GitHub.

## Installation

Once pushed, anyone can install your package with:

```r
# Install from GitHub (repo name uses underscores)
devtools::install_github("Bevann/my_common_functions")

# Load library (package name uses periods)
library(my.common.functions)
```

**Note:** The GitHub repository name (`my_common_functions`) differs from the R package name (`my.common.functions`) because:
- GitHub allows underscores but not periods in repository names
- R packages allow periods but not underscores in package names
