# Push to GitHub

After creating the repository at https://github.com/Bevann/my_common_functions, run:

```powershell
git push -u origin main
```

This will upload your package to GitHub.

## Installation

Once pushed, anyone can install your package with:

```r
devtools::install_github("Bevann/my_common_functions")
library(my_common_functions)
```
