# toastmaker

Personal project setup toolbox for R: create a consistent project folder tree, clean up the folders you never used, and keep your workspace tidy.

## Installation

``` r
# from GitHub (use a personal access token if the repo is private:
# Sys.setenv(GITHUB_PAT = "ghp_..."))
pak::pak("tomasmartak/toastmaker")

# or from a local clone
remotes::install_local("path/to/toastmaker")
```

## Usage

``` r
library(toastmaker)

project_setup(lab = "ABC", project = "Proj1",
              experiment_name = "2026-09-30_exp1",
              raw_subdir = "FC", analysis_subdir = "FlowJo")
```

This creates (existing folders are left alone):

```         
~/Documents/heiBOX/GRK2727/ABC/Projects/Proj1/
├── analysis/FlowJo/2026-09-30_exp1/
├── raw/FC/2026-09-30_exp1/
├── img/2026-09-30_exp1/
├── doc/2026-09-30_exp1/
└── scripts/
```

and puts `bd`, `dir_analysis`, `dir_raw`, `dir_img`, `dir_doc`, `dir_scripts` and `dir_list` into your workspace:

``` r
df <- read.csv(file.path(dir_raw, "counts.csv"))
```

| Function | What it does |
|------------------|------------------------------------------------------|
| `project_setup()` | Create the project tree and assign the path variables. |
| `project_cleanup()` | Remove empty folders. Also runs automatically when R exits. |
| `project_snapshot()` | Show which folders exist, how many files they hold, and their size. |
| `crumber()` | Clear the workspace, keeping only protected objects. |

### Keeping the workspace tidy

``` r
crumber(add.to.cookiejar = c(df, fit))  # protect `df` and `fit`
crumber()                               # remove everything else
crumber(consume.cookie = fit)           # stop protecting `fit`
crumber(notify = TRUE)                  # beep when done (needs `beepr`)
```

## Development

``` r
devtools::document()  # regenerate man/ and NAMESPACE from roxygen comments
devtools::test()      # run tests
devtools::check()     # full R CMD check
```
