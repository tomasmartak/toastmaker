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

project_setup(lab = "AC", project = "Initial_KOs",
              experiment_name = "2025-04-28_growth-countess",
              raw_subdir = "FC", analysis_subdir = "FlowJo")
```

This creates (existing folders are left alone):

```         
~/Documents/heiBOX/GRK2727/AC/Projects/Initial_KOs/
├── analysis/FlowJo/2025-04-28_growth-countess/
├── raw/FC/2025-04-28_growth-countess/
├── img/2025-04-28_growth-countess/
├── doc/2025-04-28_growth-countess/
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

## Configuration

Set defaults in your `~/.Rprofile` (open it with `usethis::edit_r_profile()`):

``` r
options(
    toastmaker.root      = "/run/media/me/My SSD",  # folder containing home_base
    toastmaker.home_base = "heiBOX",
    toastmaker.grant     = "GRK2727"
)
```

Any of `home_base`, `grant` or `lab` can be set to `NULL` to leave that level out of the path.

## Development

``` r
devtools::document()  # regenerate man/ and NAMESPACE from roxygen comments
devtools::test()      # run tests
devtools::check()     # full R CMD check
```
