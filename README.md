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

## First-time setup

Nothing is configured when the package is installed. Run the guided setup once:

``` r
library(toastmaker)
toastmaker_setup()
```

It asks for your root folder, your base folder (e.g. a synced cloud folder such as `heiBOX`; required), an optional grant and lab, and which layout `project_setup()` should create by default. View or change single settings later with `toastmaker_settings()`:

``` r
toastmaker_settings()                    # show current settings
toastmaker_settings(grant = "GRK2727")   # change one
toastmaker_settings(grant = NULL)        # unset it (the grant folder is then skipped)
```

## Usage

``` r
project_setup("Proj1", lab = "ABC",
              experiment_name = "2026-09-30_exp1",
              raw_subdir = "FC", analysis_subdir = "FlowJo")
```

This shows the tree, asks you to confirm anything new, and creates (existing folders are left alone):

```         
~/Documents/heiBOX/GRK2727/ABC/Projects/Proj1/
├── .toastmaker          marker: lets scripts find the project from inside it
├── analysis/FlowJo/2026-09-30_exp1/
├── raw/FC/2026-09-30_exp1/
├── img/2026-09-30_exp1/
├── doc/2026-09-30_exp1/
└── scripts/
```

It puts `bd`, `dir_analysis`, `dir_raw`, `dir_img`, `dir_doc`, `dir_scripts` and `dir_list` into your workspace:

``` r
df <- read.csv(file.path(dir_raw, "counts.csv"))
```

Inside a script that lives in the project, `project` can be left out. The project is then found from its `.toastmaker` marker, so the script still works after the folder is synced to another computer:

``` r
p <- project_setup(experiment_name = "2026-09-30_exp1", raw_subdir = "FC")
```

### Papers, presentations and posters

These live next to `Projects/`, independent of any one project:

``` r
project_setup("2026_KO-screen", type = "paper")           # Papers/2026_KO-screen/
project_setup("2026-11-05_retreat", type = "presentation") # Presentations/...
project_setup("2026-11-05_EMBO", type = "poster")          # Posters/...
```

| `type` | Subfolders |
|------------------|------------------------------------------------------|
| `paper` | `manuscript/`, `figures/`, `supplementary/`, `scripts/`, `submission/` |
| `presentation` | `slides/`, `figures/`, `scripts/`, `notes/` |
| `poster` | `poster/`, `abstract/`, `figures/`, `scripts/` |

Each one gets a `README.md` explaining what goes where. Figures go in one folder each (e.g. `figures/Fig1_growth/`) with the final image, `source_data/`, and a README naming the projects and experiments they came from.

### Documenting raw data

When you set up an experiment, `project_setup()` offers to write a `README.md` template into the raw data folder (experiment, date, technique, samples, protocol, related folders). Once the folder holds data, it offers a checksum manifest:

``` r
data_manifest(p)  # write raw/FC/2026-09-30_exp1/MANIFEST.md5
data_verify(p)    # later: has anything changed, gone missing, or appeared?
```

| Function | What it does |
|------------------|------------------------------------------------------|
| `toastmaker_setup()` | Guided setup of your defaults. |
| `toastmaker_settings()` | View or change single defaults. |
| `project_setup()` | Create a project, paper, presentation or poster tree and assign the path variables. |
| `project_root()` | Find the project a script lives in. |
| `data_manifest()` / `data_verify()` | Record and check checksums of raw data. |
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

`crumber()` frees memory and declutters during interactive work. It does not reset loaded packages, options or the random seed, so to check that a script is reproducible, restart R and run it from the top.

## Development

``` r
devtools::document()  # regenerate man/ and NAMESPACE from roxygen comments
devtools::test()      # run tests
devtools::check()     # full R CMD check
```
