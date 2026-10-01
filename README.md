# toastmaker

Project setup toolbox for R: a consistent folder tree for projects, papers, presentations, posters and datasets, with a single object that holds all the paths.

## Installation

``` r
pak::pak("tomasmartak/toastmaker")                      # from GitHub
remotes::install_local("path/to/toastmaker")       # or from a local clone
```

Restart R before reinstalling if the package is already loaded.

## Setup

Run once; it asks for your root folder, a base folder inside it (e.g. a synced cloud folder), an optional grant and lab, and the default layout.

``` r
library(toastmaker)
toastmaker_setup()
toastmaker_settings()                  # show settings
toastmaker_settings(lab = "ABC")       # change one
toastmaker_settings(lab = NULL)        # unset it
```

## Usage

``` r
toast <- project_setup("Proj1", experiment_name = "2026-09-30_exp1",
                       raw_subdir = "FC", analysis_subdir = "FlowJo")
```

This shows the tree, asks to confirm anything new, and creates:

```
<root>/<base>/[<grant>/][<lab>/]Projects/Proj1/
├── .toastmaker          marker file, so scripts can find the project
├── analysis/FlowJo/2026-09-30_exp1/
├── raw/FC/2026-09-30_exp1/
├── img/2026-09-30_exp1/
├── doc/2026-09-30_exp1/
└── scripts/
```

All paths are in the returned object:

``` r
toast$bd            # project folder
toast$dir_home      # <root>/<base>
toast$dir_raw       # also dir_analysis, dir_img, dir_doc, dir_scripts
df <- read.csv(file.path(toast$dir_raw, "counts.csv"))
```

Inside a project, leave out the name and it is found from the marker, wherever the folder is synced to:

``` r
toast <- project_setup(experiment_name = "2026-09-30_exp1", raw_subdir = "FC")
```

### Papers, presentations, posters

``` r
paper <- project_setup("2026_KO-screen", type = "paper")
```

| `type`         | Subfolders                                                             |
|----------------|------------------------------------------------------------------------|
| `paper`        | `manuscript/`, `figures/`, `supplementary/`, `scripts/`, `submission/` |
| `presentation` | `slides/`, `figures/`, `scripts/`, `notes/`                            |
| `poster`       | `poster/`, `abstract/`, `figures/`, `scripts/`                         |

Add a figure folder linked to the folders it was made from:

``` r
figure_setup("Fig2_growth", sources = c(toast$dir_analysis, toast$dir_img), where = paper)
```

### Datasets

Shared or public data goes in `Datasets/`; link it from a project:

``` r
dataset_setup("GEO_GSE12345")
toast <- project_setup("Reanalysis", datasets = "GEO_GSE12345")
toast$datasets$GEO_GSE12345
```

### Raw data and package versions

``` r
data_manifest(toast)    # checksum manifest for the raw data
data_verify(toast)      # later: what changed?
project_record("Proj1") # write renv.lock (R and package versions used in scripts/)
```

`project_record()` accepts a project name, a path inside the project, or the `toast` object.

### Workspace

`crumber()` removes everything from the workspace except protected objects. It needs `project_setup(..., assign_global = TRUE)` (or your own `core_objects`).

``` r
crumber(add.to.cookiejar = c(df, fit))   # protect
crumber()                                # clear the rest
```

## Functions

| Function                            | Purpose                                              |
|-------------------------------------|------------------------------------------------------|
| `toastmaker_setup()`, `toastmaker_settings()` | Defaults.                                  |
| `project_setup()`                   | Create a folder tree and return its paths.           |
| `dataset_setup()`, `figure_setup()` | Datasets; figure folders.                            |
| `project_record()`                  | Record package versions in `renv.lock`.              |
| `project_root()`                    | Find the project a script lives in.                  |
| `data_manifest()`, `data_verify()`  | Checksums of raw data.                               |
| `project_cleanup()`, `project_snapshot()` | Remove empty folders; show folder sizes.       |
| `crumber()`                         | Clear the workspace.                                 |

## Development

``` r
devtools::document(); devtools::test(); devtools::check()
```
