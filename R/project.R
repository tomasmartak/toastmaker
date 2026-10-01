#' Set up a project, paper, presentation or poster folder
#'
#' Creates a folder tree under
#' `<root>/<home_base>/[<grant>/][<lab>/]<Top>/<project>/`, where `<Top>`
#' depends on `type`:
#'
#' | `type`           | `<Top>`         | Subfolders |
#' |------------------|-----------------|------------|
#' | `"project"`      | `Projects`      | `analysis/[analysis_subdir/][experiment_name/]`, `raw/[raw_subdir/][experiment_name/]`, `img/[img_subdir/][experiment_name/]`, `doc/[experiment_name/]`, `scripts/` |
#' | `"paper"`        | `Papers`        | `manuscript/`, `figures/`, `supplementary/`, `scripts/`, `submission/` |
#' | `"presentation"` | `Presentations` | `slides/`, `figures/`, `scripts/`, `notes/` |
#' | `"poster"`       | `Posters`       | `poster/`, `abstract/`, `figures/`, `scripts/` |
#'
#' Papers, presentations and posters also get a `README.md` explaining what
#' goes in each subfolder; add figure folders to them with [figure_setup()].
#' Shared or public data lives in `Datasets/` (see [dataset_setup()]). Every folder gets a `.toastmaker` marker file at
#' its root (see [project_root()]).
#'
#' Before creating anything new, the tree is printed with new folders marked,
#' and (in interactive sessions) you are asked to confirm. When all folders
#' already exist, nothing is asked, so scripts can call `project_setup()` at
#' the top every time.
#'
#' For projects with an `experiment_name`, you are offered a `README.md`
#' template for the raw data folder and, once it holds files, a checksum
#' manifest (see [data_manifest()]).
#'
#' Defaults for `root`, `home_base`, `grant`, `lab` and `type` come from
#' [toastmaker_settings()]. `home_base` must be set; `grant` and `lab` are
#' skipped when unset (or `NULL`). If you pass `grant` or `lab` yourself, the
#' other one is *not* taken from the settings, so
#' `project_setup("X", grant = "G")` gives no lab folder even if a default lab
#' is saved. Pass both to use both, or neither to use the saved defaults.
#'
#' @param project Name of the project, paper, presentation or poster, e.g.
#'   `"Initial_KOs"`. If omitted, the project containing the working
#'   directory is used (found via its `.toastmaker` marker), so scripts work
#'   wherever the project is synced to.
#' @param type Folder layout: `"project"`, `"paper"`, `"presentation"` or
#'   `"poster"`. Default from settings (`"project"` unless changed).
#' @param experiment_name Experiment folder appended to `analysis`, `raw`,
#'   `img` and `doc` (projects only). By convention it starts with the date,
#'   e.g. `"2025-04-28_growth-countess"`; a warning is given otherwise.
#' @param analysis_subdir,raw_subdir,img_subdir Optional intermediate folders
#'   (e.g. `"FlowJo"`, `"FC"`, `"cytotoxicity_assays"`), inserted before
#'   `experiment_name` (projects only).
#' @param datasets Names of datasets this folder uses, e.g. `"GEO_GSE12345"`
#'   or `"public/GEO_GSE12345"` (see [dataset_setup()]). Their paths are
#'   returned in `$datasets`, and the link is recorded in both folders'
#'   `.toastmaker` markers.
#' @param lab Lab folder, e.g. `"AC"`. Optional.
#' @param grant Grant or grouping folder. Optional.
#' @param home_base Base folder inside `root`, e.g. a synced cloud folder.
#' @param root Folder that contains `home_base`.
#' @param confirm Ask before creating new folders (default: `interactive()`).
#' @param check_raw Offer a README and checksum manifest for the raw data
#'   folder (default `TRUE`; prompts only in interactive sessions).
#' @param auto_cleanup If `TRUE` (default), directories created by this call
#'   that are still empty are removed when the R session exits.
#' @param assign_global If `TRUE`, also assigns `bd`, each `dir_*` path,
#'   `dir_list` and `core_objects` (used by [crumber()]) into the calling
#'   environment. Default `FALSE`: keep the result, e.g.
#'   `toast <- project_setup("X")`, and use `toast$dir_raw`.
#'
#' @return Invisibly, a named list of class `"toast"` with `bd` (the
#'   folder root), `dir_home` (the `<root>/<home_base>` folder, e.g.
#'   `C:/Users/abc/Documents/heiBOX`), one `dir_*` element per subfolder and,
#'   if requested, `datasets`; or `NULL` if you declined to create it. Its `type` attribute holds the layout.
#' @seealso [toastmaker_setup()], [dataset_setup()], [figure_setup()],
#'   [project_record()], [project_cleanup()], [project_snapshot()],
#'   [project_root()], [crumber()]
#' @export
#' @examples
#' tmp <- tempfile()
#' toast <- project_setup("Initial_KOs", lab = "AC", home_base = "heiBOX",
#'                    experiment_name = "2025-04-28_growth",
#'                    raw_subdir = "FC", root = tmp, confirm = FALSE,
#'                    check_raw = FALSE, auto_cleanup = FALSE,
#'                    assign_global = FALSE)
#' p
#' paper <- project_setup("2025_KO-screen", type = "paper", lab = "AC",
#'                        home_base = "heiBOX", root = tmp, confirm = FALSE,
#'                        auto_cleanup = FALSE, assign_global = FALSE)
#' paper
#' unlink(tmp, recursive = TRUE)
project_setup <- function(project,
                          type            = NULL,
                          experiment_name = NULL,
                          analysis_subdir = NULL,
                          raw_subdir      = NULL,
                          img_subdir      = NULL,
                          datasets        = NULL,
                          lab             = .tm_setting("lab"),
                          grant           = .tm_setting("grant"),
                          home_base       = .tm_setting("home_base"),
                          root            = .tm_setting("root"),
                          confirm         = interactive(),
                          check_raw       = TRUE,
                          auto_cleanup    = TRUE,
                          assign_global   = FALSE) {
    gl <- .tm_grant_lab(grant, lab, missing(grant), missing(lab))
    grant <- gl$grant
    lab <- gl$lab
    if (!is.null(type)) {
        type <- .tm_check_type(type)
    } else if (!missing(project)) {
        type <- .tm_check_type(.tm_setting("type"))
    }
    .tm_setup(type = type,
              name = if (missing(project)) NULL else project,
              args = list(experiment_name = experiment_name,
                          analysis_subdir = analysis_subdir,
                          raw_subdir = raw_subdir, img_subdir = img_subdir),
              datasets = datasets, lab = lab, grant = grant, home_base = home_base,
              root = root, confirm = confirm, check_raw = check_raw,
              auto_cleanup = auto_cleanup, assign_global = assign_global,
              env = parent.frame())
}

#' @export
print.toast <- function(x, ...) {
    cat("<toast ", attr(x, "type") %||% "project", ">\n", sep = "")
    for (nm in setdiff(names(x), "datasets")) cat(sprintf("  %-12s %s\n", nm, x[[nm]]))
    for (nm in names(x$datasets)) {
        cat(sprintf("  %-12s %s\n", paste0("datasets$", nm), x$datasets[[nm]]))
    }
    invisible(x)
}

#' Remove empty project directories
#'
#' Removes directories that contain no files (checked recursively, so a
#' folder holding only empty folders is empty). Directories with files are
#' always kept.
#'
#' @param dirs Directories to inspect. May be a character vector or a
#'   `"toast"` object from [project_setup()] (its subdirectories are
#'   checked; the project root `bd` is left alone). If `NULL` (default), uses the directories created by
#'   `project_setup()` in this session with `auto_cleanup = TRUE`.
#' @param verbose Print what is removed or kept (default `TRUE`).
#'
#' @return Invisibly, the paths that were removed.
#' @export
#' @examples
#' tmp <- tempfile()
#' p <- project_setup("demo", home_base = "base", root = tmp, confirm = FALSE,
#'                    auto_cleanup = FALSE, assign_global = FALSE)
#' writeLines("x", file.path(p$dir_scripts, "analysis.R"))
#' project_cleanup(p) # removes everything but scripts/
#' unlink(tmp, recursive = TRUE)
project_cleanup <- function(dirs = NULL, verbose = TRUE) {
    from_state <- is.null(dirs)
    if (from_state) dirs <- .tm_state$dirs
    if (inherits(dirs, "toast")) {
        # Leaf directories plus their parents below the project root.
        bd <- dirs$bd
        dirs <- unlist(lapply(.tm_dirs(dirs), .tm_below, bd = bd))
    }
    if (length(dirs) == 0L) {
        if (verbose) message("project_cleanup(): nothing to clean.")
        return(invisible(character(0L)))
    }

    # Deepest paths first, so parents are checked after their children.
    dirs <- unique(dirs[order(nchar(dirs), decreasing = TRUE)])
    removed <- character(0L)
    for (d in dirs[dir.exists(dirs)]) {
        if (.tm_is_empty(d)) {
            unlink(d, recursive = TRUE)
            removed <- c(removed, d)
            if (verbose) .tm_say("  - removed (empty): ", d, style = "33")
        } else if (verbose) {
            .tm_say("  = kept (has files): ", d, style = "90")
        }
    }
    if (from_state) .tm_state$dirs <- setdiff(.tm_state$dirs, removed)

    invisible(removed)
}

#' Report the state of the project directories
#'
#' Shows, for each directory of a project, whether it exists, how many files
#' it holds and their total size.
#'
#' @param cc A `"toast"` object from [project_setup()]. If `NULL`,
#'   `dir_list` is looked up from the calling environment.
#'
#' @return Invisibly, a data frame with columns `dir`, `path`, `exists`,
#'   `n_files` and `bytes`.
#' @export
#' @examples
#' tmp <- tempfile()
#' p <- project_setup("demo", home_base = "base", root = tmp, confirm = FALSE,
#'                    auto_cleanup = FALSE, assign_global = FALSE)
#' project_snapshot(p)
#' unlink(tmp, recursive = TRUE)
project_snapshot <- function(cc = NULL) {
    if (is.null(cc)) {
        cc <- get0("dir_list", envir = parent.frame(), inherits = TRUE)
    }
    if (!inherits(cc, "toast")) {
        stop("No project found: pass the result of project_setup() as 'cc'.",
             call. = FALSE)
    }

    dirs <- .tm_dirs(cc)
    info <- lapply(dirs, function(d) {
        files <- list.files(d, recursive = TRUE, all.files = TRUE, full.names = TRUE)
        c(n_files = length(files), bytes = sum(file.size(files)))
    })
    out <- data.frame(
        dir     = names(dirs),
        path    = unname(dirs),
        exists  = dir.exists(dirs),
        n_files = vapply(info, `[[`, numeric(1L), "n_files"),
        bytes   = vapply(info, `[[`, numeric(1L), "bytes"),
        row.names = NULL
    )

    cat("Project snapshot:", cc$bd, "\n")
    shown <- data.frame(
        dir     = out$dir,
        exists  = ifelse(out$exists, "yes", "no"),
        n_files = ifelse(out$exists, out$n_files, NA),
        size    = ifelse(out$n_files > 0,
                         vapply(out$bytes, function(b) format(structure(b, class = "object_size"),
                                                              units = "auto"), ""),
                         "-")
    )
    print(shown, row.names = FALSE)
    invisible(out)
}
