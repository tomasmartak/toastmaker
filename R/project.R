#' Set up a project directory tree
#'
#' Creates the standard subdirectories under
#' `<root>/<home_base>/<grant>/<lab>/Projects/<project>/`:
#'
#' ```
#' analysis/[analysis_subdir/][experiment_name/]
#' raw/[raw_subdir/][experiment_name/]
#' img/[img_subdir/][experiment_name/]
#' doc/[experiment_name/]
#' scripts/
#' ```
#'
#' Existing directories are left untouched. Any path component set to `NULL`
#' (e.g. `lab` or `grant`) is skipped.
#'
#' @param home_base Sync/home folder inside `root`
#'   (default `getOption("toastmaker.home_base", "heiBOX")`).
#' @param grant Grant or grouping folder
#'   (default `getOption("toastmaker.grant", "GRK2727")`).
#' @param lab Lab identifier, e.g. `"AC"`. Optional.
#' @param project Project name, e.g. `"Initial_KOs"`. Required.
#' @param experiment_name Experiment folder appended to `analysis`, `raw`,
#'   `img` and `doc`, e.g. `"2025-04-28_growth-countess"`. Optional.
#' @param analysis_subdir,raw_subdir,img_subdir Optional intermediate folders
#'   (e.g. `"FlowJo"`, `"FC"`, `"cytotoxicity_assays"`), inserted before
#'   `experiment_name`.
#' @param root Folder that contains `home_base`
#'   (default `getOption("toastmaker.root")`, else `~/Documents`, or `~` on
#'   Windows).
#' @param auto_cleanup If `TRUE` (default), directories created by this call
#'   that are still empty are removed when the R session exits.
#' @param assign_global If `TRUE` (default), assigns `bd`, `dir_analysis`,
#'   `dir_raw`, `dir_img`, `dir_doc`, `dir_scripts`, `dir_list` and
#'   `core_objects` (used by [crumber()]) into the calling environment.
#'
#' @return Invisibly, a named list of class `"cookiecutter"` with elements
#'   `bd`, `dir_analysis`, `dir_raw`, `dir_img`, `dir_doc` and `dir_scripts`.
#' @seealso [project_cleanup()], [project_snapshot()], [crumber()]
#' @export
#' @examples
#' tmp <- tempfile()
#' p <- project_setup(lab = "AC", project = "Initial_KOs",
#'                    experiment_name = "2025-04-28_growth",
#'                    raw_subdir = "FC", root = tmp,
#'                    auto_cleanup = FALSE, assign_global = FALSE)
#' p
#' unlink(tmp, recursive = TRUE)
project_setup <- function(home_base       = getOption("toastmaker.home_base", "heiBOX"),
                          grant           = getOption("toastmaker.grant", "GRK2727"),
                          lab             = NULL,
                          project,
                          experiment_name = NULL,
                          analysis_subdir = NULL,
                          raw_subdir      = NULL,
                          img_subdir      = NULL,
                          root            = getOption("toastmaker.root", .tm_default_root()),
                          auto_cleanup    = TRUE,
                          assign_global   = TRUE) {
    if (missing(project) || !is.character(project) || length(project) != 1L ||
        !nzchar(project)) {
        stop("'project' is required, e.g. project = \"MyProject\".", call. = FALSE)
    }

    bd <- .tm_path(path.expand(root), home_base, grant, lab, "Projects", project)

    cc <- structure(
        list(
            bd           = bd,
            dir_analysis = .tm_path(bd, "analysis", analysis_subdir, experiment_name),
            dir_raw      = .tm_path(bd, "raw", raw_subdir, experiment_name),
            dir_img      = .tm_path(bd, "img", img_subdir, experiment_name),
            dir_doc      = .tm_path(bd, "doc", experiment_name),
            dir_scripts  = file.path(bd, "scripts")
        ),
        class = "cookiecutter"
    )

    # Record every directory (including parents) this call creates, so that
    # cleanup only ever touches what we made.
    .tm_say("Setting up project tree under ", bd, style = "32")
    created <- character(0L)
    for (d in unlist(cc[-1L])) {
        new <- .tm_missing_ancestors(d)
        if (length(new) > 0L) {
            dir.create(d, recursive = TRUE, showWarnings = FALSE)
            .tm_say("  + created: ", d, style = "90")
        } else {
            .tm_say("  = exists:  ", d, style = "90")
        }
        created <- union(created, new)
    }

    if (isTRUE(assign_global)) {
        env <- parent.frame()
        list2env(unclass(cc), envir = env)
        assign("dir_list", cc, envir = env)
        protected <- c(names(cc), "dir_list", "core_objects")
        if (exists("core_objects", envir = env, inherits = FALSE)) {
            protected <- union(get("core_objects", envir = env), protected)
        }
        assign("core_objects", protected, envir = env)
    }

    if (isTRUE(auto_cleanup) && length(created) > 0L) {
        .tm_state$dirs <- union(.tm_state$dirs, created)
        .tm_register_exit_hook()
        .tm_say("Empty new directories will be removed when R exits; ",
                "run project_cleanup() to do it now.", style = "3")
    }

    invisible(cc)
}

#' @export
print.cookiecutter <- function(x, ...) {
    cat("<cookiecutter project>\n")
    for (nm in names(x)) cat(sprintf("  %-12s %s\n", nm, x[[nm]]))
    invisible(x)
}

#' Remove empty project directories
#'
#' Removes directories that contain no files (checked recursively, so a
#' folder holding only empty folders is empty). Directories with files are
#' always kept.
#'
#' @param dirs Directories to inspect. May be a character vector or a
#'   `"cookiecutter"` object from [project_setup()] (its subdirectories are
#'   checked; the project root `bd` is left alone). If `NULL` (default), uses the directories created by
#'   `project_setup()` in this session with `auto_cleanup = TRUE`.
#' @param verbose Print what is removed or kept (default `TRUE`).
#'
#' @return Invisibly, the paths that were removed.
#' @export
#' @examples
#' tmp <- tempfile()
#' p <- project_setup(project = "demo", root = tmp,
#'                    auto_cleanup = FALSE, assign_global = FALSE)
#' writeLines("x", file.path(p$dir_scripts, "analysis.R"))
#' project_cleanup(p) # removes everything but scripts/
#' unlink(tmp, recursive = TRUE)
project_cleanup <- function(dirs = NULL, verbose = TRUE) {
    from_state <- is.null(dirs)
    if (from_state) dirs <- .tm_state$dirs
    if (inherits(dirs, "cookiecutter")) {
        # Leaf directories plus their parents below the project root.
        bd <- dirs$bd
        dirs <- unlist(lapply(unlist(unclass(dirs)[-1L]), function(d) {
            out <- character(0L)
            while (!identical(d, bd) && startsWith(d, bd)) {
                out <- c(out, d)
                d <- dirname(d)
            }
            out
        }))
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
#' @param cc A `"cookiecutter"` object from [project_setup()]. If `NULL`,
#'   `dir_list` is looked up from the calling environment.
#'
#' @return Invisibly, a data frame with columns `dir`, `path`, `exists`,
#'   `n_files` and `bytes`.
#' @export
#' @examples
#' tmp <- tempfile()
#' p <- project_setup(project = "demo", root = tmp,
#'                    auto_cleanup = FALSE, assign_global = FALSE)
#' project_snapshot(p)
#' unlink(tmp, recursive = TRUE)
project_snapshot <- function(cc = NULL) {
    if (is.null(cc)) {
        cc <- get0("dir_list", envir = parent.frame(), inherits = TRUE)
    }
    if (!inherits(cc, "cookiecutter")) {
        stop("No project found: pass the result of project_setup() as 'cc'.",
             call. = FALSE)
    }

    dirs <- unlist(unclass(cc)[-1L])
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
