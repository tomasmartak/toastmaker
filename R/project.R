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
#' goes in each subfolder. Every folder gets a `.toastmaker` marker file at
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
#' skipped when unset (or `NULL`).
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
#' @param lab Lab folder, e.g. `"AC"`.
#' @param grant Grant or grouping folder.
#' @param home_base Base folder inside `root`, e.g. a synced cloud folder.
#' @param root Folder that contains `home_base`.
#' @param confirm Ask before creating new folders (default: `interactive()`).
#' @param check_raw Offer a README and checksum manifest for the raw data
#'   folder (default `TRUE`; prompts only in interactive sessions).
#' @param auto_cleanup If `TRUE` (default), directories created by this call
#'   that are still empty are removed when the R session exits.
#' @param assign_global If `TRUE` (default), assigns `bd`, each `dir_*`
#'   path, `dir_list` and `core_objects` (used by [crumber()]) into the
#'   calling environment.
#'
#' @return Invisibly, a named list of class `"cookiecutter"` with `bd` (the
#'   folder root) and one `dir_*` element per subfolder, or `NULL` if you
#'   declined to create it. Its `type` attribute holds the layout.
#' @seealso [toastmaker_setup()], [project_cleanup()], [project_snapshot()],
#'   [project_root()], [crumber()]
#' @export
#' @examples
#' tmp <- tempfile()
#' p <- project_setup("Initial_KOs", lab = "AC", home_base = "heiBOX",
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
                          lab             = .tm_setting("lab"),
                          grant           = .tm_setting("grant"),
                          home_base       = .tm_setting("home_base"),
                          root            = .tm_setting("root"),
                          confirm         = interactive(),
                          check_raw       = TRUE,
                          auto_cleanup    = TRUE,
                          assign_global   = TRUE) {
    if (missing(project)) {
        bd <- .tm_find_marker(".")
        if (is.null(bd)) {
            stop("'project' is required, e.g. project_setup(\"MyProject\"), ",
                 "unless the working directory is inside an existing project.",
                 call. = FALSE)
        }
        marker <- .tm_read_marker(bd)
        if (!is.null(type) && !identical(type, marker$Type)) {
            stop("The folder ", bd, " is a '", marker$Type, "', not a '", type, "'.",
                 call. = FALSE)
        }
        type <- marker$Type
        project <- marker$Name %||% basename(bd)
    } else {
        if (!is.character(project) || length(project) != 1L || !nzchar(project)) {
            stop("'project' must be a single non-empty string.", call. = FALSE)
        }
        type <- .tm_check_type(type %||% .tm_setting("type"))
        home_base <- .tm_require_home_base(home_base)
        bd <- .tm_path(path.expand(root), home_base, grant, lab,
                       .tm_layouts[[type]]$top, project)
    }

    proj_args <- list(experiment_name = experiment_name, analysis_subdir = analysis_subdir,
                      raw_subdir = raw_subdir, img_subdir = img_subdir)
    if (!identical(type, "project")) {
        given <- names(Filter(Negate(is.null), proj_args))
        if (length(given) > 0L) {
            stop(paste(given, collapse = ", "), " can only be used with type = \"project\".",
                 call. = FALSE)
        }
    }
    if (!is.null(experiment_name) &&
        !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", experiment_name)) {
        warning("experiment_name \"", experiment_name, "\" does not start with a date ",
                "(YYYY-MM-DD, e.g. \"", format(Sys.Date()), "_", experiment_name, "\").",
                call. = FALSE)
    }

    cc <- structure(c(list(bd = bd), .tm_layouts[[type]]$dirs(bd, proj_args)),
                    class = "cookiecutter", type = type)
    dirs <- unlist(unclass(cc)[-1L])

    # Show what will be created and, if asked to, wait for confirmation.
    bd_new <- !dir.exists(bd)
    if (any(!dir.exists(dirs))) {
        .tm_say("Setting up ", type, " folder:", style = "32")
        for (l in .tm_tree(bd, dirs)) {
            .tm_say("  ", l, style = if (endsWith(l, "(new)")) "32" else "90")
        }
        if (isTRUE(confirm) && !.tm_yes("Create the folders marked (new)?")) {
            .tm_say("Nothing was created.", style = "33")
            return(invisible(NULL))
        }
    } else {
        .tm_say("Using ", type, " folder ", bd, style = "90")
    }

    # Record every directory (including parents) this call creates, so that
    # cleanup only ever touches what we made.
    created <- character(0L)
    for (d in dirs) {
        created <- union(created, .tm_missing_ancestors(d))
        dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }

    .tm_write_marker(bd, type, project, lab = lab, grant = grant, new = bd_new)
    guide <- file.path(bd, "README.md")
    if (!is.null(.tm_layouts[[type]]$guide) && !file.exists(guide)) {
        writeLines(.tm_guide_text(type, project), guide)
    }
    if (identical(type, "project") && !is.null(experiment_name) && isTRUE(check_raw)) {
        .tm_check_raw(cc, project, experiment_name, raw_subdir, .tm_read_marker(bd)$Lab)
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
    cat("<cookiecutter ", attr(x, "type") %||% "project", ">\n", sep = "")
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
#' p <- project_setup("demo", home_base = "base", root = tmp, confirm = FALSE,
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
        dirs <- unlist(lapply(unlist(unclass(dirs)[-1L]), .tm_below, bd = bd))
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
#' p <- project_setup("demo", home_base = "base", root = tmp, confirm = FALSE,
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
