#' Record the package versions a project uses
#'
#' Writes `renv.lock` at the root of a project (or paper, presentation,
#' poster or dataset), listing the R version and the exact version of every
#' package that the code in its `scripts/` folder loads, plus their
#' dependencies. Only this small text file is written: no package library is
#' copied into the (synced) project folder.
#'
#' Run it whenever you finish a piece of analysis. [project_setup()] reminds
#' you when scripts have changed since the lockfile was written. To recreate
#' the environment later, e.g. on a new computer:
#'
#' ```r
#' renv::restore(lockfile = "<project>/renv.lock")
#' ```
#'
#' Requires the \pkg{renv} package.
#'
#' @param where A `"toast"` from [project_setup()], a path inside the project,
#'   or the name of a project, e.g. `"Initial_KOs"` (looked up under the
#'   `root`/`home_base` from [toastmaker_settings()]). Default: the working
#'   directory.
#'
#' @return Invisibly, the path of the lockfile.
#' @export
project_record <- function(where = ".") {
    if (!requireNamespace("renv", quietly = TRUE)) {
        stop("project_record() needs the renv package: install.packages(\"renv\").",
             call. = FALSE)
    }
    bd <- if (inherits(where, "toast")) where$bd else .tm_locate(where)
    scripts <- file.path(bd, "scripts")
    pkgs <- if (dir.exists(scripts)) {
        unique(renv::dependencies(scripts, progress = FALSE, quiet = TRUE)$Package)
    }
    base_pkgs <- rownames(utils::installed.packages(priority = "base"))
    pkgs <- setdiff(pkgs, c(base_pkgs, "R"))
    if (length(pkgs) == 0L) {
        .tm_say("No packages found in ", scripts, "; nothing recorded.", style = "33")
        return(invisible(NULL))
    }
    missing_pkgs <- pkgs[!nzchar(vapply(pkgs, function(p) system.file(package = p), ""))]
    if (length(missing_pkgs) > 0L) {
        .tm_say("Not installed, so not recorded: ", paste(missing_pkgs, collapse = ", "),
                style = "33")
        pkgs <- setdiff(pkgs, missing_pkgs)
    }

    lock <- file.path(bd, "renv.lock")
    renv::snapshot(project = bd, lockfile = lock, packages = pkgs,
                   library = .libPaths(), prompt = FALSE)
    .tm_say("Recorded R ", getRversion(), " and ", length(pkgs), " package(s) ",
            "used in scripts/ (plus their dependencies) in ", lock, style = "32")
    invisible(lock)
}

# Remind (once per session per folder) when scripts exist but renv.lock is
# missing or older than the newest script.
.tm_renv_hint <- function(bd) {
    key <- paste0("renv:", bd)
    if (key %in% .tm_state$declined) return(invisible(NULL))
    scripts <- list.files(file.path(bd, "scripts"), pattern = "\\.(R|r|Rmd|rmd|qmd)$",
                          recursive = TRUE, full.names = TRUE)
    if (length(scripts) == 0L) return(invisible(NULL))
    lock <- file.path(bd, "renv.lock")
    if (!file.exists(lock)) {
        msg <- "Package versions are not recorded yet"
    } else if (max(file.mtime(scripts)) > file.mtime(lock)) {
        msg <- "Scripts changed since package versions were recorded"
    } else {
        return(invisible(NULL))
    }
    .tm_say(msg, ": run project_record() to update renv.lock.", style = "3")
    .tm_state$declined <- c(.tm_state$declined, key)
    invisible(NULL)
}

# A project folder from a path inside it, or from its name.
.tm_locate <- function(where) {
    if (dir.exists(where)) {
        bd <- .tm_find_marker(where)
        if (!is.null(bd)) return(bd)
    }
    if (is.character(where) && length(where) == 1L && !grepl("[/\\]", where)) {
        home <- .tm_path(path.expand(.tm_setting("root")),
                         .tm_require_home_base(.tm_setting("home_base")))
        tops <- vapply(.tm_layouts, `[[`, "", "top")
        hits <- unlist(lapply(c("", "*/", "*/*/"), function(g) {
            Sys.glob(file.path(home, paste0(g, tops), where, ".toastmaker"))
        }))
        hits <- unique(dirname(.tm_norm(hits)))
        if (length(hits) == 1L) return(hits)
        if (length(hits) > 1L) {
            stop("Several folders are called '", where, "': ",
                 paste(hits, collapse = ", "), ". Pass the full path.", call. = FALSE)
        }
    }
    stop("No toastmaker project found for '", paste(where, collapse = ""), "'. ",
         "Pass the project's path, its name, or the result of project_setup().",
         call. = FALSE)
}
