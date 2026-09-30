# Internal helpers -------------------------------------------------------------

# Session state: directories created by project_setup() that are candidates
# for cleanup, and whether the exit hook is registered.
.tm_state <- new.env(parent = emptyenv())
.tm_state$dirs <- character(0L)
.tm_state$hooked <- FALSE

# file.path() that silently drops NULL components.
.tm_path <- function(...) {
    parts <- Filter(Negate(is.null), list(...))
    if (length(parts) == 0L) stop("No path components supplied.", call. = FALSE)
    do.call(file.path, parts)
}

.tm_default_root <- function() {
    if (.Platform$OS.type == "windows") "~" else "~/Documents"
}

# A directory counts as empty if it holds no files, even in subdirectories.
.tm_is_empty <- function(path) {
    !dir.exists(path) ||
        length(list.files(path, recursive = TRUE, all.files = TRUE)) == 0L
}

# `path` plus every ancestor that does not exist yet (deepest first).
.tm_missing_ancestors <- function(path) {
    out <- character(0L)
    while (!dir.exists(path) && !identical(dirname(path), path)) {
        out <- c(out, path)
        path <- dirname(path)
    }
    out
}

# cat() a line, optionally wrapped in an ANSI style (e.g. "32" = green).
.tm_say <- function(..., style = NULL) {
    msg <- paste0(...)
    if (!is.null(style) && isTRUE(getOption("toastmaker.color", interactive()))) {
        msg <- paste0("\033[", style, "m", msg, "\033[0m")
    }
    cat(msg, "\n", sep = "")
}

# Run project_cleanup() when the R session ends. The finalizer is attached to
# the package state environment, which lives until exit, so it never touches
# the user's .Last().
.tm_register_exit_hook <- function() {
    if (isTRUE(.tm_state$hooked)) return(invisible(FALSE))
    reg.finalizer(.tm_state, function(e) try(project_cleanup(), silent = TRUE),
                  onexit = TRUE)
    .tm_state$hooked <- TRUE
    invisible(TRUE)
}

# Turn an argument into object names: accepts a character vector, a bare
# name, or c() of bare names / strings.
.tm_names <- function(expr) {
    if (is.null(expr)) return(NULL)
    if (is.character(expr)) return(expr)
    if (is.symbol(expr)) return(as.character(expr))
    if (is.call(expr) && identical(expr[[1L]], quote(c))) {
        return(unlist(lapply(as.list(expr)[-1L], .tm_names)))
    }
    stop("Supply object names as bare names, strings, or c(...) of them.",
         call. = FALSE)
}
