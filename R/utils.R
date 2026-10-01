# Internal helpers -------------------------------------------------------------

# Session state: directories created by project_setup() that are candidates
# for cleanup, and whether the exit hook is registered.
.tm_state <- new.env(parent = emptyenv())
.tm_state$dirs <- character(0L)
.tm_state$hooked <- FALSE
.tm_state$declined <- character(0L) # README/manifest offers declined this session

`%||%` <- function(x, y) if (is.null(x)) y else x

# file.path() that silently drops NULL components. Separators are always "/",
# matching what dirname() returns on Windows, so paths compare reliably.
.tm_path <- function(...) {
    parts <- Filter(Negate(is.null), list(...))
    if (length(parts) == 0L) stop("No path components supplied.", call. = FALSE)
    gsub("\\\\", "/", do.call(file.path, parts))
}

.tm_default_root <- function() {
    if (.Platform$OS.type == "windows") "~" else "~/Documents"
}

# A directory counts as empty if it holds no files, even in subdirectories.
# toastmaker's own marker files do not count.
.tm_is_empty <- function(path) {
    !dir.exists(path) ||
        all(basename(list.files(path, recursive = TRUE, all.files = TRUE)) ==
                ".toastmaker")
}

# `path` and its parents, up to but excluding `bd`.
.tm_below <- function(path, bd) {
    out <- character(0L)
    while (!identical(path, bd) && startsWith(path, bd)) {
        out <- c(out, path)
        path <- dirname(path)
    }
    out
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

# Wrappers so tests can mock user interaction.
.tm_interactive <- function() interactive()
.tm_readline <- function(prompt) readline(prompt)

# Ask a yes/no question; Enter gives `default`.
.tm_yes <- function(question, default = TRUE) {
    ans <- tolower(trimws(.tm_readline(paste0(question, if (default) " [Y/n] " else " [y/N] "))))
    if (!nzchar(ans)) return(default)
    startsWith(ans, "y")
}

# Draw the folders `dirs` (and their parents) below `bd` as a tree, marking
# the ones that do not exist yet.
.tm_tree <- function(bd, dirs) {
    nodes <- unique(unlist(lapply(dirs, .tm_below, bd = bd)))
    rel <- substring(nodes, nchar(bd) + 2L)
    utf8 <- isTRUE(l10n_info()[["UTF-8"]])
    tee  <- if (utf8) "\u251c\u2500\u2500 " else "|-- "
    elb  <- if (utf8) "\u2514\u2500\u2500 " else "`-- "
    pipe <- if (utf8) "\u2502   " else "|   "
    new  <- function(p) if (dir.exists(p)) "" else "  (new)"

    lines <- paste0(bd, "/", new(bd))
    walk <- function(parent, prefix) {
        kids <- sort(rel[dirname(rel) == parent])
        for (i in seq_along(kids)) {
            last <- i == length(kids)
            lines <<- c(lines, paste0(prefix, if (last) elb else tee,
                                      basename(kids[i]), "/", new(file.path(bd, kids[i]))))
            walk(kids[i], paste0(prefix, if (last) "    " else pipe))
        }
    }
    walk(".", "")
    lines
}
