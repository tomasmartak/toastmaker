#' Find the root of the current project
#'
#' Every folder created by [project_setup()] (project, paper, presentation or
#' poster) has a small `.toastmaker` marker file at its root. `project_root()`
#' walks up from `path` until it finds one, so scripts can locate their
#' project wherever it has been copied or synced to.
#'
#' @param path Where to start looking (default: the working directory).
#'
#' @return The root folder, as an absolute path. An error if no marker is
#'   found.
#' @seealso [project_setup()], which uses this when called without `project`.
#' @export
#' @examples
#' tmp <- tempfile()
#' p <- project_setup("demo", home_base = "base", root = tmp, confirm = FALSE,
#'                    auto_cleanup = FALSE, assign_global = FALSE)
#' project_root(p$dir_scripts)
#' unlink(tmp, recursive = TRUE)
project_root <- function(path = ".") {
    root <- .tm_find_marker(path)
    if (is.null(root)) {
        stop("No toastmaker project found in or above ", .tm_norm(path), ".",
             call. = FALSE)
    }
    root
}

.tm_marker_name <- ".toastmaker"

.tm_norm <- function(path) normalizePath(path, winslash = "/", mustWork = FALSE)

.tm_find_marker <- function(path = ".") {
    d <- .tm_norm(path)
    repeat {
        if (file.exists(file.path(d, .tm_marker_name))) return(d)
        up <- dirname(d)
        if (identical(up, d)) return(NULL)
        d <- up
    }
}

.tm_read_marker <- function(bd) {
    m <- read.dcf(file.path(bd, .tm_marker_name))
    as.list(m[1L, ])
}

# Write the marker if it is missing. `new` says whether `bd` was created by
# this call (then the date is the creation date) or already existed.
.tm_write_marker <- function(bd, type, name, lab = NULL, grant = NULL, new = TRUE) {
    f <- file.path(bd, .tm_marker_name)
    if (file.exists(f)) return(invisible(FALSE))
    fields <- list(Type = type, Name = name, Lab = .tm_clean(lab),
                   Grant = .tm_clean(grant))
    fields[[if (new) "Created" else "Marked"]] <- format(Sys.Date())
    fields$Toastmaker <- as.character(utils::packageVersion("toastmaker"))
    fields <- Filter(Negate(is.null), fields)
    write.dcf(matrix(unlist(fields), nrow = 1L, dimnames = list(NULL, names(fields))), f)
    invisible(TRUE)
}
