#' Clear the workspace, keeping protected objects
#'
#' Removes every object from `env` whose name is not listed in
#' `core_objects`. [project_setup()] creates `core_objects` with the project
#' paths; add your own long-lived objects to it with `add.to.cookiejar`.
#' Hidden objects (names starting with `.`) are never removed.
#'
#' Object names can be given as bare names (`crumber(add.to.cookiejar = df)`),
#' strings (`"df"`), or `c()` of either (`c(df, fit)`).
#'
#' @param do.gc Run [gc()] after removal (default `TRUE`).
#' @param add.to.cookiejar Names of objects to protect. When given, nothing
#'   is removed; only `core_objects` is updated.
#' @param consume.cookie Names of objects to stop protecting. When given,
#'   nothing is removed; only `core_objects` is updated.
#' @param env Environment to clean (default: the global environment).
#' @param notify Play a sound when done (requires the \pkg{beepr} package).
#'
#' @return Invisibly, the names of removed objects, or the updated
#'   `core_objects` when adding/removing protection.
#' @export
#' @examples
#' env <- new.env()
#' assign("core_objects", "keep_me", envir = env)
#' assign("keep_me", 1, envir = env)
#' assign("scratch", 2, envir = env)
#' crumber(add.to.cookiejar = scratch, env = env) # protect `scratch` too
#' crumber(consume.cookie = "scratch", env = env) # ...and unprotect it again
#' crumber(env = env, do.gc = FALSE)              # removes `scratch`
#' ls(env)
crumber <- function(do.gc = TRUE, add.to.cookiejar = NULL, consume.cookie = NULL,
                    env = globalenv(), notify = FALSE) {
    add <- .tm_names(substitute(add.to.cookiejar))
    drop <- .tm_names(substitute(consume.cookie))

    if (!exists("core_objects", envir = env, inherits = FALSE)) {
        stop("No 'core_objects' found: run project_setup() first, or create ",
             "'core_objects' (a character vector of names to keep).", call. = FALSE)
    }
    core <- get("core_objects", envir = env, inherits = FALSE)

    if (!is.null(add) || !is.null(drop)) {
        core <- setdiff(union(core, add), drop)
        assign("core_objects", core, envir = env)
        return(invisible(core))
    }

    surplus <- setdiff(ls(envir = env), c(core, "core_objects"))
    rm(list = surplus, envir = env)
    message("crumber: removed ", length(surplus), " object(s)",
            if (length(surplus)) paste0(": ", paste(surplus, collapse = ", ")))
    if (do.gc) invisible(gc())

    if (notify) {
        if (requireNamespace("beepr", quietly = TRUE)) {
            beepr::beep(4)
        } else {
            message("crumber: install.packages(\"beepr\") to enable notify = TRUE.")
        }
    }
    invisible(surplus)
}
