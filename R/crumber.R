#' Clear the workspace, keeping protected objects
#'
#' Removes every object from `env` except:
#'
#' * objects named in `core_objects` (add to it with `add.to.cookiejar`),
#' * results of [project_setup()] and [dataset_setup()] (objects of class
#'   `"toast"`), so `toast <- project_setup("X")` survives,
#' * hidden objects (names starting with `.`).
#'
#' `core_objects` is created by the first `add.to.cookiejar`, or by
#' `project_setup(..., assign_global = TRUE)`, which protects the paths it
#' assigns.
#'
#' Object names can be given as bare names (`crumber(add.to.cookiejar = df)`),
#' strings (`"df"`), or `c()` of either (`c(df, fit)`).
#'
#' `crumber()` is meant for freeing memory and decluttering during
#' interactive work. It is not a substitute for a fresh R session: loaded
#' packages, options, the working directory and the random seed all survive
#' it. To check that a script is reproducible, restart R and run it top to
#' bottom.
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
#' assign("keep_me", 1, envir = env)
#' assign("scratch", 2, envir = env)
#' crumber(add.to.cookiejar = c(keep_me, scratch), env = env) # protect both
#' crumber(consume.cookie = "scratch", env = env)  # ...and unprotect `scratch`
#' crumber(env = env, do.gc = FALSE)               # removes `scratch`
#' ls(env)
crumber <- function(do.gc = TRUE, add.to.cookiejar = NULL, consume.cookie = NULL,
                    env = globalenv(), notify = FALSE) {
    add <- .tm_names(substitute(add.to.cookiejar))
    drop <- .tm_names(substitute(consume.cookie))
    core <- get0("core_objects", envir = env, inherits = FALSE) %||% character(0L)

    if (length(add) > 0L || length(drop) > 0L) {
        core <- setdiff(union(core, add), drop)
        assign("core_objects", core, envir = env)
        return(invisible(core))
    }

    objs <- ls(envir = env)
    toasts <- objs[vapply(objs, function(o) inherits(get(o, envir = env), "toast"), NA)]
    surplus <- setdiff(objs, c(core, toasts, "core_objects"))
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
