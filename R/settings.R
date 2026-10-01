#' View or change toastmaker settings
#'
#' Settings are the defaults used by [project_setup()]. They are stored in a
#' small file in [tools::R_user_dir()] (`"config"`), so they persist across
#' sessions. Nothing is set when the package is first installed: run
#' [toastmaker_setup()] for a guided setup, or set values directly here.
#'
#' Each setting is looked up in this order:
#'
#' 1. the R option `toastmaker.<name>` (e.g. set in `~/.Rprofile`),
#' 2. the settings file,
#' 3. the built-in default.
#'
#' Available settings:
#'
#' * `root`: folder that contains `home_base`. Default: `~/Documents`
#'   (`~` on Windows, where it already points to Documents).
#' * `home_base`: base folder inside `root`, typically a synced cloud folder
#'   such as `"heiBOX"`. **Required**; [project_setup()] will not run until it
#'   is set.
#' * `grant`: grant or grouping folder. Unset by default, in which case it is
#'   skipped in the path.
#' * `lab`: default lab folder. Unset by default (skipped). Not asked by
#'   [toastmaker_setup()]; set it here if you want one.
#' * `type`: default folder layout for [project_setup()]: `"project"`
#'   (default), `"paper"`, `"presentation"` or `"poster"`.
#'
#' @param ... Settings to change, as `name = value`. Use `NULL`, `NA` or `""`
#'   to unset a value.
#' @param reset If `TRUE`, delete all saved settings first.
#'
#' @return Invisibly, a named list of the effective settings.
#' @seealso [toastmaker_setup()]
#' @export
#' @examples
#' \dontrun{
#' toastmaker_settings()                         # show current settings
#' toastmaker_settings(home_base = "heiBOX", grant = "MyGrant")
#' toastmaker_settings(grant = NULL)             # unset the grant
#' }
toastmaker_settings <- function(..., reset = FALSE) {
    new <- list(...)
    if (length(new) > 0L) {
        nms <- names(new)
        if (is.null(nms) || any(!nzchar(nms))) {
            stop("Settings must be named, e.g. toastmaker_settings(grant = \"X\").",
                 call. = FALSE)
        }
        bad <- setdiff(nms, .tm_keys)
        if (length(bad) > 0L) {
            stop("Unknown setting(s): ", paste(bad, collapse = ", "),
                 ". Available: ", paste(.tm_keys, collapse = ", "), ".", call. = FALSE)
        }
    }

    if (isTRUE(reset)) unlink(.tm_config_path())
    if (length(new) > 0L) {
        cfg <- .tm_read_config()
        for (k in names(new)) {
            v <- .tm_clean(new[[k]])
            if (identical(k, "type") && !is.null(v)) v <- .tm_check_type(v)
            cfg[k] <- list(v)
        }
        .tm_write_config(cfg)
    }

    eff <- .tm_settings_table()
    cat("toastmaker settings (", .tm_config_path(), ")\n", sep = "")
    for (i in seq_len(nrow(eff))) {
        cat(sprintf("  %-10s %-30s [%s]\n", eff$setting[i],
                    if (is.na(eff$value[i])) "<unset>" else eff$value[i], eff$source[i]))
    }
    if (is.na(eff$value[eff$setting == "home_base"])) {
        .tm_say("home_base is not set: run toastmaker_setup() before project_setup().",
                style = "33")
    }
    out <- as.list(eff$value)
    names(out) <- eff$setting
    invisible(lapply(out, function(v) if (is.na(v)) NULL else v))
}

#' Guided setup of toastmaker defaults
#'
#' Walks you through the settings used by [project_setup()] (root folder,
#' base folder, grant and default layout) and saves them. Press Enter to
#' keep the value shown in brackets, or type `-` to clear an optional value.
#' Runs automatically the first time [project_setup()] is used interactively
#' without a `home_base`.
#'
#' @return Invisibly, `TRUE` if settings were saved, `FALSE` otherwise.
#' @seealso [toastmaker_settings()] to view or change single settings.
#' @export
toastmaker_setup <- function() {
    if (!.tm_interactive()) {
        stop("toastmaker_setup() needs an interactive session; use ",
             "toastmaker_settings(home_base = ...) instead.", call. = FALSE)
    }
    cur <- function(k) .tm_setting(k)

    cat("\ntoastmaker setup\n",
        "Press Enter to keep the value in [brackets]; type - to clear an optional value.\n\n",
        sep = "")

    cat("Root folder: the existing folder on this computer that holds your base folder.\n",
        "  Type a full path, e.g. ",
        if (.Platform$OS.type == "windows") "C:/Users/me/Documents" else "/home/me/Documents",
        ", or just ~ for your home/Documents folder.\n", sep = "")
    root <- .tm_prompt("Root folder (full path, or ~)", cur("root"), optional = FALSE)
    cat("  -> ", path.expand(root), "\n", sep = "")

    repeat {
        home_base <- .tm_prompt("Base folder inside the root, e.g. your synced cloud folder (required)",
                                cur("home_base") %||% "heiBOX", optional = FALSE)
        hb_path <- file.path(path.expand(root), home_base)
        if (dir.exists(hb_path) ||
            .tm_yes(paste0("  ", hb_path, " does not exist yet. Use it anyway?"),
                    default = FALSE)) break
    }

    grant <- .tm_prompt("Grant folder (Enter with no value = none)", cur("grant"))

    types <- .tm_types
    cat("Default folder layout:\n")
    for (i in seq_along(types)) {
        cat(sprintf("  %d. %-13s -> %s/\n", i, types[i], .tm_layouts[[types[i]]]$top))
    }
    repeat {
        ans <- .tm_prompt("Choose a number or name", cur("type"), optional = FALSE)
        if (grepl("^[0-9]+$", ans) && as.integer(ans) %in% seq_along(types)) {
            ans <- types[as.integer(ans)]
        }
        if (ans %in% types) break
        cat("  Please choose one of: ", paste(types, collapse = ", "), "\n", sep = "")
    }
    type <- ans

    cfg <- list(root = root, home_base = home_base, grant = grant,
                type = type)
    base <- do.call(.tm_path, unname(c(list(path.expand(root)), cfg[c("home_base", "grant")])))
    cat("\nSummary\n")
    for (k in names(cfg)) cat(sprintf("  %-10s %s\n", k, cfg[[k]] %||% "<none>"))
    cat("New folders will be created under:\n")
    for (t in types) cat("  ", file.path(base, .tm_layouts[[t]]$top), "/\n", sep = "")

    if (!.tm_yes("\nSave these settings?")) {
        cat("Nothing saved.\n")
        return(invisible(FALSE))
    }
    .tm_write_config(cfg)
    cat("Saved to ", .tm_config_path(), "\n", sep = "")
    invisible(TRUE)
}

# Internal ---------------------------------------------------------------------

.tm_keys <- c("root", "home_base", "grant", "lab", "type")

.tm_builtin_default <- function(key) {
    switch(key, root = .tm_default_root(), type = "project", NULL)
}

.tm_config_path <- function() {
    file.path(tools::R_user_dir("toastmaker", which = "config"), "settings.dcf")
}

.tm_read_config <- function() {
    f <- .tm_config_path()
    if (!file.exists(f)) return(list())
    m <- read.dcf(f)
    if (nrow(m) == 0L) return(list())
    cfg <- as.list(m[1L, ])
    Filter(Negate(is.null), lapply(cfg, .tm_clean))
}

.tm_write_config <- function(cfg) {
    cfg <- Filter(Negate(is.null), lapply(cfg, .tm_clean))
    f <- .tm_config_path()
    if (length(cfg) == 0L) {
        unlink(f)
        return(invisible(f))
    }
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    m <- matrix(unlist(cfg), nrow = 1L, dimnames = list(NULL, names(cfg)))
    write.dcf(m, f)
    invisible(f)
}

# NULL, NA and "" all mean "unset".
.tm_clean <- function(v) {
    if (is.null(v) || length(v) == 0L || is.na(v[1L]) || !nzchar(v[1L])) return(NULL)
    as.character(v[1L])
}

# Effective value of a setting: option > settings file > built-in default.
.tm_setting <- function(key) {
    opt <- getOption(paste0("toastmaker.", key))
    if (!is.null(opt)) return(.tm_clean(opt))
    cfg <- .tm_read_config()[[key]]
    if (!is.null(cfg)) return(cfg)
    .tm_builtin_default(key)
}

.tm_settings_table <- function() {
    cfg <- .tm_read_config()
    src <- vapply(.tm_keys, function(k) {
        if (!is.null(getOption(paste0("toastmaker.", k)))) "option"
        else if (!is.null(cfg[[k]])) "settings file"
        else if (!is.null(.tm_builtin_default(k))) "default"
        else "-"
    }, "")
    val <- vapply(.tm_keys, function(k) .tm_setting(k) %||% NA_character_, "")
    data.frame(setting = .tm_keys, value = unname(val), source = unname(src))
}

# home_base must be set before anything is created. In an interactive session
# the setup wizard is offered; otherwise this is an error.
.tm_require_home_base <- function(home_base) {
    if (!is.null(.tm_clean(home_base))) return(home_base)
    if (.tm_interactive()) {
        cat("No base folder (home_base) is set yet; starting toastmaker_setup().\n")
        toastmaker_setup()
        home_base <- .tm_setting("home_base")
        if (!is.null(home_base)) return(home_base)
    }
    stop("No base folder set. Run toastmaker_setup(), or ",
         "toastmaker_settings(home_base = \"...\"), or pass home_base = \"...\".",
         call. = FALSE)
}

# Grant and lab are each optional. When the caller sets one explicitly, the
# other is not filled in from the saved settings (so no unwanted lab appears).
.tm_grant_lab <- function(grant, lab, grant_missing, lab_missing) {
    if (grant_missing && !lab_missing) grant <- NULL
    if (lab_missing && !grant_missing) lab <- NULL
    list(grant = .tm_clean(grant), lab = .tm_clean(lab))
}

# Prompt with a current value; Enter keeps it, "-" clears it (optional only).
.tm_prompt <- function(label, current = NULL, optional = TRUE) {
    repeat {
        shown <- if (is.null(current)) "" else paste0(" [", current, "]")
        ans <- trimws(.tm_readline(paste0(label, shown, ": ")))
        if (!nzchar(ans)) ans <- current %||% ""
        if (identical(ans, "-")) ans <- ""
        if (nzchar(ans) || optional) return(.tm_clean(ans))
        cat("  A value is required.\n")
    }
}
