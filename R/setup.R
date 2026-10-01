# Shared engine behind project_setup() and dataset_setup().
#
# `name` is NULL when the caller should be found from the working directory
# via its .toastmaker marker. `args` holds the layout-specific arguments
# (experiment_name, raw_subdir, ... for projects; source for datasets).
.tm_setup <- function(type, name, args, datasets, lab, grant, home_base, root,
                      confirm, check_raw, auto_cleanup, assign_global, env) {
    if (is.null(name)) {
        bd <- .tm_find_marker(".")
        if (is.null(bd)) {
            stop("A name is required, e.g. project_setup(\"MyProject\"), ",
                 "unless the working directory is inside an existing folder.",
                 call. = FALSE)
        }
        marker <- .tm_read_marker(bd)
        if (!is.null(type) && !identical(type, marker$Type)) {
            stop("The folder ", bd, " is a '", marker$Type, "', not a '", type, "'.",
                 call. = FALSE)
        }
        type <- marker$Type
        name <- marker$Name %||% basename(bd)
        home <- .tm_base(bd, type)
        for (f in c(marker$Lab, marker$Grant)) if (!is.na(f)) home <- dirname(home)
        if (identical(type, "dataset")) args$source <- marker$Source
    } else {
        if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
            stop("The name must be a single non-empty string.", call. = FALSE)
        }
        home_base <- .tm_require_home_base(home_base)
        home <- .tm_path(path.expand(root), home_base)
        bd <- .tm_path(home, grant, lab,
                       .tm_layouts[[type]]$top, args$source, name)
    }
    layout <- .tm_layouts[[type]]

    given <- names(Filter(Negate(is.null), args))
    bad <- setdiff(given, layout$args)
    if (length(bad) > 0L) {
        stop(paste(bad, collapse = ", "), " can only be used with type = \"project\".",
             call. = FALSE)
    }
    if (!is.null(args$experiment_name) &&
        !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", args$experiment_name)) {
        warning("experiment_name \"", args$experiment_name, "\" does not start with a ",
                "date (YYYY-MM-DD, e.g. \"", format(Sys.Date()), "_",
                args$experiment_name, "\").", call. = FALSE)
    }

    cc <- structure(c(list(bd = bd, dir_home = home), layout$dirs(bd, args)),
                    class = "toast", type = type)
    if (!is.null(datasets)) {
        cc$datasets <- .tm_resolve_datasets(datasets, .tm_base(bd, type))
    }
    dirs <- .tm_dirs(cc)

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

    .tm_write_marker(bd, type, name, lab = lab, grant = grant, source = args$source,
                     new = bd_new)
    readme <- file.path(bd, "README.md")
    if (!is.null(layout$readme) && !file.exists(readme)) {
        writeLines(layout$readme(name, args), readme)
        if (identical(type, "dataset")) {
            .tm_say("  + README.md written; please describe where the data came from: ",
                    readme, style = "32")
        }
    }
    if (isTRUE(check_raw)) {
        if (identical(type, "project") && !is.null(args$experiment_name)) {
            .tm_offer_raw_readme(cc, name, args$experiment_name, args$raw_subdir,
                                 .tm_read_marker(bd)$Lab)
            .tm_offer_manifest(cc$dir_raw)
        } else if (identical(type, "dataset")) {
            .tm_offer_manifest(cc$dir_raw)
        }
    }
    if (!is.null(cc$datasets)) .tm_link_datasets(cc, type)
    .tm_renv_hint(bd)

    if (isTRUE(assign_global)) {
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

# The project's own folder paths (its dir_* elements; `dir_home` is the
# surrounding home folder, not part of the project, so it is left out).
.tm_dirs <- function(cc) {
    x <- unclass(cc)
    unlist(x[startsWith(names(x), "dir_") & names(x) != "dir_home"])
}

# The base folder (<root>/<home_base>/[<grant>/][<lab>/]) of an item.
.tm_base <- function(bd, type) {
    up <- if (identical(type, "dataset")) 3L else 2L
    for (i in seq_len(up)) bd <- dirname(bd)
    bd
}

# `path` relative to `base`, or NULL if it is not inside it.
.tm_rel <- function(path, base) {
    path <- .tm_norm(path)
    base <- .tm_norm(base)
    if (identical(path, base)) return(".")
    if (!startsWith(path, paste0(base, "/"))) return(NULL)
    substring(path, nchar(base) + 2L)
}
