#' Set up a shared or public dataset folder
#'
#' Datasets that are public, large, or used by more than one project live in
#' a datasets folder, `<datasets>/<source>/<name>/`, instead of inside a
#' single project. The datasets folder is the `datasets` setting (asked by
#' [toastmaker_setup()]) or, if unset,
#' `<root>/<home_base>/[<grant>/][<lab>/]Datasets/`, next to `Projects/`:
#'
#' ```
#' Datasets/
#' |-- public/                 downloaded from elsewhere
#' |   |-- GEO_GSE12345/
#' |   |   |-- README.md       where the data came from and how to get it again
#' |   |   |-- raw/            the data exactly as obtained (+ MANIFEST.md5)
#' |   |   |-- processed/      tables derived from raw/
#' |   |   `-- scripts/        download and processing code
#' |   `-- scRNAseq@pozniak/
#' `-- internal/               generated in-house
#' ```
#'
#' Every object returned by [project_setup()] lists all datasets in
#' `$datasets`, e.g. `toast$datasets$public$GEO_GSE12345`. A name of the
#' form `"<group>@<variant>"` is nested by group, so `"scRNAseq@pozniak"` is
#' reached as `toast$datasets$public$scRNAseq@pozniak` (or
#' `...$scRNAseq$pozniak`). The `@` form needs R >= 4.3.0.
#'
#' To record that a project uses a dataset, pass
#' `project_setup(..., datasets = "GEO_GSE12345")`; the link is written to
#' both folders' `.toastmaker` markers. Data generated for a single project
#' belongs in that project's `raw/` instead.
#'
#' Once `raw/` holds files, you are offered a checksum manifest (see
#' [data_manifest()]).
#'
#' @param name Dataset name, e.g. an accession (`"GEO_GSE12345"`), a
#'   `"<group>@<variant>"` name (`"scRNAseq@pozniak"`) or, for internal
#'   data, a date-first name (`"2026-03-12_scRNAseq-cohortA"`). If omitted,
#'   the dataset containing the working directory is used.
#' @param source `"public"` (downloaded from elsewhere) or `"internal"`
#'   (generated in-house).
#' @param datasets_dir Datasets folder. Default from settings; if unset,
#'   `Datasets/` in the base folder.
#' @inheritParams project_setup
#'
#' @return Invisibly, a `"toast"` object with `bd`, `dir_raw`,
#'   `dir_processed`, `dir_scripts` and `datasets`, or `NULL` if you
#'   declined.
#' @seealso [project_setup()]
#' @export
#' @examples
#' tmp <- tempfile()
#' d <- dataset_setup("GEO_GSE12345", home_base = "heiBOX", root = tmp,
#'                    confirm = FALSE, auto_cleanup = FALSE,
#'                    assign_global = FALSE)
#' p <- project_setup("Reanalysis", datasets = "GEO_GSE12345",
#'                    home_base = "heiBOX", root = tmp, confirm = FALSE,
#'                    auto_cleanup = FALSE, assign_global = FALSE)
#' p$datasets$public$GEO_GSE12345
#' unlink(tmp, recursive = TRUE)
dataset_setup <- function(name,
                          source        = c("public", "internal"),
                          datasets_dir  = .tm_setting("datasets"),
                          lab           = .tm_setting("lab"),
                          grant         = .tm_setting("grant"),
                          home_base     = .tm_setting("home_base"),
                          root          = .tm_setting("root"),
                          confirm       = interactive(),
                          check_raw     = TRUE,
                          auto_cleanup  = TRUE,
                          assign_global = FALSE) {
    source <- match.arg(source)
    gl <- .tm_grant_lab(grant, lab, missing(grant), missing(lab))
    .tm_setup(type = "dataset", name = if (missing(name)) NULL else name,
              args = list(source = source), datasets = NULL,
              datasets_dir = datasets_dir, lab = gl$lab, grant = gl$grant,
              home_base = home_base, root = root, confirm = confirm,
              check_raw = check_raw, auto_cleanup = auto_cleanup,
              assign_global = assign_global, env = parent.frame())
}

.tm_sources <- c("public", "internal")

# The folder that holds public/ and internal/ datasets: the `datasets`
# setting if given, else Datasets/ in the base folder `base`.
.tm_datasets_dir <- function(dir, base) {
    dir <- .tm_clean(dir)
    if (is.null(dir)) file.path(base, "Datasets") else .tm_path(path.expand(dir))
}

# Every dataset under `dir`, as list(public = ..., internal = ...) of paths.
# A name "group@variant" (e.g. "scRNAseq@pozniak") is nested as
# $group$variant, so that `toast$datasets$public$scRNAseq@pozniak` works.
.tm_list_datasets <- function(dir) {
    out <- lapply(.tm_sources, function(src) {
        paths <- list.dirs(file.path(dir, src), recursive = FALSE)
        paths <- gsub("\\\\", "/", paths)
        nms <- basename(paths)
        at <- grepl("@", nms, fixed = TRUE)
        groups <- sub("@.*$", "", nms[at])
        plain <- nms[!at]
        clash <- intersect(plain, groups)
        if (length(clash) > 0L) {
            warning("Datasets ", paste0("'", src, "/", clash, "'", collapse = ", "),
                    " clash with '<name>@...' datasets and are left out of $datasets$",
                    src, "; rename them.", call. = FALSE)
        }
        res <- as.list(paths[!at & !nms %in% clash])
        names(res) <- plain[!plain %in% clash]
        for (g in unique(groups)) {
            sel <- at & sub("@.*$", "", nms) == g
            members <- as.list(paths[sel])
            names(members) <- sub("^[^@]*@", "", nms[sel])
            res[[g]] <- structure(members, class = "toast_datasets")
        }
        structure(res[order(names(res))], class = "toast_datasets")
    })
    names(out) <- .tm_sources
    structure(out, class = "toast_datasets")
}

# `x@name` on a dataset list, e.g. `toast$datasets$public$scRNAseq@pozniak`.
# (S3 methods for `@` need R >= 4.3.0.)
#' @export
`@.toast_datasets` <- function(object, name) {
    if (!name %in% names(object)) {
        stop("No dataset '", name, "'. Available: ",
             if (length(object)) paste(names(object), collapse = ", ") else "none",
             ".", call. = FALSE)
    }
    object[[name]]
}

#' @export
print.toast_datasets <- function(x, ...) {
    flat <- .tm_flatten_datasets(x)
    if (length(flat) == 0L) {
        cat("<no datasets>\n")
    } else {
        for (nm in names(flat)) cat(sprintf("  %-28s %s\n", nm, flat[[nm]]))
    }
    invisible(x)
}

# Named character vector of dataset paths, named by how to reach them,
# e.g. "public$X" or "public$scRNAseq$pozniak".
.tm_flatten_datasets <- function(x, prefix = NULL) {
    out <- character(0L)
    for (nm in names(x)) {
        key <- paste(c(prefix, nm), collapse = "$")
        out <- c(out, if (is.list(x[[nm]])) .tm_flatten_datasets(x[[nm]], key) else
            structure(x[[nm]], names = key))
    }
    out
}

# Turn dataset names into dataset folders under `dir` (named by dataset).
# A name may include its source ("public/X"); otherwise both are searched.
.tm_resolve_datasets <- function(datasets, dir) {
    out <- vapply(datasets, function(nm) {
        cands <- if (grepl("/", nm, fixed = TRUE)) nm else file.path(.tm_sources, nm)
        hits <- file.path(dir, cands)
        hits <- hits[dir.exists(hits)]
        if (length(hits) == 0L) {
            stop("Dataset '", nm, "' not found in ", dir,
                 ". Create it with dataset_setup(\"", basename(nm), "\").", call. = FALSE)
        }
        if (length(hits) > 1L) {
            stop("Dataset '", nm, "' exists in both public/ and internal/; ",
                 "write \"public/", nm, "\" or \"internal/", nm, "\".", call. = FALSE)
        }
        hits
    }, "")
    names(out) <- basename(datasets)
    out
}

# Record which datasets a folder uses (Datasets: in its marker, relative to
# the datasets folder `dir`) and which folders use a dataset (UsedBy: in the
# dataset's marker, relative to the folder's base).
.tm_link_datasets <- function(bd, type, paths, dir) {
    me <- .tm_rel(bd, .tm_base(bd, type))
    rel <- vapply(paths, function(d) .tm_rel(d, dir), "")
    .tm_marker_add(bd, "Datasets", unname(rel))
    for (d in paths) .tm_marker_add(d, "UsedBy", me)
    invisible(NULL)
}
