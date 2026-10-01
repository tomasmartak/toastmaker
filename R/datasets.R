#' Set up a shared or public dataset folder
#'
#' Datasets that are public, large, or used by more than one project live in
#' `<root>/<home_base>/[<grant>/][<lab>/]Datasets/<source>/<name>/`, next to
#' `Projects/`, instead of inside a single project:
#'
#' ```
#' Datasets/public/GEO_GSE12345/
#' |-- README.md     where the data came from and how to get it again
#' |-- raw/          the data exactly as obtained (+ MANIFEST.md5)
#' |-- processed/    tables derived from raw/
#' `-- scripts/      download and processing code
#' ```
#'
#' A project uses a dataset with `project_setup(..., datasets = "GEO_GSE12345")`,
#' which returns its path in `$datasets` and records the link in both
#' folders' `.toastmaker` markers. Data generated for a single project
#' belongs in that project's `raw/` instead.
#'
#' Once `raw/` holds files, you are offered a checksum manifest (see
#' [data_manifest()]).
#'
#' @param name Dataset name, e.g. an accession (`"GEO_GSE12345"`) or, for
#'   internal data, a date-first name (`"2026-03-12_scRNAseq-cohortA"`). If
#'   omitted, the dataset containing the working directory is used.
#' @param source `"public"` (downloaded from elsewhere) or `"internal"`
#'   (generated in-house).
#' @inheritParams project_setup
#'
#' @return Invisibly, a `"cookiecutter"` object with `bd`, `dir_raw`,
#'   `dir_processed` and `dir_scripts`, or `NULL` if you declined.
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
#' p$datasets$GEO_GSE12345
#' unlink(tmp, recursive = TRUE)
dataset_setup <- function(name,
                          source        = c("public", "internal"),
                          lab           = .tm_setting("lab"),
                          grant         = .tm_setting("grant"),
                          home_base     = .tm_setting("home_base"),
                          root          = .tm_setting("root"),
                          confirm       = interactive(),
                          check_raw     = TRUE,
                          auto_cleanup  = TRUE,
                          assign_global = TRUE) {
    source <- match.arg(source)
    .tm_setup(type = "dataset", name = if (missing(name)) NULL else name,
              args = list(source = source), datasets = NULL, lab = lab, grant = grant,
              home_base = home_base, root = root, confirm = confirm,
              check_raw = check_raw, auto_cleanup = auto_cleanup,
              assign_global = assign_global, env = parent.frame())
}

# Turn dataset names into a named list of dataset folders under `base`.
# A name may include its source ("public/X"); otherwise both are searched.
.tm_resolve_datasets <- function(datasets, base) {
    out <- lapply(datasets, function(nm) {
        cands <- if (grepl("/", nm, fixed = TRUE)) nm else
            file.path(c("public", "internal"), nm)
        hits <- file.path(base, "Datasets", cands)
        hits <- hits[dir.exists(hits)]
        if (length(hits) == 0L) {
            stop("Dataset '", nm, "' not found in ", file.path(base, "Datasets"),
                 ". Create it with dataset_setup(\"", basename(nm), "\").", call. = FALSE)
        }
        if (length(hits) > 1L) {
            stop("Dataset '", nm, "' exists in both public/ and internal/; ",
                 "write \"public/", nm, "\" or \"internal/", nm, "\".", call. = FALSE)
        }
        hits
    })
    names(out) <- basename(datasets)
    out
}

# Record which datasets a folder uses (Datasets: in its marker) and which
# folders use a dataset (UsedBy: in the dataset's marker), as paths relative
# to the base folder.
.tm_link_datasets <- function(cc, type) {
    base <- .tm_base(cc$bd, type)
    me <- .tm_rel(cc$bd, base)
    rel <- vapply(cc$datasets, function(d) substring(.tm_rel(d, base), nchar("Datasets/") + 1L), "")
    .tm_marker_add(cc$bd, "Datasets", unname(rel))
    for (d in cc$datasets) .tm_marker_add(d, "UsedBy", me)
    invisible(NULL)
}
