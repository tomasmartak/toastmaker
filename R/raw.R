#' Create a checksum manifest for a data folder
#'
#' Writes `MANIFEST.md5` listing an MD5 checksum for every file in `dir`
#' (recursively). Use [data_verify()] later to check that nothing has
#' changed. The file uses the standard `md5sum` format, so it can also be
#' checked without R (`md5sum -c MANIFEST.md5`). `README.md` and the manifest
#' itself are not included, so the README can be edited freely.
#'
#' @param dir A folder, or a `"cookiecutter"` project from [project_setup()]
#'   (its `dir_raw` is used).
#' @param overwrite Replace an existing manifest (default `FALSE`).
#'
#' @return Invisibly, the path of the manifest.
#' @seealso [data_verify()]
#' @export
#' @examples
#' tmp <- tempfile(); dir.create(tmp)
#' writeLines("1,2,3", file.path(tmp, "counts.csv"))
#' data_manifest(tmp)
#' data_verify(tmp)
#' unlink(tmp, recursive = TRUE)
data_manifest <- function(dir, overwrite = FALSE) {
    dir <- .tm_data_dir(dir)
    f <- file.path(dir, .tm_manifest_name)
    if (file.exists(f) && !isTRUE(overwrite)) {
        stop(f, " already exists; use overwrite = TRUE to replace it.", call. = FALSE)
    }
    files <- .tm_data_files(dir)
    sums <- tools::md5sum(file.path(dir, files))
    writeLines(paste0(unname(sums), "  ", files), f, useBytes = TRUE)
    .tm_say("Wrote checksums for ", length(files), " file(s) to ", f, style = "32")
    invisible(f)
}

#' Check a data folder against its checksum manifest
#'
#' Re-computes the checksums of the files in `dir` and compares them with
#' `MANIFEST.md5` written by [data_manifest()].
#'
#' @inheritParams data_manifest
#'
#' @return Invisibly, a data frame with columns `file` and `status`
#'   (`"ok"`, `"changed"`, `"missing"` or `"new"`).
#' @seealso [data_manifest()]
#' @export
data_verify <- function(dir) {
    dir <- .tm_data_dir(dir)
    f <- file.path(dir, .tm_manifest_name)
    if (!file.exists(f)) {
        stop("No ", .tm_manifest_name, " in ", dir, "; create one with data_manifest().",
             call. = FALSE)
    }
    lines <- readLines(f, warn = FALSE)
    lines <- lines[nzchar(lines)]
    expected <- stats::setNames(sub("^([0-9a-f]+) [ *].*$", "\\1", lines),
                                sub("^[0-9a-f]+ [ *]", "", lines))

    present <- .tm_data_files(dir)
    listed <- names(expected)
    both <- intersect(listed, present)
    actual <- unname(tools::md5sum(file.path(dir, both)))

    out <- data.frame(
        file   = c(both, setdiff(listed, present), setdiff(present, listed)),
        status = c(ifelse(actual == expected[both], "ok", "changed"),
                   rep("missing", length(setdiff(listed, present))),
                   rep("new", length(setdiff(present, listed))))
    )
    n_ok <- sum(out$status == "ok")
    if (n_ok == nrow(out)) {
        .tm_say("All ", n_ok, " file(s) match ", f, style = "32")
    } else {
        .tm_say(nrow(out) - n_ok, " of ", nrow(out), " file(s) differ from ", f, ":",
                style = "33")
        bad <- out[out$status != "ok", ]
        for (i in seq_len(nrow(bad))) .tm_say(sprintf("  %-8s %s", bad$status[i], bad$file[i]))
    }
    invisible(out)
}

# Internal ---------------------------------------------------------------------

.tm_manifest_name <- "MANIFEST.md5"

.tm_data_dir <- function(dir) {
    if (inherits(dir, "cookiecutter")) {
        if (is.null(dir$dir_raw)) stop("This project has no 'dir_raw'.", call. = FALSE)
        dir <- dir$dir_raw
    }
    if (!is.character(dir) || length(dir) != 1L || !dir.exists(dir)) {
        stop("'dir' must be an existing folder.", call. = FALSE)
    }
    dir
}

# Data files, relative to `dir`, excluding the README and the manifest.
.tm_data_files <- function(dir) {
    files <- list.files(dir, recursive = TRUE, all.files = TRUE)
    sort(files[!files %in% c("README.md", .tm_manifest_name)])
}

.tm_raw_readme <- function(cc, project, experiment_name, raw_subdir, lab) {
    rel <- function(d) substring(d, nchar(cc$bd) + 2L)
    date <- regmatches(experiment_name, regexpr("^[0-9]{4}-[0-9]{2}-[0-9]{2}", experiment_name))
    row <- function(k, v = "") sprintf("| %-11s | %s |", k, v)
    c(paste0("# Raw data: ", experiment_name),
      "",
      "| Field       | Value |",
      "|-------------|-------|",
      row("Experiment", experiment_name),
      row("Date", if (length(date)) date else ""),
      row("Technique", raw_subdir %||% ""),
      row("Project", project),
      row("Lab", lab %||% ""),
      row("Operator"),
      row("Instrument"),
      "",
      "## Description",
      "",
      "<!-- What was measured, and why. -->",
      "",
      "## Samples / conditions",
      "",
      "<!-- One line per sample or condition, or name the sample sheet in this folder. -->",
      "",
      "## Protocol / instrument settings",
      "",
      "<!-- Or link to the protocol used. -->",
      "",
      "## Files",
      "",
      "<!-- How the files are named and what each contains. Checksums are in MANIFEST.md5",
      "     (create with toastmaker::data_manifest(), check with toastmaker::data_verify()). -->",
      "",
      "## Related folders (relative to the project root)",
      "",
      paste0("- Analysis: `", rel(cc$dir_analysis), "`"),
      paste0("- Figures: `", rel(cc$dir_img), "`"),
      paste0("- Notes: `", rel(cc$dir_doc), "`"),
      "",
      "## Notes",
      "")
}

# Offer a README for a raw experiment folder, and a checksum manifest once it
# holds data. Declined offers are not repeated in the same session.
.tm_check_raw <- function(cc, project, experiment_name, raw_subdir, lab) {
    d <- cc$dir_raw
    readme <- file.path(d, "README.md")
    manifest <- file.path(d, .tm_manifest_name)
    ask <- .tm_interactive()

    if (!file.exists(readme) && !(paste0("readme:", d) %in% .tm_state$declined)) {
        if (ask && .tm_yes(paste0("No README.md in ", d, ". Create one from the template?"))) {
            writeLines(.tm_raw_readme(cc, project, experiment_name, raw_subdir, lab), readme)
            .tm_say("  + README.md written; please fill it in: ", readme, style = "32")
        } else {
            if (!ask) .tm_say("Note: ", d, " has no README.md describing the data.", style = "3")
            .tm_state$declined <- c(.tm_state$declined, paste0("readme:", d))
        }
    }

    if (length(.tm_data_files(d)) > 0L && !file.exists(manifest) &&
        !(paste0("manifest:", d) %in% .tm_state$declined)) {
        if (ask && .tm_yes(paste0(d, " holds ", length(.tm_data_files(d)),
                                  " file(s) but no checksum manifest. Create one?"),
                           default = FALSE)) {
            data_manifest(d)
        } else {
            if (!ask) .tm_say("Note: ", d, " has no checksum manifest (see data_manifest()).",
                              style = "3")
            .tm_state$declined <- c(.tm_state$declined, paste0("manifest:", d))
        }
    }
    invisible(NULL)
}
