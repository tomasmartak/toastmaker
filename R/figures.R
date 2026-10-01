#' Add a figure folder to a paper, presentation or poster
#'
#' Creates one folder per figure, holding the final figure, its `panels/`,
#' the numbers it plots (`source_data/`) and a `README.md` that names the
#' projects, experiments or datasets it was made from:
#'
#' ```
#' Papers/2026_KO-screen/figures/Fig2_growth/
#' |-- README.md       made from: Projects/KO/analysis/FlowJo/2026-10-01_growth, ...
#' |-- panels/
#' `-- source_data/
#' ```
#'
#' Source paths are written relative to the base folder, so the links work
#' on any computer that syncs it. The figure is also added to the `UsedIn:`
#' field of each source project's `.toastmaker` marker, so you can see from a
#' project where its results ended up. Calling `figure_setup()` again with
#' more sources adds them to the README.
#'
#' In a paper, figures whose name starts with `Supp` go into
#' `supplementary/`; all others go into `figures/`.
#'
#' @param figure Figure folder name, e.g. `"Fig2_growth"`,
#'   `"SuppFig1_gating"`, `"Slide03_knockouts"` or `"Panel1_overview"`.
#' @param sources Folders or files the figure was made from, e.g.
#'   `c(p$dir_analysis, p$dir_img)`.
#' @param where The paper, presentation or poster: a `"toast"` from
#'   [project_setup()] or a path inside it. Default: the working directory.
#'
#' @return Invisibly, a list with `dir_figure`, `dir_panels` and
#'   `dir_source_data`.
#' @seealso [project_setup()]
#' @export
#' @examples
#' tmp <- tempfile()
#' p <- project_setup("KO", experiment_name = "2026-10-01_growth",
#'                    home_base = "heiBOX", root = tmp, confirm = FALSE,
#'                    check_raw = FALSE, auto_cleanup = FALSE,
#'                    assign_global = FALSE)
#' paper <- project_setup("2026_KO-screen", type = "paper", home_base = "heiBOX",
#'                        root = tmp, confirm = FALSE, auto_cleanup = FALSE,
#'                        assign_global = FALSE)
#' f <- figure_setup("Fig2_growth", sources = p$dir_analysis, where = paper)
#' readLines(file.path(f$dir_figure, "README.md"))
#' unlink(tmp, recursive = TRUE)
figure_setup <- function(figure, sources = NULL, where = ".") {
    if (missing(figure) || !is.character(figure) || length(figure) != 1L ||
        !nzchar(figure)) {
        stop("'figure' must be a single name, e.g. \"Fig2_growth\".", call. = FALSE)
    }
    bd <- if (inherits(where, "toast")) where$bd else project_root(where)
    bd <- .tm_norm(bd)
    marker <- .tm_read_marker(bd)
    type <- marker$Type
    if (!type %in% c("paper", "presentation", "poster")) {
        stop(bd, " is a ", type, "; figure_setup() works in papers, presentations ",
             "and posters.", call. = FALSE)
    }
    parent <- if (identical(type, "paper") && startsWith(figure, "Supp"))
        "supplementary" else "figures"
    fig <- file.path(bd, parent, figure)
    out <- list(dir_figure      = fig,
                dir_panels      = file.path(fig, "panels"),
                dir_source_data = file.path(fig, "source_data"))
    for (d in out) dir.create(d, recursive = TRUE, showWarnings = FALSE)

    base <- .tm_base(bd, type)
    fig_rel <- .tm_rel(fig, base)
    src <- character(0L)
    for (s in sources) {
        if (!file.exists(s)) warning("Source does not exist: ", s, call. = FALSE)
        rel <- .tm_rel(s, base)
        if (is.null(rel)) {
            warning("Source ", s, " is outside ", base, "; its absolute path is ",
                    "recorded, which may not work on other computers.", call. = FALSE)
            rel <- .tm_norm(s)
        }
        src <- c(src, rel)
        owner <- if (file.exists(s)) .tm_find_marker(s)
        if (!is.null(owner) && !identical(owner, bd)) .tm_marker_add(owner, "UsedIn", fig_rel)
    }

    readme <- file.path(fig, "README.md")
    if (!file.exists(readme)) {
        writeLines(.tm_figure_readme(figure, type, marker$Name, src), readme)
        .tm_say("  + created ", fig_rel, "/", style = "32")
    } else {
        .tm_add_sources(readme, src)
        .tm_say("  = updated ", fig_rel, "/README.md", style = "90")
    }
    invisible(out)
}

.tm_src_open <- "<!-- toastmaker:sources -->"
.tm_src_close <- "<!-- /toastmaker:sources -->"

.tm_src_lines <- function(src) if (length(src)) paste0("- `", src, "`") else character(0L)

.tm_figure_readme <- function(figure, type, name, src) {
    c(paste0("# ", figure),
      "",
      paste0("Figure folder in the ", type, " ", name, "."),
      "",
      "- this folder: the final figure",
      "- `panels/`: the individual panels",
      "- `source_data/`: the numbers plotted, one file per panel",
      "",
      "## Made from",
      "",
      "Paths are relative to the base folder that contains Projects/, Papers/, ...",
      "",
      .tm_src_open,
      .tm_src_lines(src),
      .tm_src_close,
      "",
      "## Script",
      "",
      "<!-- Which script in scripts/ makes this figure. -->",
      "",
      "## Notes",
      "")
}

# Add sources not yet listed to an existing figure README.
.tm_add_sources <- function(readme, src) {
    lines <- readLines(readme, warn = FALSE)
    new <- setdiff(.tm_src_lines(src), lines)
    if (length(new) == 0L) return(invisible(FALSE))
    end <- match(.tm_src_close, lines)
    if (is.na(end)) {
        lines <- c(lines, "", "## Made from", "", .tm_src_open, new, .tm_src_close)
    } else {
        lines <- append(lines, new, after = end - 1L)
    }
    writeLines(lines, readme)
    invisible(TRUE)
}
