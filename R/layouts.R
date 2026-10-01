# Folder layouts ---------------------------------------------------------------
#
# Each layout lives in its own top-level folder under
# <root>/<home_base>/[<grant>/][<lab>/] and defines the subdirectories that
# are created. `dirs` receives the item folder `bd` and the layout-specific
# arguments `a` (only those named in `args` are allowed). `readme` (if
# present) returns the lines of README.md written in `bd`, so that anyone
# browsing the folder knows what goes where.

.tm_layouts <- list(
    project = list(
        top  = "Projects",
        args = c("experiment_name", "analysis_subdir", "raw_subdir", "img_subdir"),
        dirs = function(bd, a) list(
            dir_analysis = .tm_path(bd, "analysis", a$analysis_subdir, a$experiment_name),
            dir_raw      = .tm_path(bd, "raw", a$raw_subdir, a$experiment_name),
            dir_img      = .tm_path(bd, "img", a$img_subdir, a$experiment_name),
            dir_doc      = .tm_path(bd, "doc", a$experiment_name),
            dir_scripts  = file.path(bd, "scripts")
        )
    ),
    paper = list(
        top  = "Papers",
        figure_example = "Fig1_<short-title>",
        dirs = function(bd, a) list(
            dir_manuscript    = file.path(bd, "manuscript"),
            dir_figures       = file.path(bd, "figures"),
            dir_supplementary = file.path(bd, "supplementary"),
            dir_scripts       = file.path(bd, "scripts"),
            dir_submission    = file.path(bd, "submission")
        ),
        readme = function(name, a) .tm_guide_text("paper", name, c(
            "manuscript/    Text drafts and versions of the paper.",
            "figures/       One folder per main figure, e.g. Fig1_<short-title>/, holding",
            "               the final figure, panels/, source_data/ (the numbers plotted)",
            "               and a README.md naming the projects/experiments it came from.",
            "supplementary/ Supplementary figures and tables, organised like figures/,",
            "               e.g. SuppFig1_<short-title>/, SuppTable1_<short-title>/.",
            "scripts/       Code that assembles the figures and tables.",
            "submission/    Everything sent to or received from the journal: cover",
            "               letter, submitted versions, reviews and responses."
        ))
    ),
    presentation = list(
        top  = "Presentations",
        figure_example = "Slide03_<short-title>",
        dirs = function(bd, a) list(
            dir_slides  = file.path(bd, "slides"),
            dir_figures = file.path(bd, "figures"),
            dir_scripts = file.path(bd, "scripts"),
            dir_notes   = file.path(bd, "notes")
        ),
        readme = function(name, a) .tm_guide_text("presentation", name, c(
            "slides/   The slide deck(s), final version and drafts.",
            "figures/  One folder per slide that shows results, e.g. Slide03_<short-title>/,",
            "          holding the image, panels/, source_data/ and a README.md naming",
            "          the projects/experiments it came from.",
            "scripts/  Code that makes the figures.",
            "notes/    Speaker notes, outline, feedback received."
        ))
    ),
    poster = list(
        top  = "Posters",
        figure_example = "Panel1_<short-title>",
        dirs = function(bd, a) list(
            dir_poster   = file.path(bd, "poster"),
            dir_abstract = file.path(bd, "abstract"),
            dir_figures  = file.path(bd, "figures"),
            dir_scripts  = file.path(bd, "scripts")
        ),
        readme = function(name, a) .tm_guide_text("poster", name, c(
            "poster/    The poster file(s), final version and drafts.",
            "abstract/  The submitted abstract and conference correspondence.",
            "figures/   One folder per poster panel, e.g. Panel1_<short-title>/, holding",
            "           the image, panels/, source_data/ and a README.md naming the",
            "           projects/experiments it came from.",
            "scripts/   Code that makes the figures."
        ))
    ),
    dataset = list(
        top  = "Datasets",
        args = "source",
        dirs = function(bd, a) list(
            dir_raw       = file.path(bd, "raw"),
            dir_processed = file.path(bd, "processed"),
            dir_scripts   = file.path(bd, "scripts")
        ),
        readme = function(name, a) .tm_dataset_readme(name, a$source)
    )
)

# Layouts that project_setup() can create (datasets have dataset_setup()).
.tm_types <- c("project", "paper", "presentation", "poster")

.tm_check_type <- function(type) {
    if (!is.character(type) || length(type) != 1L || !type %in% .tm_types) {
        stop("'type' must be one of: ", paste(.tm_types, collapse = ", "),
             ".", call. = FALSE)
    }
    type
}

# README.md written at the root of a paper/presentation/poster folder.
.tm_guide_text <- function(type, name, guide) {
    c(paste0("# ", name),
      "",
      paste0("This is a ", type, " folder created with toastmaker on ",
             format(Sys.Date()), "."),
      "",
      "## What is where",
      "",
      "```",
      guide,
      "```",
      "")
}

.tm_dataset_readme <- function(name, source) {
    row <- function(k, v = "") sprintf("| %-15s | %s |", k, v)
    public <- identical(source, "public")
    c(paste0("# Dataset: ", name),
      "",
      "| Field           | Value |",
      "|-----------------|-------|",
      row("Dataset", name),
      row("Source type", source),
      row(if (public) "Accession / URL" else "Generated by"),
      row(if (public) "Downloaded on" else "Date"),
      row("Version"),
      row("Licence"),
      row("Citation"),
      "",
      "## Description",
      "",
      "<!-- What the data are: organism, samples, assay, size. -->",
      "",
      "## How to obtain it again",
      "",
      if (public) "<!-- The download script in scripts/, or the exact commands/URLs. -->"
      else "<!-- Where the original lives and who to ask. -->",
      "",
      "## What is where",
      "",
      "```",
      "raw/        The data exactly as obtained. Never edited; checksums in",
      "            raw/MANIFEST.md5 (toastmaker::data_manifest() / data_verify()).",
      "processed/  Tables derived from raw/, made by the scripts in scripts/.",
      "scripts/    Download and processing code.",
      "```",
      "",
      "## Used by",
      "",
      "<!-- Projects using this dataset are listed in the hidden .toastmaker file",
      "     (UsedBy: field). -->",
      "")
}
