# Folder layouts ---------------------------------------------------------------
#
# Each layout lives in its own top-level folder under
# <root>/<home_base>/[<grant>/][<lab>/] and defines the subdirectories that
# project_setup() creates. `dirs` receives the item folder `bd` and the
# project-only arguments `a`; `guide` (if present) is written to README.md in
# `bd` so that anyone browsing the folder knows what goes where.

.tm_layouts <- list(
    project = list(
        top  = "Projects",
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
        dirs = function(bd, a) list(
            dir_manuscript    = file.path(bd, "manuscript"),
            dir_figures       = file.path(bd, "figures"),
            dir_supplementary = file.path(bd, "supplementary"),
            dir_scripts       = file.path(bd, "scripts"),
            dir_submission    = file.path(bd, "submission")
        ),
        guide = c(
            "manuscript/    Text drafts and versions of the paper.",
            "figures/       One folder per main figure, e.g. Fig1_<short-title>/, holding",
            "               the final figure, panels/, source_data/ (the numbers plotted)",
            "               and a README.md naming the projects/experiments it came from.",
            "supplementary/ Supplementary figures and tables, organised like figures/,",
            "               e.g. SuppFig1_<short-title>/, SuppTable1_<short-title>/.",
            "scripts/       Code that assembles the figures and tables.",
            "submission/    Everything sent to or received from the journal: cover",
            "               letter, submitted versions, reviews and responses."
        )
    ),
    presentation = list(
        top  = "Presentations",
        dirs = function(bd, a) list(
            dir_slides  = file.path(bd, "slides"),
            dir_figures = file.path(bd, "figures"),
            dir_scripts = file.path(bd, "scripts"),
            dir_notes   = file.path(bd, "notes")
        ),
        guide = c(
            "slides/   The slide deck(s), final version and drafts.",
            "figures/  One folder per slide that shows results, e.g. Slide03_<short-title>/,",
            "          holding the image, source_data/ and a README.md naming the",
            "          projects/experiments it came from.",
            "scripts/  Code that makes the figures.",
            "notes/    Speaker notes, outline, feedback received."
        )
    ),
    poster = list(
        top  = "Posters",
        dirs = function(bd, a) list(
            dir_poster   = file.path(bd, "poster"),
            dir_abstract = file.path(bd, "abstract"),
            dir_figures  = file.path(bd, "figures"),
            dir_scripts  = file.path(bd, "scripts")
        ),
        guide = c(
            "poster/    The poster file(s), final version and drafts.",
            "abstract/  The submitted abstract and conference correspondence.",
            "figures/   One folder per poster panel, e.g. Panel1_<short-title>/, holding",
            "           the image, source_data/ and a README.md naming the",
            "           projects/experiments it came from.",
            "scripts/   Code that makes the figures."
        )
    )
)

.tm_check_type <- function(type) {
    if (!is.character(type) || length(type) != 1L || !type %in% names(.tm_layouts)) {
        stop("'type' must be one of: ", paste(names(.tm_layouts), collapse = ", "),
             ".", call. = FALSE)
    }
    type
}

# README.md written at the root of a paper/presentation/poster folder.
.tm_guide_text <- function(type, name) {
    c(paste0("# ", name),
      "",
      paste0("This is a ", type, " folder created with toastmaker on ",
             format(Sys.Date()), "."),
      "",
      "## What is where",
      "",
      "```",
      .tm_layouts[[type]]$guide,
      "```",
      "")
}
