test_that("figure_setup creates a figure folder linked to its sources", {
    local_tm_settings()
    root <- local_root()
    p <- setup_quiet("KO", experiment_name = "2026-10-01_growth", analysis_subdir = "FlowJo",
                     root = root, check_raw = FALSE)
    paper <- setup_quiet("KO-screen", type = "paper", root = root)

    utils::capture.output(
        f <- figure_setup("Fig2_growth", sources = c(p$dir_analysis, p$dir_img),
                          where = paper)
    )
    expect_equal(f$dir_figure, file.path(paper$bd, "figures", "Fig2_growth"))
    expect_true(all(dir.exists(unlist(f))))
    readme <- readLines(file.path(f$dir_figure, "README.md"))
    expect_true("- `Projects/KO/analysis/FlowJo/2026-10-01_growth`" %in% readme)
    expect_true("- `Projects/KO/img/2026-10-01_growth`" %in% readme)

    pm <- read.dcf(file.path(p$bd, ".toastmaker"))
    expect_equal(unname(pm[1, "UsedIn"]), "Papers/KO-screen/figures/Fig2_growth")

    # more sources are added, existing ones are not repeated
    utils::capture.output(
        figure_setup("Fig2_growth", sources = c(p$dir_analysis, p$dir_raw), where = paper)
    )
    readme <- readLines(file.path(f$dir_figure, "README.md"))
    expect_equal(sum(grepl("analysis/FlowJo", readme)), 1)
    expect_true("- `Projects/KO/raw/2026-10-01_growth`" %in% readme)
    expect_lt(match("- `Projects/KO/raw/2026-10-01_growth`", readme),
              match("<!-- /toastmaker:sources -->", readme))
})

test_that("supplementary figures, slides, and the working directory", {
    local_tm_settings()
    root <- local_root()
    paper <- setup_quiet("KO-screen", type = "paper", root = root)
    utils::capture.output(s <- figure_setup("SuppFig1_gating", where = paper))
    expect_equal(dirname(s$dir_figure), file.path(paper$bd, "supplementary"))

    talk <- setup_quiet("2026-11-05_retreat", type = "presentation", root = root)
    withr::local_dir(talk$dir_slides)
    utils::capture.output(t <- figure_setup("Slide03_KOs"))
    expect_equal(t$dir_figure, file.path(.tm_norm(talk$bd), "figures", "Slide03_KOs"))

    p <- setup_quiet("KO", root = root)
    expect_error(figure_setup("Fig1", where = p), "works in papers")
})

test_that("sources outside the base folder are kept with a warning", {
    local_tm_settings()
    root <- local_root()
    paper <- setup_quiet("KO-screen", type = "paper", root = root)
    elsewhere <- local_root()
    expect_warning(
        utils::capture.output(figure_setup("Fig1", sources = elsewhere, where = paper)),
        "outside"
    )
})
