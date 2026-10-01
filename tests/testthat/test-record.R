test_that("project_record writes a lockfile for packages used in scripts/", {
    skip_if_not_installed("renv")
    local_tm_settings()
    root <- local_root()
    p <- setup_quiet("KO", root = root)
    writeLines(c("library(withr)", "x <- stats::median(1)"),
               file.path(p$dir_scripts, "analysis.R"))

    utils::capture.output(lock <- project_record(p))
    expect_equal(lock, file.path(p$bd, "renv.lock"))
    pkgs <- names(renv::lockfile_read(lock)$Packages)
    expect_true("withr" %in% pkgs)
    expect_false("stats" %in% pkgs)
    # only the lockfile is written, no library
    expect_false(dir.exists(file.path(p$bd, "renv")))
})

test_that("project_setup reminds about missing or outdated lockfiles", {
    local_tm_settings()
    root <- local_root()
    p <- setup_quiet("KO", root = root)
    writeLines("library(withr)", file.path(p$dir_scripts, "a.R"))
    run <- function() utils::capture.output(
        project_setup("KO", root = root, confirm = FALSE, assign_global = FALSE)
    )
    expect_true(any(grepl("not recorded yet", run())))
    expect_false(any(grepl("not recorded", run())))  # once per session
})
