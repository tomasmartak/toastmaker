test_that("data_manifest and data_verify detect changes", {
    d <- withr::local_tempdir()
    withr::local_options(toastmaker.color = FALSE)
    writeLines("1", file.path(d, "a.csv"))
    dir.create(file.path(d, "sub"))
    writeLines("2", file.path(d, "sub", "b.csv"))
    writeLines("readme", file.path(d, "README.md"))

    utils::capture.output(data_manifest(d))
    lines <- readLines(file.path(d, "MANIFEST.md5"))
    expect_length(lines, 2)
    expect_match(lines, "^[0-9a-f]{32}  (a\\.csv|sub/b\\.csv)$")
    expect_error(data_manifest(d), "already exists")

    utils::capture.output(res <- data_verify(d))
    expect_true(all(res$status == "ok"))

    writeLines("changed", file.path(d, "a.csv"))
    unlink(file.path(d, "sub", "b.csv"))
    writeLines("3", file.path(d, "c.csv"))
    writeLines("edited", file.path(d, "README.md"))
    utils::capture.output(res <- data_verify(d))
    expect_equal(res$status[match(c("a.csv", "sub/b.csv", "c.csv"), res$file)],
                 c("changed", "missing", "new"))
    expect_equal(nrow(res), 3)
})

test_that("project_setup offers a raw README, then a manifest once data exists", {
    local_tm_settings()
    root <- local_root()
    args <- list("KO", lab = "AC", experiment_name = "2025-01-01_exp1", raw_subdir = "FC",
                 root = root, confirm = FALSE, auto_cleanup = FALSE, assign_global = FALSE)

    local_answers("y")
    utils::capture.output(p <- do.call(project_setup, args))
    readme <- readLines(file.path(p$dir_raw, "README.md"))
    expect_true(any(grepl("| Technique   | FC |", readme, fixed = TRUE)))
    expect_true(any(grepl("| Date        | 2025-01-01 |", readme, fixed = TRUE)))
    expect_true(any(grepl("analysis/2025-01-01_exp1", readme, fixed = TRUE)))

    writeLines("x", file.path(p$dir_raw, "data.fcs"))
    local_answers("y")
    utils::capture.output(do.call(project_setup, args))
    expect_true(file.exists(file.path(p$dir_raw, "MANIFEST.md5")))
})

test_that("declined offers are not repeated in the session", {
    local_tm_settings()
    root <- local_root()
    exp <- paste0("2025-01-01_", basename(root))  # unique per test run
    args <- list("KO", experiment_name = exp, root = root, confirm = FALSE,
                 auto_cleanup = FALSE, assign_global = FALSE)
    local_answers("n")
    utils::capture.output(p <- do.call(project_setup, args))
    expect_false(file.exists(file.path(p$dir_raw, "README.md")))
    local_answers(character(0))  # any prompt would fail
    utils::capture.output(do.call(project_setup, args))
})
