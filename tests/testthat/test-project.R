test_that("project_setup builds the expected tree", {
    root <- withr::local_tempdir()
    p <- project_setup(lab = "AC", project = "KO", experiment_name = "exp1",
                       raw_subdir = "FC", root = root,
                       auto_cleanup = FALSE, assign_global = FALSE)

    bd <- file.path(root, "heiBOX", "GRK2727", "AC", "Projects", "KO")
    expect_s3_class(p, "cookiecutter")
    expect_equal(p$bd, bd)
    expect_equal(p$dir_raw, file.path(bd, "raw", "FC", "exp1"))
    expect_equal(p$dir_analysis, file.path(bd, "analysis", "exp1"))
    expect_equal(p$dir_scripts, file.path(bd, "scripts"))
    expect_true(all(dir.exists(unlist(p))))
})

test_that("NULL path components are skipped", {
    root <- withr::local_tempdir()
    p <- project_setup(grant = NULL, project = "KO", root = root,
                       auto_cleanup = FALSE, assign_global = FALSE)
    expect_equal(p$bd, file.path(root, "heiBOX", "Projects", "KO"))
})

test_that("project_setup requires a project name", {
    expect_error(project_setup(), "'project' is required")
})

test_that("assign_global populates the calling environment", {
    root <- withr::local_tempdir()
    env <- new.env()
    local(project_setup(project = "KO", root = root, auto_cleanup = FALSE),
          envir = env)
    expect_true(all(c("bd", "dir_raw", "dir_list", "core_objects") %in% ls(env)))
    expect_true("dir_scripts" %in% env$core_objects)
})

test_that("project_cleanup removes only empty directories", {
    root <- withr::local_tempdir()
    p <- project_setup(project = "KO", experiment_name = "exp1", root = root,
                       auto_cleanup = FALSE, assign_global = FALSE)
    writeLines("x", file.path(p$dir_raw, "data.csv"))

    removed <- project_cleanup(p, verbose = FALSE)
    expect_true(dir.exists(p$dir_raw))
    expect_false(dir.exists(p$dir_img))
    expect_false(dir.exists(file.path(p$bd, "img"))) # empty parent goes too
    expect_true(p$dir_doc %in% removed)
})

test_that("session cleanup only touches directories it created", {
    root <- withr::local_tempdir()
    pre <- file.path(root, "heiBOX", "Projects", "KO", "doc")
    dir.create(pre, recursive = TRUE)

    p <- project_setup(grant = NULL, project = "KO", root = root,
                       assign_global = FALSE)
    project_cleanup(verbose = FALSE)
    expect_true(dir.exists(pre))
    expect_false(dir.exists(p$dir_raw))
})

test_that("project_snapshot counts files", {
    root <- withr::local_tempdir()
    p <- project_setup(project = "KO", root = root,
                       auto_cleanup = FALSE, assign_global = FALSE)
    writeLines("x", file.path(p$dir_raw, "a.csv"))
    snap <- NULL
    expect_output(snap <- project_snapshot(p), "Project snapshot")
    expect_equal(snap$n_files[snap$dir == "dir_raw"], 1)
    expect_error(project_snapshot(list()), "No project found")
})
