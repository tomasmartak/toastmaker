test_that("project_setup builds the expected tree", {
    local_tm_settings()
    root <- local_root()
    p <- setup_quiet("KO", lab = "AC", grant = "GRK2727", experiment_name = "2025-01-01_exp1",
                     raw_subdir = "FC", root = root, check_raw = FALSE)

    bd <- file.path(root, "heiBOX", "GRK2727", "AC", "Projects", "KO")
    expect_s3_class(p, "cookiecutter")
    expect_equal(attr(p, "type"), "project")
    expect_equal(p$bd, bd)
    expect_equal(p$dir_raw, file.path(bd, "raw", "FC", "2025-01-01_exp1"))
    expect_equal(p$dir_analysis, file.path(bd, "analysis", "2025-01-01_exp1"))
    expect_equal(p$dir_scripts, file.path(bd, "scripts"))
    expect_true(all(dir.exists(unlist(p))))
})

test_that("unset grant and lab are skipped", {
    local_tm_settings()
    root <- local_root()
    p <- setup_quiet("KO", root = root)
    expect_equal(p$bd, file.path(root, "heiBOX", "Projects", "KO"))
})

test_that("paper, presentation and poster layouts", {
    local_tm_settings()
    root <- local_root()
    paper <- setup_quiet("KO-screen", type = "paper", root = root)
    expect_equal(paper$bd, file.path(root, "heiBOX", "Papers", "KO-screen"))
    expect_setequal(names(paper), c("bd", "dir_manuscript", "dir_figures",
                                    "dir_supplementary", "dir_scripts", "dir_submission"))
    expect_true(file.exists(file.path(paper$bd, "README.md")))

    talk <- setup_quiet("2026-11-05_retreat", type = "presentation", root = root)
    expect_equal(dirname(talk$bd), file.path(root, "heiBOX", "Presentations"))
    poster <- setup_quiet("2026-11-05_EMBO", type = "poster", root = root)
    expect_equal(dirname(poster$bd), file.path(root, "heiBOX", "Posters"))
    expect_true(dir.exists(poster$dir_abstract))

    expect_error(setup_quiet("x", type = "paper", experiment_name = "e", root = root),
                 "only be used with type = \"project\"")
    expect_error(setup_quiet("x", type = "book", root = root), "'type' must be one of")
})

test_that("default type comes from settings", {
    local_tm_settings()
    root <- local_root()
    withr::local_options(toastmaker.type = "paper")
    p <- setup_quiet("P", root = root)
    expect_equal(attr(p, "type"), "paper")
})

test_that("project_setup requires a project name and a home_base", {
    local_tm_settings()
    withr::local_dir(withr::local_tempdir())
    expect_error(project_setup(), "'project' is required")

    local_tm_settings(home_base = NULL)
    expect_error(setup_quiet("KO", root = local_root()), "No base folder set")
})

test_that("experiment names without a date give a warning", {
    local_tm_settings()
    root <- local_root()
    expect_warning(setup_quiet("KO", experiment_name = "exp1", root = root,
                               check_raw = FALSE), "does not start with a date")
})

test_that("the marker lets project_setup find the project from inside", {
    local_tm_settings()
    root <- local_root()
    p <- setup_quiet("KO", lab = "AC", root = root)
    marker <- read.dcf(file.path(p$bd, ".toastmaker"))
    expect_equal(unname(marker[1, c("Type", "Name", "Lab")]), c("project", "KO", "AC"))

    expect_equal(project_root(p$dir_scripts), .tm_norm(p$bd))
    withr::local_dir(p$dir_scripts)
    inside <- setup_quiet(experiment_name = "2025-01-01_e", check_raw = FALSE)
    expect_equal(inside$bd, .tm_norm(p$bd))
    expect_true(dir.exists(file.path(p$bd, "raw", "2025-01-01_e")))
    expect_error(setup_quiet(type = "paper"), "is a 'project'")
})

test_that("confirmation shows the tree and can be declined", {
    local_tm_settings()
    root <- local_root()
    local_answers("n")
    out <- utils::capture.output(
        p <- project_setup("KO", root = root, confirm = TRUE, assign_global = FALSE)
    )
    expect_null(p)
    expect_true(any(grepl("scripts/  (new)", out, fixed = TRUE)))
    expect_false(dir.exists(file.path(root, "heiBOX")))
})

test_that("no confirmation is asked when everything exists", {
    local_tm_settings()
    root <- local_root()
    setup_quiet("KO", root = root)
    local_answers(character(0))  # any prompt would fail
    utils::capture.output(
        p <- project_setup("KO", root = root, confirm = TRUE, assign_global = FALSE)
    )
    expect_s3_class(p, "cookiecutter")
})

test_that("assign_global populates the calling environment", {
    local_tm_settings()
    root <- local_root()
    env <- new.env()
    utils::capture.output(
        local(project_setup("KO", root = root, auto_cleanup = FALSE, confirm = FALSE),
              envir = env)
    )
    expect_true(all(c("bd", "dir_raw", "dir_list", "core_objects") %in% ls(env)))
    expect_true("dir_scripts" %in% env$core_objects)
})

test_that("project_cleanup removes only empty directories", {
    local_tm_settings()
    root <- local_root()
    p <- setup_quiet("KO", experiment_name = "2025-01-01_exp1", root = root,
                     check_raw = FALSE)
    writeLines("x", file.path(p$dir_raw, "data.csv"))

    removed <- project_cleanup(p, verbose = FALSE)
    expect_true(dir.exists(p$dir_raw))
    expect_false(dir.exists(p$dir_img))
    expect_false(dir.exists(file.path(p$bd, "img"))) # empty parent goes too
    expect_true(p$dir_doc %in% removed)
})

test_that("session cleanup only touches directories it created", {
    local_tm_settings()
    root <- local_root()
    pre <- file.path(root, "heiBOX", "Projects", "KO", "doc")
    dir.create(pre, recursive = TRUE)

    utils::capture.output(
        p <- project_setup("KO", root = root, confirm = FALSE, assign_global = FALSE)
    )
    project_cleanup(verbose = FALSE)
    expect_true(dir.exists(pre))
    expect_false(dir.exists(p$dir_raw))
})

test_that("session cleanup removes an unused new project, marker included", {
    local_tm_settings()
    root <- local_root()
    utils::capture.output(
        p <- project_setup("KO", root = root, confirm = FALSE, assign_global = FALSE)
    )
    project_cleanup(verbose = FALSE)
    expect_false(dir.exists(p$bd))
})

test_that("project_snapshot counts files", {
    local_tm_settings()
    root <- local_root()
    p <- setup_quiet("KO", root = root)
    writeLines("x", file.path(p$dir_raw, "a.csv"))
    snap <- NULL
    expect_output(snap <- project_snapshot(p), "Project snapshot")
    expect_equal(snap$n_files[snap$dir == "dir_raw"], 1)
    expect_error(project_snapshot(list()), "No project found")
})
