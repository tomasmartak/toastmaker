test_that("settings start unset and can be saved and cleared", {
    local_tm_settings(home_base = NULL)
    utils::capture.output(s <- toastmaker_settings())
    expect_null(s$home_base)
    expect_null(s$grant)
    expect_equal(s$type, "project")

    utils::capture.output(toastmaker_settings(home_base = "heiBOX", grant = "GRK2727"))
    expect_equal(.tm_setting("grant"), "GRK2727")
    expect_true(file.exists(.tm_config_path()))

    utils::capture.output(toastmaker_settings(grant = NULL))
    expect_null(.tm_setting("grant"))
    expect_equal(.tm_setting("home_base"), "heiBOX")

    utils::capture.output(toastmaker_settings(reset = TRUE))
    expect_null(.tm_setting("home_base"))
})

test_that("options override the settings file", {
    local_tm_settings(home_base = NULL)
    utils::capture.output(toastmaker_settings(lab = "AC"))
    withr::local_options(toastmaker.lab = "XY")
    expect_equal(.tm_setting("lab"), "XY")
})

test_that("toastmaker_settings validates input", {
    local_tm_settings()
    expect_error(toastmaker_settings(colour = "x"), "Unknown setting")
    expect_error(toastmaker_settings("x"), "must be named")
    expect_error(toastmaker_settings(type = "book"), "'type' must be one of")
})

test_that("the setup wizard saves the answers", {
    local_tm_settings(home_base = NULL)
    root <- local_root()
    dir.create(file.path(root, "Cloud"))
    # root, home_base, grant, lab, type, save
    local_answers(c(root, "Cloud", "GRK1", "", "2", "y"))
    utils::capture.output(toastmaker_setup())
    expect_equal(.tm_setting("home_base"), "Cloud")
    expect_equal(.tm_setting("grant"), "GRK1")
    expect_null(.tm_setting("lab"))
    expect_equal(.tm_setting("type"), "paper")
})

test_that("project_setup starts the wizard when home_base is unset", {
    local_tm_settings(home_base = NULL)
    root <- local_root()
    dir.create(file.path(root, "Cloud"))
    # wizard: root, home_base, grant, lab, type, save; then confirm folders
    local_answers(c(root, "Cloud", "", "", "", "y", "y"))
    utils::capture.output(
        p <- project_setup("KO", root = root, assign_global = FALSE,
                           auto_cleanup = FALSE, confirm = TRUE)
    )
    expect_equal(p$bd, file.path(root, "Cloud", "Projects", "KO"))
})
