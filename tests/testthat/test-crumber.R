make_env <- function() {
    env <- new.env()
    env$core_objects <- "keep"
    env$keep <- 1
    env$junk <- 2
    env$.hidden <- 3
    env
}

test_that("crumber removes unprotected objects only", {
    env <- make_env()
    expect_message(removed <- crumber(env = env, do.gc = FALSE), "removed 1")
    expect_equal(removed, "junk")
    expect_setequal(ls(env, all.names = TRUE), c("core_objects", "keep", ".hidden"))
})

test_that("add.to.cookiejar accepts bare names, strings and c()", {
    env <- make_env()
    crumber(add.to.cookiejar = junk, env = env)
    expect_true("junk" %in% env$core_objects)
    crumber(add.to.cookiejar = c(a, "b"), env = env)
    expect_true(all(c("a", "b") %in% env$core_objects))
    expect_message(crumber(env = env, do.gc = FALSE), "removed 0")
    expect_true(exists("junk", envir = env))
})

test_that("consume.cookie removes protection", {
    env <- make_env()
    crumber(consume.cookie = keep, env = env)
    expect_false("keep" %in% env$core_objects)
    suppressMessages(crumber(env = env, do.gc = FALSE))
    expect_false(exists("keep", envir = env))
})

test_that("crumber works without core_objects and keeps toast objects", {
    env <- new.env()
    env$toast <- structure(list(bd = "x"), class = "toast")
    env$junk <- 1
    expect_message(crumber(env = env, do.gc = FALSE), "removed 1")
    expect_equal(ls(env), "toast")

    crumber(add.to.cookiejar = df, env = env)
    expect_equal(env$core_objects, "df")
})

test_that("add.to.cookiejar = all protects everything currently there", {
    env <- make_env()
    crumber(add.to.cookiejar = all, env = env)
    expect_setequal(env$core_objects, c("keep", "junk"))
    env$later <- 4
    expect_message(removed <- crumber(env = env, do.gc = FALSE), "removed 1")
    expect_equal(removed, "later")

    env2 <- make_env()
    crumber(add.to.cookiejar = c("all", "future"), env = env2)
    expect_setequal(env2$core_objects, c("keep", "junk", "future"))
})

test_that("add.to.cookiejar = all fails loudly if an object named all exists", {
    env <- make_env()
    env$all <- 1
    expect_error(crumber(add.to.cookiejar = all, env = env), "object named 'all' exists")
    expect_error(crumber(add.to.cookiejar = "all", env = env), "Rename `all`")
    expect_equal(env$core_objects, "keep")
})

test_that("crumber works straight after project_setup()", {
    local_tm_settings()
    root <- local_root()
    env <- new.env()
    local({
        toast <- setup_quiet("KO", root = root)
        scratch <- 1
    }, envir = env)
    expect_message(removed <- crumber(env = env, do.gc = FALSE), "removed 1")
    expect_equal(removed, "scratch")
    expect_equal(ls(env), "toast")

    # with assign_global = TRUE the paths are protected via core_objects
    env <- new.env()
    utils::capture.output(local(project_setup("KO", root = root, confirm = FALSE,
                                              auto_cleanup = FALSE, assign_global = TRUE),
                                envir = env))
    env$scratch <- 1
    expect_message(crumber(env = env, do.gc = FALSE), "removed 1 object(s): scratch", fixed = TRUE)
    expect_true(all(c("bd", "dir_raw", "dir_list", "datasets") %in% ls(env)))
})

test_that("consume.cookie = all removes all protection", {
    env <- make_env()
    crumber(add.to.cookiejar = c(junk, "future"), env = env)
    crumber(consume.cookie = all, env = env)
    expect_equal(env$core_objects, character(0L))
    env$toast <- structure(list(bd = "x"), class = "toast")
    suppressMessages(crumber(env = env, do.gc = FALSE))
    expect_setequal(ls(env), c("core_objects", "toast"))

    env <- make_env()
    env$all <- 1
    expect_error(crumber(consume.cookie = all, env = env), "consume.cookie = all")
    expect_equal(env$core_objects, "keep")
})
