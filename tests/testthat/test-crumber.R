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
