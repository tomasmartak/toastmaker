# Isolate tests from the user's real settings: a temporary settings folder,
# no toastmaker.* options, and a fixed home_base unless asked otherwise.
local_tm_settings <- function(home_base = "heiBOX", .env = parent.frame()) {
    withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir(.local_envir = .env),
                        .local_envir = .env)
    opts <- grep("^toastmaker\\.", names(options()), value = TRUE)
    withr::local_options(stats::setNames(rep(list(NULL), length(opts)), opts),
                         .local_envir = .env)
    withr::local_options(list(toastmaker.home_base = home_base,
                              toastmaker.color = FALSE), .local_envir = .env)
}

# Answer prompts with `answers`, in order.
local_answers <- function(answers, .env = parent.frame()) {
    i <- 0L
    testthat::local_mocked_bindings(
        .tm_interactive = function() TRUE,
        .tm_readline = function(prompt) {
            i <<- i + 1L
            if (i > length(answers)) stop("Unexpected prompt: ", prompt)
            answers[[i]]
        },
        .package = "toastmaker", .env = .env
    )
}

setup_quiet <- function(...) {
    out <- NULL
    utils::capture.output(out <- project_setup(..., confirm = FALSE,
                                               auto_cleanup = FALSE,
                                               assign_global = FALSE))
    out
}

# Temporary root with "/" separators, as toastmaker builds its paths.
local_root <- function(.env = parent.frame()) {
    gsub("\\\\", "/", withr::local_tempdir(.local_envir = .env))
}
