#' Cookiecutter project setup
#'
#' This function sets up a directory for you to do your analysis in.
#'
#' @param home_base The name of the folder in your documents/home that you want the project to be in (default = "heiBOX")
#' @param grant The name of the grant this project is to be categorised under.
#' @param lab The name of the lab this project is to be categorised under.
#' @param lab The name that you wish to give this project.
#' @param return_paths A bool to return the base and auxiliary directory paths as a list for further use (default = TRUE)
#'
#' @return Base and auxiliary directory paths in a vector.
#' @export
project_setup <- function (home_base = "heiBOX", grant = "GRK2727", lab, project,
                           return_paths = T) {
    # get system and base location
    loc <- paste0("~/",
                  ifelse(tolower( as.character( Sys.info()[1] ) ) == "windows",
                         "", "Documents/"),
                  home_base)

    # setup base dir
    bd <- file.path(loc, grant, lab, "Projects", project)

    # setup auxiliary dirs
    dir_analysis <- file.path(bd, "analysis")
    dir_doc <- file.path(bd, "doc")
    dir_img <- file.path(bd, "img")
    dir_raw <- file.path(bd, "raw")
    dir_scripts <- file.path(bd, "scripts")
    dir_list <- c(dir_analysis, dir_doc, dir_img, dir_raw, dir_scripts)

    # create directories
    cat("\033[32mCreating base directory and and children: ", bd, "...\033[0m\n")
    for (dir in dir_list) {
        if (!dir.exists(dir)) {
            dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        }
    }
    cat("\033[1;32mDone!\033[0m\n")

    # return auxdirs
    if (return_paths) {
        cat("\033[3mReturning auxiliary directories list: analysis, documentation, image, raw, and scripts...\033[0m\n")
        output_handles <- c(bd, dir_list)
        attr(output_handles, "class") <- "cookiecutter"
        output_handles
    }
}


#' Cookiecutter project cleanup
#'
#' This function scours your analysis directory and removes empty auxiliary directories.
#'
#' @param home_base The name of the folder in your documents/home that you want the project to be in (default = "heiBOX")
#' @param grant The name of the grant this project is to be categorised under.
#' @param lab The name of the lab this project is to be categorised under.
#' @param lab The name that you wish to give this project.
#' @param return_paths A bool to return the base and auxiliary directory paths as a list for further use (default = TRUE)
#'
#' @return Base and auxiliary directory paths in a vector.
#' @export
project_cleanup <- function (dir_list) {
    # check class of input
    if (class(dir_list) != "cookiecutter") stop ("\033[31mError! Input directory list is not of class cookiecutter. Please use a cookiecutter::project_setup() object as input.\033[0m\n")

    # setup dir lists
    bd <- dir_list[1] # base directory
    auxdirs <- dir_list[2:length(dir_list)] # auxiliary directory

    # remove directories by handle if they are empty. Applies to parent dir too
    cat("\033[34mRemoving auxiliary directories from base directory: ", bd, "...\033[0m\n")
    for (dh in auxdirs) {
        if (length(list.files(dh)) == 0) {
            cat("\033[3;33m   removing empty auxiliary directory:", dh, "\033[0m\n")
            unlink(dh, recursive = T)
        }
    }

    if (length(list.files(bd)) == 0) {
        cat("\033[3;35mRemoving empty project directory:", bd, "\033[0m\n")
        unlink(bd, recursive = T)
    }
    cat("\033[1;32mCleaning complete!\n\033[0m")
}
