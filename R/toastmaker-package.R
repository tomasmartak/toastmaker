#' toastmaker: personal project setup toolbox
#'
#' Helpers for a reproducible analysis layout:
#'
#' * [toastmaker_setup()] walks you through the defaults (base folder,
#'   grant, lab, default layout); [toastmaker_settings()] views or changes
#'   them.
#' * [project_setup()] creates a project, paper, presentation or poster
#'   folder under `<root>/<home_base>/[<grant>/][<lab>/]`, in `Projects/`,
#'   `Papers/`, `Presentations/` or `Posters/`.
#' * [dataset_setup()] creates a shared or public dataset folder in
#'   `Datasets/` that projects can use.
#' * [figure_setup()] adds a figure folder to a paper, presentation or
#'   poster, linked to the projects it came from.
#' * [project_record()] records package versions in `renv.lock`.
#' * [project_root()] finds the folder a script belongs to.
#' * [data_manifest()] and [data_verify()] record and check checksums of raw
#'   data.
#' * [project_cleanup()] removes the directories that stayed empty.
#' * [project_snapshot()] reports how many files each directory holds.
#' * [crumber()] clears the global environment except for protected objects.
#'
#' @section Options:
#' Every setting in [toastmaker_settings()] can be overridden with an R
#' option `toastmaker.<name>` (e.g. `toastmaker.home_base`), for example in
#' `~/.Rprofile`. In addition:
#'
#' * `toastmaker.color`: use ANSI colours in messages (default: `interactive()`).
#'
#' @keywords internal
"_PACKAGE"
