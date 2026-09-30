#' toastmaker: personal project setup toolbox
#'
#' Helpers for a reproducible analysis layout:
#'
#' * [project_setup()] creates `<root>/<home_base>/<grant>/<lab>/Projects/<project>/`
#'   with `analysis/`, `raw/`, `img/`, `doc/` and `scripts/` subdirectories.
#' * [project_cleanup()] removes the directories that stayed empty.
#' * [project_snapshot()] reports how many files each directory holds.
#' * [crumber()] clears the global environment except for protected objects.
#'
#' @section Options:
#' Set these in your `~/.Rprofile` to avoid repeating yourself:
#'
#' * `toastmaker.root`: folder that contains `home_base`. Defaults to
#'   `~/Documents` (or `~` on Windows, where it already points to Documents).
#' * `toastmaker.home_base`: default `home_base` (`"heiBOX"`).
#' * `toastmaker.grant`: default `grant` (`"GRK2727"`).
#' * `toastmaker.color`: use ANSI colours in messages (default: `interactive()`).
#'
#' @keywords internal
"_PACKAGE"
