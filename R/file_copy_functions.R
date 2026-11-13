#' Copy a Large Folder Quickly Using Robocopy
#'
#' Copies an entire folder with the Windows `robocopy` utility, which is
#' much faster than [file.copy()] for large folders. Uses multi-threaded copying
#' and shows a progress bar (by file count). The source folder is only read,
#' never modified or deleted, and files already in the destination that are not
#' in the source are left alone (no `/MIR`, `/MOVE` or `/PURGE`). Files with the
#' same name but a different size or date are overwritten.
#'
#' Requires Windows and the `processx` package. Do not point `destination_dir`
#' at a folder inside `source_dir`.
#'
#' @param source_dir Source folder path.
#' @param destination_dir Destination folder path (created if missing).
#' @param threads Number of copy threads (robocopy `/MT`, 1-128). Default 4.
#' @param include_subfolders Copy all subfolders, including empty ones
#'   (`/E`). Default `TRUE`. If `FALSE`, only files in the top level are copied.
#'
#' @return Invisibly, the robocopy exit code (0-7 = success, 8 or more =
#'   failure, in which case an error is raised).
#'
#' @examples
#' \dontrun{
#' ROBOCOPY_FOLDER("C:/data/big_folder", "D:/backup/big_folder")
#' ROBOCOPY_FOLDER("C:/src", "D:/dst", threads = 16, include_subfolders = FALSE)
#' }
#'
#' @export
ROBOCOPY_FOLDER <- function(
    source_dir,
    destination_dir,
    threads = 4,
    include_subfolders = TRUE
) {
    if (.Platform$OS.type != "windows") {
        stop("robocopy is only available on Windows.")
    }
    if (!dir.exists(source_dir)) {
        stop("Source folder does not exist: ", source_dir)
    }
    if (!requireNamespace("processx", quietly = TRUE)) {
        stop("Please install.packages('processx').")
    }
    threads <- as.integer(threads)
    if (is.na(threads) || threads < 1 || threads > 128) {
        stop("'threads' must be between 1 and 128.")
    }

    src <- normalizePath(source_dir, winslash = "\\", mustWork = TRUE)
    dest <- gsub("/", "\\\\", destination_dir)
    dest <- sub("\\\\+$", "", dest)

    message("Counting files in source...")
    total <- length(list.files(
        src,
        recursive = include_subfolders,
        all.files = TRUE,
        include.dirs = FALSE,
        no.. = TRUE
    ))
    total <- max(total, 1L)
    message(sprintf(
        "Found %s files. Copying with %d threads...",
        format(total, big.mark = ","),
        threads
    ))

    # processx passes each argument safely, so no quoting is needed
    args <- c(
        src,
        dest,
        if (include_subfolders) "/E",
        "/DCOPY:DAT",
        paste0("/MT:", threads),
        "/R:3",
        "/W:5",
        "/NDL",
        "/NP",
        "/NJH",
        "/NJS",
        "/NC",
        "/NS",
        "/BYTES"
    )

    # Each file processed (copied or skipped) produces one line of stdout
    proc <- processx::process$new(
        "robocopy",
        args,
        stdout = "|",
        stderr = "2>&1"
    )
    pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
    done <- 0L
    repeat {
        proc$poll_io(500)
        out <- proc$read_output_lines()
        if (length(out)) {
            done <- min(done + sum(nzchar(trimws(out))), total)
            utils::setTxtProgressBar(pb, done)
        }
        if (!proc$is_alive()) break
    }
    proc$read_output_lines()
    utils::setTxtProgressBar(pb, total)
    close(pb)

    status <- proc$get_exit_status()

    # Robocopy exit codes below 8 indicate success (bitmask of copy results)
    if (status >= 8) {
        stop("Robocopy failed with exit code ", status)
    }
    message("Robocopy finished (exit code ", status, ").")
    invisible(status)
}
