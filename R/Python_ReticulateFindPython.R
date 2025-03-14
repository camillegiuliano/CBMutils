
#' Reticulate find Python
#'
#' Use reticulate to find or install Python that meets version requirements.
#' Download the pyenv-win Python version management tool from Github if necessary.
#'
#' This function was created to bypass the requirement for Git to be installed
#' when \code{\link[reticulate]{install_python}} is called on a Windows computer
#' without pyenv-win already installed.
#' pyenv-win is installed by reticulate by cloning the Github repository.
#' This will instead download the pyenv-win repository as a ZIP file.
#'
#' Note: If pyenv-win is installed by ZIP download instead of via Git clone,
#' it will not be updated after the first time it is downloaded.
#'
#' @param version character. Python version or a comma separated list of version constraints.
#' See ?reticulate::virtualenv_starter 'version' argument
#' @param versionInstall character. Version to install if suitable version not found.
#' Defaults to 'version'. Required if 'version' is a string of versions constraints instead of a specific version.
#' @param useGit logical. Allow reticulate to clone pyenv-win if Git is available
#' @param prompt logical. Prompt user to approve download of pyenv-win tool
#' @param pyenvRoot character. Path to directory of where to download the pyenv-win tool
#' @param pyenvOnly logical. Exclude versions not within a pyenv install directory
#'
#' @return character. Path to Python executable (.exe) file
#' @export
ReticulateFindPython <- function(version, versionInstall = version, useGit = TRUE, prompt = FALSE,
                                 pyenvRoot = tools::R_user_dir("CBMutils"), pyenvOnly = FALSE){

  # Get path to Python interpreter
  pyExe <- reticulate_python_exe_path(version, pyenvRoot = pyenvRoot, pyenvOnly = pyenvOnly)

  # If found: return
  if (!is.null(pyExe)) return(pyExe)

  # If not found: install Python
  if (identical(tolower(Sys.info()[["sysname"]]), "windows")){

    reticulate_install_python_windows(
      versionInstall, pyenvRoot = pyenvRoot, useGit = useGit, prompt = prompt)

  }else{

    reticulate::install_python(versionInstall)
  }

  # Return path to interpreter
  reticulate_python_exe_path(version, pyenvRoot = pyenvRoot)
}

#' Python EXE path
#'
#' Get path to Python interpreter, including installs at a given pyenv-win location
#'
#' @param version character. Python version or a comma separated list of version constraints.
#' See ?reticulate::virtualenv_starter 'version' argument
#' @param pyenvRoot character. Path to directory containing pyenv-win tool
#' @param pyenvOnly logical. Exclude versions not within a pyenv install directory
#'
#' @return character or NULL. IF found, a path to Python executable (.exe) file
reticulate_python_exe_path <- function(version = NULL,
                                       pyenvRoot = tools::R_user_dir("CBMutils"), pyenvOnly = FALSE){

  # Get paths to Python interpreters in known locations
  pyPaths <- reticulate::virtualenv_starter(version = version, all = TRUE)

  # Search provided 'pyenvRoot' for more installs
  pyenvDir <- file.path(pyenvRoot, "pyenv")
  if (file.exists(pyenvDir)){

    withr::local_envvar(
      c(PYENV      = file.path(pyenvDir, "pyenv-win", fsep = "/"),
        PYENV_ROOT = file.path(pyenvDir, "pyenv-win", fsep = "/"),
        PYENV_HOME = file.path(pyenvDir, "pyenv-win", fsep = "/")
      ))
    withr::local_path(
      c(file.path(pyenvDir, "pyenv-win/bin",   fsep = "/"),
        file.path(pyenvDir, "pyenv-win/shims", fsep = "/")),
      action = "prefix")

    pyPaths <- rbind(
      pyPaths,
      reticulate::virtualenv_starter(version = version, all = TRUE)
    )
  }

  if (pyenvOnly & nrow(pyPaths) > 0){
    pyPaths <- pyPaths[sapply(pyPaths$path, function(path){
      any(c("pyenv", "pyenv-win") %in% strsplit(normalizePath(path, winslash = "/"), "/")[[1]])
    }),]
  }

  # Choose highest version
  if (nrow(pyPaths) > 1){
    pyPaths <- pyPaths[pyPaths$version == max(pyPaths$version),]
  }

  # Return path or NULL
  if (nrow(pyPaths) > 0) return(pyPaths[["path"]][[1]])
}


#' Install Python with reticulate::install_python
#'
#' Download the pyenv-win Python version management tool from Github if necessary.
#' See: https://github.com/pyenv-win/pyenv-win
#'
#' @param version character. Python version string.
#' @param useGit logical. Allow reticulate to clone pyenv-win if Git is available
#' @param prompt logical. Prompt user to approve download of pyenv-win tool
#' @param pyenvRoot character. Path to directory of where to download the pyenv-win tool
reticulate_install_python_windows <- function(version = NULL, useGit = TRUE, prompt = interactive(),
                                              pyenvRoot = tools::R_user_dir("CBMutils")){

  # Check if Git is available on system
  reqAvailable <- c(
    git = ifelse(!useGit, FALSE, suppressWarnings(tryCatch({
      system("git --version", intern = TRUE)
      TRUE
    }, error = function(e) FALSE)))
  )

  # If Git not available: check if pyenv is available
  if (!reqAvailable[["git"]]){

    reqAvailable[["pyenv"]] <- suppressWarnings(tryCatch({
      system("pyenv --version", intern = TRUE)
      TRUE
    }, error = function(e) FALSE))
  }

  # If neither Git or pyenv is available: install pyenv-win directly from Github
  if (!any(reqAvailable)){

    # Set location for local install of pyenv-win
    pyenvDir <- file.path(pyenvRoot, "pyenv")

    # Download 'pyenv-win' from Github
    if (!file.exists(pyenvDir)){

      dlPyenv <- TRUE

      if (prompt){

        ans <- readline("Type Y to download the pyenv-win tool for managing Python installations ")

        if (!identical(trimws(tolower(ans)), "y")){

          dlPyenv <- FALSE

          warning("reticulate may not be able install Python without pyenv-win")
        }
      }

      if (dlPyenv){

        dir.create(pyenvRoot, recursive = TRUE, showWarnings = FALSE)
        download_unzip_url(
          "https://github.com/pyenv-win/pyenv-win/archive/master.zip",
          destdir = pyenvDir)
      }
    }

    # Add pyenv-win to environmental variables
    if (file.exists(pyenvDir)){

      withr::local_envvar(
        c(PYENV      = file.path(pyenvDir, "pyenv-win", fsep = "/"),
          PYENV_ROOT = file.path(pyenvDir, "pyenv-win", fsep = "/"),
          PYENV_HOME = file.path(pyenvDir, "pyenv-win", fsep = "/")
        ))
      withr::local_path(
        c(file.path(pyenvDir, "pyenv-win/bin",   fsep = "/"),
          file.path(pyenvDir, "pyenv-win/shims", fsep = "/")),
        action = "prefix")
    }
  }

  # Install Python
  ## If not specified, let reticulate decide which version to install
  tryCatch({

    do.call(

      reticulate::install_python,

      if (!is.null(version)){
        list(version = version)
      }else list()
    )
  }, error = function(e) stop(
    "Python installation failed. Python can be installed directly from python.org/downloads",
    "\n", e$message,
    call. = FALSE))
}

#' Download and unzip URL
#' @param url character.
#' @param destdir character. Path to destination directory
#' @param overwrite logical. Overwrite existing file
#' @importFrom utils download.file unzip
download_unzip_url <- function(url, destdir, overwrite = FALSE){

  if (file.exists(destdir) & overwrite){
    unlink(destdir, recursive = TRUE)
    if (file.exists(destdir)) stop("Could not remove directory: ", destdir)
  }
  if (file.exists(destdir)) stop("Destination directory found; set overwrite = TRUE: ", destdir)

  # Create temporary directory
  tempDir <- tempfile()
  dir.create(tempDir)
  on.exit(unlink(tempDir, recursive = TRUE))

  # Download URL
  tempZip <- file.path(tempDir, "temp.zip")
  utils::download.file(url = url, destfile = tempZip, quiet = TRUE)

  # Unzip and move to destination path
  unzip(tempZip, exdir = tempDir)

  unzipDir <- list.dirs(tempDir, recursive = FALSE)
  tryCatch(
    file.rename(unzipDir, destdir),
    warning = function(w) stop(w, call. = FALSE))
}

