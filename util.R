package <- function(pkgs, install=TRUE, update=FALSE, quiet=TRUE, verbose=TRUE, ...) {
  myrequire <- function(package, ...) {
    result <- FALSE
    if(quiet) { 
      suppressMessages(suppressWarnings(result <- require(package, ...)))
    } else {
      result <- suppressWarnings(require(package, ...))
    }
    return(result)
  }
  mymessage <- function(msg) {
    if(verbose) {
      message(msg)
    }
  }
  
  installedpkgs <- installed.packages()
  availpkgs <- available.packages()[,c('Package','Version')]
  if(nrow(availpkgs) == 0) {
    warning(paste0('There appear to be no packages available from the ',
                   'repositories. Perhaps you are not connected to the ',
                   'Internet?'))
  }
  # It appears that hyphens (-) will be replaced with dots (.) in version
  # numbers by the packageVersion function
  availpkgs[,'Version'] <- gsub('-', '.', availpkgs[,'Version'])
  results <- data.frame(loaded=rep(FALSE, length(pkgs)),
                        installed=rep(FALSE, length(pkgs)),
                        loaded.version=rep(as.character(NA), length(pkgs)),
                        available.version=rep(as.character(NA), length(pkgs)),
                        stringsAsFactors=FALSE)
  row.names(results) <- pkgs
  for(i in pkgs) {
    loadedPkgs <- search()
    needInstall <- FALSE
    if(i %in% row.names(installedpkgs)) {
      v <- as.character(packageVersion(i))
      if(i %in% row.names(availpkgs)) {
        if(v != availpkgs[i,'Version']) {
          if(!update) {
            mymessage(paste0('A newer version of ', i, 
                             ' is available ', '(current=', v, 
                             '; available=',
                             availpkgs[i,'Version'], ')'))
          }
          needInstall <- update
        }
        results[i,]$available.version <- availpkgs[i,'Version']
      } else {
        mymessage(paste0(i, ' is not available on the repositories.'))
      }
    } else {
      if(i %in% row.names(availpkgs)) {
        needInstall <- TRUE & install
        results[i,]$available.version <- availpkgs[i,'Version']
      } else {
        warning(paste0(i, ' is not available on the repositories and ',
                       'is not installed locally'))
      }
    }
    if(needInstall | !myrequire(i, character.only=TRUE)) {
      install.packages(pkgs=i, quiet=quiet)
      if(!myrequire(i, character.only=TRUE, ...)) {
        warning(paste0('Error loading package: ', i))
      } else {
        results[i,]$installed <- TRUE
        results[i,]$loaded <- TRUE
        results[i,]$loaded.version <- as.character(packageVersion(i))
      }
    } else {
      results[i,]$loaded <- TRUE
      results[i,]$loaded.version <- as.character(packageVersion(i))
    }
    loadedPkgs2 <- search()
    for(j in loadedPkgs2[!loadedPkgs2 %in% loadedPkgs]) {
      try(detach(j, character.only=TRUE), silent=TRUE)
    }
  }
  if(verbose) {
    return(results)
  } else {
    invisible(results)
  }
}


theme_ibex <- function (base_size = 12, base_family = "") 
{
  theme_classic() +
    theme(
      #legend.position = 'none',
      legend.title=element_blank(),
      axis.line.y =
        element_blank(),
      axis.ticks.y =
        element_blank(),
      axis.text.y =
        element_blank(),
      axis.text.x = element_text(color =
                                   '#aaaaaa'),
      strip.text = element_text(face =
                                  'bold'),
      strip.background =
        element_rect(fill = '#eeeeee', color = NA),
      panel.margin.x = unit(0.25, 'in'),
      panel.margin.y = unit(0.25, 'in')
    ) 
}
