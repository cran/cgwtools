dirdir <- function(path = ".", pattern = NULL, all.files = FALSE,
 full.names = FALSE, recursive = FALSE,
 ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE) {
# common sense here:
if(recursive) include.dirs = TRUE
fulldir <-dir(path, pattern, all.files, full.names, recursive, ignore.case,include.dirs, no..)
fullinfo <- file.info(fulldir) 
idxdir <- fullinfo$isdir == TRUE 
return(fulldir[idxdir])
}
