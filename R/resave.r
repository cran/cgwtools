#' Append data to a RData file
#'  
#' Save one or more \code{R} objects to a RData file already containing objects.
#' 
#'  ... The names of objects to be saved.
#'  list A character vector containing the names of objects to be saved.
#'  file The name of a file where the data will be saved.
#' overwrite Set to FALSE to disallow replacing items in "file" with items in the list
#' loadverbose Set to TRUE to get a list of items currently in 'file' printed to the console
#' 
# TODO
# set an error code so return 0 invisibly if ok and x>0 if something went wrong ?
resave <- function(..., list=character(), file, overwrite = TRUE, loadverbose = FALSE) {
  # for use in messaging,
  filnam = deparse(substitute(file))
  # Finalize the list of files - this is taken from the 'save' function
    names <- as.character(substitute(list(...)))[-1]
  if (missing(list) && !length(names)) 
    stop("nothing specified to be save()d")
  list <- c(list, names)
  # If the file does not exist, show  message
  if (!file.exists(file)) message( filnam," does not exist. A new file will be created.")
  # Create a new environment to store the data
  data_env <- new.env()
  # Load the data from the file into that environment
  load(file = file, envir = data_env, verbose = loadverbose)
  # Determine if any objects in the list are already in the file
  overlap <- list[list %in% ls(data_env)]
  if (length(overlap) > 0)
	if(overwrite){
		warning("The following objects will be overwritten in", filnam,': ', paste0(overlap,coll=' '))
		}else{
	# remove dupes from items to be written. make sure I got this right
			warning('These items will not overwrite current objects in ',filnam,': ', paste0(overlap,coll=' '))
			list <- list[!(list %in% overlap)]
		}
  # Check if the objects exist
  ok <- vapply(list, exists, NA)
  #stopifnot(all(ok))
  if (!all(ok)){
    warning('Items not found: ', paste0(names(ok[!ok]),coll= ''))
    list <- list[ok]
  }
  # Assign the objects in the list to the environment
  for (obj in list) assign(x = obj, value = get(obj), envir = data_env)
  # Save the file
  save(list = ls(data_env), file = file, envir = data_env)
#nothing to return
}