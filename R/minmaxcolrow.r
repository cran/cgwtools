# Obvious quickies


maxRow <- function(mat,ties.method = c("random", "first", "last")) {
foo <- max.col(t(mat), ties.method)
return(invisible(foo))
}


minCol <-function(mat, ties.method = c("random", "first", "last")){
foo <- max.col(-mat, ties.method)
return(invisible(foo))
}

minRow <-function(mat, ties.method = c("random", "first", "last")) {
foo <- max.col(t(-mat), ties.method)
}
maxCol <-function(mat, ties.method = c("random", "first", "last")){
foo <- max.col(mat, ties.method)
return(invisible(foo))
}