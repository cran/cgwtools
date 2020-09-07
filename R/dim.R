#  "overload" to return dim() when it's sensible and length() elsewise
# Interesting fact --  the function  "dim" and the function  " dim<-" are different
# primitives, so this function here doesn't interfere with the latter.
dim <- function(item) {

if (is.null(base::dim(item)) ) {  
# I wanted to do dims$(thenameitself), but I fear only 
# eval(parse(text=paste("dims$",names,'<-length(get(names))',sep="")))
# would work. Simpler just to use numerical loop index
	dims<-length(item)  
	} else{
		dims  <- base::dim(item)  
		}

return(dims)
}