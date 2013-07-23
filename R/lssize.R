lssize <-
function(items,byte=FALSE){
	setTimeLimit(elapsed=40, transient=T)
	if (any(sapply(sapply(items,get),typeof)=='closure')){
		warning('Closures in list, will ignore.')
		items<-items[(sapply(sapply(items,get),typeof)=='closure')!=TRUE]
	}
# S4-specific tool, courtesy M. Morgan of Fred Hutchinson CRC 
# the trick is: once S4 is fully 'unwound,' the last call to 'f' will skip
# the "if" and do the desired function
# 'length' here won't work the way one might desire.  The result of the recursed
# deslotting produces an object each of whose names represents foo@slot@slot...
# all the way down, and whose value is the number of items in that subslot.
# Handily, empty slot returns a zero, so rather than "length" of the output,
# really want "sum" of output to get true number of elements of original S4 item.
	if(byte) {
	sizes<-sapply(items,object.size,simplify=FALSE)
	} else {
		s4gone <- function(object) {
		  fs4 <- function(x) {
			   if (isS4(x)) {
			      slots <- setNames(slotNames(x), slotNames(x))
			      lapply(lapply(slots, slot, object=x), fs4)
			      } else length(x) 
		#puts length of each subslot into an element of x
			}
		  fs4(object)
		}
		sizes<- sapply( sapply( sapply( sapply(items,get,simplify=FALSE),s4gone,simplify=FALSE), unlist,recursive=TRUE,simplify=FALSE) ,sum) 
	}
# Richie's safety timeout
	setTimeLimit(elapsed=Inf,transient=TRUE)
	return(sizes)
}
