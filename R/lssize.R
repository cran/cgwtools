
#aha!  get(activeboundthing) executes it.
#  added this fix May 2020 to kill off all activBinding items 
lssize <- function(items,byte=FALSE){
setTimeLimit(elapsed = 40, transient =T)
# remove activebindings
 items <- items[(sapply(items, function(k) bindingIsActive(k,.GlobalEnv))!=TRUE)]
# don't want to deal with functions either
if (any(sapply(sapply(items,get),typeof)=='closure')){
                warning('Closures in list, will ignore.')
                items<-items[(sapply(sapply(items,get),typeof)=='closure')!=TRUE]
        }

if(byte) {
        # I need a  s4gonebyte() function to get bytes out.
# and need that get() too.
        s4gonebyte <- function(object) {
          fb4 <- function(x) {
                   if (isS4(x)) {
                          slots <- setNames(slotNames(x), slotNames(x))
                          lapply(lapply(slots, slot, object=x), fb4)
                          } else object.size(if(is.list(x)) unlist(x) else x)
                        }
                fb4(object)
                }
        sizes<-sapply(items,function(k) sum(unlist(s4gonebyte(get(k))) ),simplify=FALSE)
        } else {
                s4gone <- function(object) {
                  fs4 <- function(x) {
                        if (isS4(x)) {
                                slots <- setNames(slotNames(x), slotNames(x))
                                lapply(lapply(slots, slot, object=x), fs4)
                                } else length( if(is.list(x)) unlist(x) else x )
                #puts length of each subslot into an element of x
                        }
                fs4(object)
                }
                sizes<- sapply( sapply( sapply( sapply(items,get,simplify=FALSE),s4gone,simplify=FALSE), unlist,simplify=FALSE) ,sum) 
        }
# Richie's safety timeout
setTimeLimit(elapsed = Inf,transient =TRUE)
return(sizes)
}
