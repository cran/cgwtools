
#the bannermaker
 
banvax <- function(msg, file = 'banner.txt',  linewid = 80, bandat = cgwtools::ascarr){
msg <- toupper(msg)
msgvec <- unlist(strsplit(msg,''))
linewid <- floor(linewid[1])
#check bandat dims
 bdim <- dim(bandat)
# Replace "unknown" item  to a 'box'
msgvec[!(msgvec %in% dimnames(bandat)[[3]])] <- 'box'
# to simplify, make matban full-width "rows" 
colsize <- bdim[2]
maxcol = ceiling( colsize*length(msgvec)/linewid) * linewid
matban <- matrix(' ',nrow = bdim[1], ncol = maxcol)
for (jmat in 1:length(msgvec)) {
	matban[, (1+(jmat-1)* colsize):(colsize +(jmat-1)* colsize)] <- bandat[,,msgvec[jmat]]
	}
# truncate linewid to multiple of dim
linewid <- floor(linewid/colsize) * colsize 

charperline = floor(linewid/colsize) 
for (jlin in 1:ceiling(length(msgvec)/charperline)) {
	for (jrow in 1:bdim[1]) {
		cat(sprintf('%s\n', unlist(paste0(matban[jrow,(1 + (jlin-1)* colsize*charperline) :(jlin* colsize*charperline)], collapse='') )), file=file, append= TRUE )
		}
	}
}
