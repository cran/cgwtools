short <-
function(x=seq(1,20),numel=4,skipel=0,ynam=deparse(substitute(x))) {
# note that HEAD and TAIL count each row of a matrix as an element
# i.e. head(matrix,2) prints the first two rows
ynam<-as.character(ynam)
#clean up spaces
ynam<-gsub(" ","",ynam)
#unlist goes by columns, so transpose to get what's expected
if(is.list(x)) x<-unlist(t(x))
if(2*numel >= length(x)) {
	print(x)
	}
	else {	
		frist=1+skipel
		last=numel+skipel
		cat(paste(ynam,'[',frist,'] thru ',ynam,'[',last,']\n',sep=""))
		print(x[frist:last])
		cat(' ... \n')
		cat(paste(ynam,'[',length(x)-numel-skipel+1,'] thru ', ynam, '[', length(x)-skipel,']\n',sep=""))
		print(x[(length(x)-numel-skipel+1):(length(x)-skipel)])
		}
}
