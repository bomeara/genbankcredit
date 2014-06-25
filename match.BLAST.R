library(BoSSA)

#can take DNAbin object or a sequence as a string
match.BLAST <- function(X, ...) {
	result <- NA
	if(class(X) == "DNAbin") {
		result <- blast(X, nb=1, ...)
	} else { 
		result <- blast(X=as.DNAbin(strsplit(X, "")[[1]]), nb=1, ...)
	}
	return(result)
}

a<-read.GenBank("U15717")
b<-blast(a)
