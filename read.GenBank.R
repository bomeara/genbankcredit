## read.GenBank.R (2012-02-17)

##   Read DNA Sequences from GenBank via Internet

## Copyright 2002-2012 Emmanuel Paradis

## This file is part of the R-package `ape'.
## See the file ../COPYING for licensing issues.

read.GenBank <-
    function(access.nb, seq.names = access.nb, species.names = TRUE,
             gene.names = FALSE, as.character = FALSE, pubmed = TRUE)
{
    N <- length(access.nb)
    ## If there are more than 400 sequences, we need to break down the
    ## requests, otherwise there is a segmentation fault.
    nrequest <- N %/% 400 + as.logical(N %% 400)
    X <- character(0)
    for (i in 1:nrequest) {
        a <- (i - 1) * 400 + 1
        b <- 400 * i
        if (i == nrequest) b <- N
        URL <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id=",
                     paste(access.nb[a:b], collapse = ","),
                     "&rettype=gb&retmode=text", sep = "")
        X <- c(X, scan(file = URL, what = "", sep = "\n", quiet = TRUE))
    }
    FI <- grep("^ {0,}ORIGIN", X) + 1
    LA <- which(X == "//") - 1
    obj <- vector("list", N)
    for (i in 1:N) {
        ## remove all spaces and digits
        tmp <- gsub("[[:digit:]]", "", X[FI[i]:LA[i]])
        obj[[i]] <- unlist(strsplit(tmp, NULL))
    }
    names(obj) <- seq.names
    if (!as.character) obj <- as.DNAbin(obj)
    if (species.names) {
        tmp <- character(N)
        sp <- grep("ORGANISM", X)
        for (i in 1:N)
            tmp[i] <- unlist(strsplit(X[sp[i]], " +ORGANISM +"))[2]
        attr(obj, "species") <- gsub(" ", "_", tmp)
    }
    if (gene.names) {
        tmp <- character(N)
        sp <- grep(" +gene +<", X)
        for (i in 1:N)
            tmp[i] <- unlist(strsplit(X[sp[i + 1L]], " +/gene=\""))[2]
        attr(obj, "gene") <- gsub("\"$", "", tmp)
    }
    if (pubmed) {
        tmp <- vector("list", N)
        endPub <- grep("//", X)
        refs <- grep("^REFERENCE", X)
        pub <- grep("^\\s+PUBMED", X)
        auth <- grep("^\\s+AUTHORS", X)
        title <- grep("^\\s+TITLE", X)
        journal <- grep("^\\s+JOURNAL", X)
        feat <- grep("^FEATURES", X)
        for (i in 1:N) {
            begPub <- ifelse(i == 1, 1, endPub[i-1])
            nRefs <- refs[refs > begPub & refs < endPub[i]]
            refLst <- vector("list", length(nRefs))
            for (j in 1:length(nRefs)) {
                rgRef <- c( nRefs[j], ifelse(j == length(nRefs), feat[i], nRefs[j+1]))
                tmpRes <- vector("list", 4)
                names(tmpRes) <- c("pubmedid", "authors", "title", "journal")
                tmpRes$pubmedid <- gsub("^\\s+PUBMED\\s+(\\d+)", "\\1", X[pub[pub > rgRef[1] & pub < rgRef[2]]])
                tmpRes$authors <- paste0(X[auth[j]:(title[j]-1)], collapse=" ")
                tmpRes$title <- paste0(X[title[j]:(journal[j]-1)], collapse=" ")
                tmpRes$journal <- paste0(X[journal[j]], collapse=" ") # JOURNAL always 1 line?
                tmpRes <- lapply(tmpRes, function(x) { gsub("\\s{2,}", " ", gsub("\\s+[A-Z]+\\s+", "", x)) })
                refLst[[j]] <- tmpRes
            }
            tmp[[i]] <- refLst
        }
        names(tmp) <- access.nb
        attr(obj, "references") <- tmp
    }
    obj
}
