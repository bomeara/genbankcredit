#Take a DNAbin object with 'pubmed' attributes
#make a pretty table of citations by count

citation_table <- function(recs){
    pmids <- sapply(recs, attr, "pubmed")
    ref_freqs <- table(pmids)
    uids <- names(ref_freqs)
    xml_recs <- lapply(uids, function(ID)
                       entrez_fetch(db="pubmed", rettype="xml",id=ID))
    parsed_recs <- lapply(recs, parse_pubmed_xml)
    first_au <- sapply(parsed_recs, function(x) x$authors[1])
    year <- sapply(parsed_recs, "[[", "year")
    doi <- sapply(parsed_recs, "[[", "doi")
    res <- data.frame(author=first_au, 
                      year=year,
                      count=ref_freqs,
                      doi=doi)
    return(res)
}



    
    
