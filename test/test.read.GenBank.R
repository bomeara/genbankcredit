
testData <- read.csv(file="10.1371_journal.pone.0094199.s001.csv", stringsAsFactors=FALSE)

col2 <- read.GenBank(na.omit(testData[, 2]))

col3 <- read.GenBank(na.omit(testData[, 3]))

