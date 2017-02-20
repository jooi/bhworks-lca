# set up environment
# setwd("C:/Users/Gavyn/Documents/R/BH-Works/LCA/bhworks-lca")
library(poLCA)

#define function
f <- cbind(BHSED04, BHSED05, BHSED06, BHSED07)~1

# load the data
attr <- read.csv('1023.csv', header=TRUE, sep=',', stringsAsFactors = FALSE)

# convert all integers into positive integers
attr2 <- subset(attr, Form.Version == "PC 12 to 24", select=c(BHSED04, BHSED05, BHSED06, BHSED07))

# transform data into characters, transpose
tmp <- sapply(attr2, as.character)
colnames(tmp) <- (c('BHSED04', 'BHSED05', 'BHSED06', 'BHSED07'))
attr3 <- as.data.frame(tmp)

# run LCA
test <- poLCA(f, attr3, nclass=2, nrep=20)
