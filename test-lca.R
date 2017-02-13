# set up environment
# setwd("C:/Users/Gavyn/Documents/R/BH-Works/LCA/bhworks-lca")
library(poLCA)

#define function
f <- cbind(BHSED04, BHSED05, BHSED06, BHSED07)~1

# load the data
attr <- read.csv('1023.csv', header=TRUE, sep=',', stringsAsFactors = FALSE)

# convert all integers into positive integers
attr2 <- subset(attr, Form.Version == "PC 12 to 24", select=c(BHSED04, BHSED05, BHSED06, BHSED07))

# transform data to positive
attr3 <- abs(attr2)
attr4 <- attr3 + 1

# transform data into characters, transpose
tmp <- sapply(attr4, as.character)
attr5 <- t(matrix(c(tmp, sapply(attr4, sub, pattern = "(.)(.)", replacement = "\\2\\1")), 4, byrow = TRUE))
colnames(attr5) <- (c('BHSED04', 'BHSED05', 'BHSED06', 'BHSED07'))
attr6 <- as.data.frame(attr5)

# run LCA
test <- poLCA(f, attr6, nclass=2)

