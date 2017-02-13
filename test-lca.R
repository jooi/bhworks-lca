# set up environment
# setwd("C:/Users/Gavyn/Documents/R/BH-Works/LCA/bhworks-lca")
library(poLCA)

#define function
f <- cbind(BHSED02, BHSED04, BHSED05, BHSED06, BHSED07)~1

# load the data
attr <- read.csv('1023.csv', header=TRUE, sep=',', stringsAsFactors = FALSE)

# convert all integers into positive integers
attr2 <- subset(attr, Form.Version == "PC 12 to 24", select=c(BHSED02, BHSED04, BHSED05, BHSED06, BHSED07))

# transform data to positive
attr3 <- abs(attr2)

# run LCA
test <- poLCA(f, attr3, nclass=2)