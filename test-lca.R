# set up environment
# setwd("C:/Users/Gavyn/Documents/R/BH-Works/LCA/bhworks-lca")
library(poLCA)

#define function
f <- cbind(BHSED04, BHSED05, BHSED06, BHSED07)~1

# load the data
attr <- read.csv('1023.csv', header=TRUE, sep=',', stringsAsFactors = FALSE)

# convert all integers into positive integers
attr2 <- subset(attr, Form.Version == "PC 12 to 24" 
                & BHSED04 >= 0 & BHSED05 >= 0 & BHSED06 >= 0 & BHSED07 >= 0,
                select=c(BHSED04, BHSED05, BHSED06, BHSED07))

# transform data into characters, add headers, convert to df
tmp <- sapply(attr2, as.character)
colnames(tmp) <- (c('BHSED04', 'BHSED05', 'BHSED06', 'BHSED07'))
attr3 <- as.data.frame(tmp)

# run LCA
riskmodel <- poLCA(f, attr3, nclass=2, nrep=20, graphs=TRUE)

#riskmodel$probs # this returns the probability table
#riskmodel$predclass # this returns a vector of class assignment

# create new data frame with the classifications
attr4 <- attr3
attr4$LCA.Class <- factor(as.character(riskmodel$predclass))

#converting factor variables into ordinal variables
attr4$BHSED04 <- ordered(attr4$BHSED04, levels = c(0, 2, 4), 
                         labels = c("Never", "Sometimes", "Often"))
attr4$BHSED05 <- ordered(attr4$BHSED05, levels = c(0, 2, 4), 
                         labels = c("Never", "Sometimes", "Often"))
attr4$BHSED06 <- ordered(attr4$BHSED06, levels = c(0, 2, 4), 
                         labels = c("Never", "Sometimes", "Often"))
attr4$BHSED07 <- ordered(attr4$BHSED07, levels = c(0, 2, 4), 
                         labels = c("Never", "Sometimes", "Often"))



# conduct logistic regression
model <- glm(LCA.Class ~ BHSED04+BHSED05+BHSED06+BHSED07, data=attr4, family="binomial")
summary(model)

# establish train 
#train <- attr4

# establish test
#tmp <- read.csv('test.csv', header=TRUE, sep=',', stringsAsFactors = FALSE)
#test <- subset(tmp, Form.Version == "PC 12 to 24", select=c(BHSED04, BHSED05, BHSED06, BHSED07))
#test <- sapply(test, as.character)
#colnames(test) <- (c('BHSED04', 'BHSED05', 'BHSED06', 'BHSED07'))
#test <- as.data.frame(test)

#riskmodeltest <- poLCA(f, test, nclass=2, nrep=20, graphs=TRUE)
