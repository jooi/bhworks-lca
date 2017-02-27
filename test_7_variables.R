# load the data
pdset <- read.csv('1023.csv', header=TRUE)
  
# selecting 7 variables from the PC data
pdset1 <- subset(pdset, Form.Version == "PC 12 to 24" 
                  & BHSSA03a >= 0 & BHSSA02a >= 0 & BHST02 >= 0 
                 & BHST03 >= 0 & BHST04 >= 0 & BHSSX05 >= 0 & BHSSX06 >= 0,
                  select=c(BHSSA03a,BHSSA02a,BHST02,BHST03,BHST04,BHSSX05,BHSSX06))

# dichotomize the variables
pdset2 <- apply(pdset1, 2, function(x) ifelse(x > 0, 2, 1))
pdset2 <- as.data.frame(pdset2)

# define function
f <- cbind(BHSSA03a,BHSSA02a,BHST02,BHST03,BHST04,BHSSX05,BHSSX06)~1

# run LCA
LCAmodel <- poLCA(f, pdset2, nclass=2, nrep=20, graphs=TRUE)

# creating new data frame with latent class assigned
pdset3 <- pdset2
pdset3$LCA.Class <- factor(as.character(LCAmodel$predclass))

# conduct logistic regression
model <- glm(LCA.Class ~ BHSSA03a+BHSSA02a+BHST02+BHST03+BHST04+BHSSX05+BHSSX06, data=pdset3, family="binomial")
summary(model)
