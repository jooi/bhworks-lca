# load the data
dataset = read.csv('/Users/LiYinuo/Desktop/mdlogix/bhworks-lca/joanna_raw.csv',
                   header=TRUE)

# selecting 20 variables from the PC data
dataset1 <- subset(dataset, 
                   select=c(Gender, BHSSA02A, BHSSA03A, BHSSC02, BHSSC03, BHSF02, BHSF05, 
                           BHSSF02, BHSSF04, BHSSX01, BHSSX06, BHSSX05, BHSSA05,
                           BHSSA06, BHSSA07, BHSSA08, BHST01, BHST02, BHST03, BHST04))

# omitting the missing data
dataset2 <- na.omit(dataset1)

# dichotomize the variables
attach(dataset2)
dataset2$BHSSA02A[BHSSA02A>0] <- 1
dataset2$BHSSA03A[BHSSA03A>0] <- 1
dataset2$BHSSC02[BHSSC02>0] <- 1
dataset2$BHSSCFail[BHSSC03<4] <- 0
dataset2$BHSSCFail[BHSSC03>=4] <- 1
dataset2$BHSF02[BHSF02>0] <- 1
dataset2$BHSF05[BHSF05<5] <- 0
dataset2$BHSF05[BHSF05==5] <- 1
dataset2$BHSSF02[BHSSF02>0] <- 1
dataset2$BHSSF04[BHSSF04>0] <- 1
if ((Gender == 1) && (BHSSX01 !=2)) {
  dataset2$BHSSX01 <- 1
}  else if ((Gender ==2) && (BHSSX01 != 1)) {
  dataset2$BHSSX01 <- 1
}  else {dataset2$BHSSX01 <- 0}

if ((Gender == 1) && (BHSSX06 !=3)) {
  dataset2$BHSSX06 <- 1
}  else if ((Gender ==2) && (BHSSX06 != 4)) {
  dataset2$BHSSX06 <- 1
}  else {dataset2$BHSSX06 <- 0}

dataset2$BHSSX05[BHSSX05>0] <- 1
dataset2$BHSSA05[BHSSA05>0] <- 1
dataset2$BHSSA06[BHSSA06>0] <- 1
dataset2$BHSSA07[BHSSA07>0] <- 1
dataset2$BHSSA08[BHSSA08>0] <- 1
dataset2$BHST01[BHST01>0] <- 1
dataset2$BHST02[BHST02>0] <- 1
dataset2$BHST03[BHST03>0] <- 1
dataset2$BHST04[BHST04>0] <- 1

detach(dataset2)

# dichotomize the variables
dataset3 <- apply(dataset2, 2, function(x) ifelse(x > 0, 2, 1))
dataset3 <- as.data.frame(dataset3)

# define function
f <- cbind(BHSSA02A, BHSSA03A, BHSSC02, BHSSCFail, BHSF02, BHSF05, 
           BHSSF02, BHSSF04, BHSSX01, BHSSX06, BHSSX05, BHSSA05,
           BHSSA06, BHSSA07, BHSSA08, BHST01, BHST02, BHST03, BHST04)~1

# run LCA
LCAmodel <- poLCA(f, dataset3, nclass=2, nrep=100, graphs=TRUE)

# Factor the variables
dataset3 <- as.data.frame(lapply(dataset3, as.factor))

# Assigning the latent class to the dataset
dataset3$LCA.Class <- factor(as.character(LCAmodel$predclass))

# conduct logistic regression
model <- glm(LCA.Class ~ BHSSA02A+BHSSA03A+BHSSC02+BHSSCFail+BHSF02+BHSF05+
             BHSSF02+BHSSF04
            # +BHSSX01+BHSSX06 # These two varibales are all zero
             +BHSSX05+BHSSA05+
             BHSSA06+BHSSA07+BHSSA08+BHST01+BHST02+BHST03+BHST04, data=dataset3, family="binomial")
summary(model)

# this model includes the top 7 from the paper; we have more managable beta coefficients here
# when we lower the number of predictors in the logistic regression model

# model <- glm(LCA.Class ~ BHSSA03A+BHST04+BHSSX06+BHST02+BHST03+BHSSX05+
#                BHSSA02A, data=dataset3, family="binomial")
