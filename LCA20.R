#1 load the data
dataset = read.csv('joanna_raw.csv',
                   header=TRUE)

#2 redefine the variables value based on the form logic
dataset$BHSSA02A = ifelse (dataset$BHSSA02 == 0, 0, dataset$BHSSA02A)
dataset$BHSSA03A = ifelse (dataset$BHSSA03 == 0, 0, dataset$BHSSA03A)

#3 selecting 20 variables from the PC data
dataset1 <- subset(dataset, 
                   select=c(Gender, BHSSA02A, BHSSA03A, BHSSC02, BHSSC03, BHSF02, BHSF05, 
                           BHSSF02, BHSSF04, BHSSX01, BHSSX06, BHSSX05, BHSSA05,
                           BHSSA06, BHSSA07, BHSSA08, BHST01, BHST02, BHST03, BHST04))

#4 omitting the missing data
dataset2 <- na.omit(dataset1)

#5 dichotomize the variables
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
dataset2$BHSSX01 = ifelse (Gender == 0, ifelse(BHSSX01 !=1, 1, 0), 
                           ifelse(BHSSX01 !=2, 1, 0))
dataset2$BHSSX06 = ifelse (Gender == 0, ifelse(BHSSX06 !=4, 1, 0), 
                           ifelse(BHSSX06 !=3, 1, 0))

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

#6 redefine variable values from 0/1 to 1/2
dataset3 <- apply(dataset2, 2, function(x) ifelse(x > 0, 2, 1))
dataset3 <- as.data.frame(dataset3)

#7 define function
f <- cbind(BHSSA02A, BHSSA03A, BHSSC02, BHSSCFail, BHSF02, BHSF05, 
           BHSSF02, BHSSF04, BHSSX01, BHSSX06, BHSSX05, BHSSA05,
           BHSSA06, BHSSA07, BHSSA08, BHST01, BHST02, BHST03, BHST04)~1

#8 run LCA
LCAmodel <- poLCA(f, dataset3, nclass=2, nrep=100, graphs=TRUE)

#9 Factor the variables
dataset3 <- as.data.frame(lapply(dataset3, as.factor))

#10 Assigning the latent class to the dataset
dataset3$LCA.Class <- factor(as.character(LCAmodel$predclass))

#11 conduct logistic regression
model <- glm(LCA.Class ~ BHSSA02A+BHSSA03A+BHSSC02+BHSSCFail+BHSF02+BHSF05+
              BHSSF02+BHSSF04
              +BHSSX01+BHSSX06
              +BHSSX05+BHSSA05+
              BHSSA06+BHSSA07+BHSSA08+BHST01+BHST02+BHST03+BHST04, data=dataset3, family="binomial")
summary(model)

# this model includes the top 7 from the paper; we have more managable beta coefficients here
# when we lower the number of predictors in the logistic regression model

model <- glm(LCA.Class ~ BHSSA03A+BHST04+BHST02+BHST03+BHSSX05+
               BHSSA02A, data=dataset3, family="binomial")
summary(model)

sa03a <- glm(LCA.Class ~ BHSSA03A, data=dataset3, family="binomial")
summary(sa03a)
