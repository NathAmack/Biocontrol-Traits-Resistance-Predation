# growth of protists on the different bacterial isolates in 2% KB

### Preface ####

# empty global environment
rm (list=ls())

# if you have troubles installing some packages
# https://support.microsoft.com/en-ca/help/3103821/general-problem-installing-any-r-package
.libPaths() # query the current location R will use to install new user-contributed libraries

#install required packages & libraries
#install.packages(), library(), require()
#install.packages("ggplot2")
require(ggplot2)

library(multcompView)
require(multcompView)


#for PCA analysis visualization
#install.packages("ggfortify")
require(ggfortify)
library(scales)

#install.packages("gplots")
library(gplots) # to use heatmap.2
require(RColorBrewer) # for the color "brewer.pal"

#install.packages("corrplot")
library(corrplot)

#install.packages('dunn.test')
require('dunn.test')

#install.packages('nlme')
require(nlme)

#install.packages("ltm") # for biserial.cor()
library(ltm)
require(ltm)

# install.packages('pscl')
require(pscl)

#set defaults margins for graphs, and only 1 graph per window (mfrow = c(1,1))
par(mar=c(5,4,4,2)+0.1, mfrow=c(1,1))

##############################################################################################################

### 0.Import and preparation of dataset ####

# for protist density 

mydatasetP <- read.table("../Data/processed/exp_setup3_counts.txt")
# rename the columns
colnames(mydatasetP) <- c("day","plate","Row","Col","medium","protozoa","bacteria","state","estimation")

# remove empty wells
mydatasetP <- subset(mydatasetP, mydatasetP$bacteria!="Blank")
mydatasetP <- subset(mydatasetP, mydatasetP$bacteria!="")

# label information

# Protist:  P1=Cercomonas C5D3; P2=Cercomonas S24D2;
#           P3=Naegleria P2881; P4=Naegleria P145-4;
#           P5= Acanthamoeba C13D2; P6=Vannella P147

# Bacteria:  A=OP50; B=SMMP3; C=SVBP3; D=SVBP6; E=SVBP8
#           F=RPBP2; G=RBBP4; H=RBAN4; I=SVMP4
#

# check the structure of the data
str(mydatasetP)
# change to the correct type
mydatasetP$bacteria <- as.factor(mydatasetP$bacteria)
mydatasetP$protozoa <- as.factor(mydatasetP$protozoa)

# set a defined order for the factor bacteria
mybactorder <- factor(mydatasetP$bacteria, 
                      levels= c("A","F","E","H","G","B","C","I", "D"))
# correspond to -> ("Ecoli","SPSA5","RPBP2","SVBP8","RBAN4","RBBP4","SMMP3",  
#                                                              "SVBP3","SVMP4", "SVBP6"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Visualization of the data for a first appreciation ####
# Plot the data of the active individuals in PAS or 5%KB and at day 5 or 3 

Active <- subset(mydatasetP, mydatasetP$state=="active")
ActivePAS <- subset(Active, Active$medium=="PAS")
ActiveKB <- subset(Active, Active$medium=="2%KB")

# Day 3 
ActivePASD3 <- subset(ActivePAS, ActivePAS$day=="3")
ActiveKBD3 <- subset(ActiveKB, ActiveKB$day=="3")

  # in 5% KB
    mybactorder <- factor(ActiveKBD3$bacteria, 
                          levels= c("NO","A","F","E","H","G","B","C","I", "D"))
    boxplot(ActiveKBD3$estimation ~ mybactorder)

# in PAS
    mybactorder <- factor(ActivePASD3$bacteria, 
                          levels= c("NO","A","F","E","H","G","B","C","I", "D"))
    boxplot(ActivePASD3$estimation ~ mybactorder)

# Day 5
ActivePASD5 <- subset(ActivePAS, ActivePAS$day=="5")
ActiveKBD5 <- subset(ActiveKB, ActiveKB$day=="5")

    # in 5%KB
    mybactorder <- factor(ActiveKBD5$bacteria, 
                          levels= c("NO","A","F","E","H","G","B","C","I", "D"))
    boxplot(ActiveKBD5$estimation ~ mybactorder, main="2%KB - Day 5")
    
    # in PAS
    mybactorder <- factor(ActivePASD5$bacteria, 
                          levels= c("NO","A","F","E","H","G","B","C","I", "D"))
    boxplot(ActivePASD5$estimation ~ mybactorder, main="PAS - Day 5")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mydatasetP$estimation <- as.numeric(mydatasetP$estimation)
# remove RBAN4 from the dataset
mydatasetP <- subset(mydatasetP, mydatasetP$bacteria!="H")
    
    
# 1.1 growth of all protists combined on the different bacterial isolates in 2% KB ####

# response variable:  estimation [ind./cm^2] OR [cysts/cm^2]
#                     type of data: count data
# explanatory variables:  bacterial isolates
#                         protist species
#                         others like well position (Col, Row, Plate)

# select wished conditions (growth medium: 5%KB OR PAS, Day: 3 OR 5, Protist state: active OR cyst)

myselectedmedium <- c("2%KB")
myselectedday <- c("3")
myselectedstate <- c("active")

mydatasetOneP <- mydatasetP

SelectedMedium <- subset(mydatasetOneP, mydatasetOneP$medium==myselectedmedium)
SelectedDay <- subset(SelectedMedium, SelectedMedium$day==myselectedday)
SelectedState <- subset(SelectedDay,SelectedDay$state==myselectedstate)

# Extract basic statistical data: mean, standard deviation and median

MySubset <- SelectedState

mybactorder <- factor(MySubset$bacteria, 
                      levels= c("NO","A","F","G","E","B","C","I", "D"))

MySubsetAllPOneB <- subset(MySubset, MySubset$bacteria=="A")
round(mean(MySubsetAllPOneB$estimation))
round(sd(MySubsetAllPOneB$estimation))
round(median(MySubsetAllPOneB$estimation))

# Analyse the protist density in relation to the bacterial isolates

# chose the reference to be used 
# "NO" = no added bacterial cells, corresponding to E. coli cells transferred alongside the protist cultures 
# at the start of the experiment 
# "A"= E. coli OP50

# note: we used "A" (E. coli OP50) as positive control in the published manuscript

MySubset$bacteria  <- relevel(MySubset$bacteria , ref = "NO")
MySubset$bacteria  <- relevel(MySubset$bacteria , ref = "A")

# investigate the data with different models
# chech model assumptions with plot()

# we are working with count data, therefore a poisson distribution may be more adapted
plot(table(MySubset$estimation))

## simple linear model
modelglm <- glm(Y ~ X) 
summary(modelglm)
plot(modelglm)
plot(modelglm, which=1)

# create square root transformed data
# apply square root transformation to decrease heteroscedasticity
Y <- MySubset$estimation 
SQRTY <- sqrt(Y)
X <- MySubset$bacteria 
modelglm <- glm(SQRTY ~ X) 
summary(modelglm)
plot(modelglm)
plot(modelglm, which=1)

mylm <- lm(SQRTY ~ X) 
summary(mylm)
plot(mylm)
# note that glm() = lm() if nothing more is specified in glm()

# Generalized least square model (gls)
## gls with VarIdent -> to account for heteroscedasticity

mygls <- gls(Y ~ X, weights = varIdent(form=~1|X))
# get error message: false convergence (8) ? 
plot(mygls)
summary(mygls)

# Generalized Linear model (glm)
## glm with poisson distribution 
modelGLMPoisson <- glm(Y~X, family=poisson)
plot(modelGLMPoisson)
summary(modelGLMPoisson)

(ExplainedDeviance <- 100*((modelGLMPoisson$null.deviance - modelGLMPoisson$deviance)/modelGLMPoisson$null.deviance))

# how many zeros does my model predict ? 
## https://data.library.virginia.edu/getting-started-with-hurdle-models/ 

mu <- predict(modelGLMPoisson, type = "response") # predict expected mean count
exp <- sum(dpois(x = 0, lambda = mu))  # sum the probabilities of a 0 count for each mean
round(exp)                                # predicted number of 0's

# how many zeros do we have in the setup ? 
sum(MySubset$estimation < 1)

mean(MySubset$estimation)
var(MySubset$estimation)
sd(MySubset$estimation)

# Poisson assumes that the mean is equal to the variance
# if it is not so, better to use quasi poisson 
# if we expect overdispersion (-> variance is larger than the mean),
# we can correct the standrad errors using a quasi-GLM model (p.226, Zuur et al. book)

modelGLMQuasiP <- glm(Y~X, family=quasipoisson)
summary(modelGLMQuasiP)
# (Dispersion parameter for quasipoisson family taken to be 43788.3)

## see also Zuur et al. p. 218
(ExplainedDeviance <- 100*((modelGLMQuasiP$null.deviance - modelGLMQuasiP$deviance)/modelGLMQuasiP$null.deviance))

anova(modelGLMPoisson,modelGLMQuasiP)

# how many zero predicted ? 
mu <- predict(modelGLMQuasiP, type = "response") # predict expected mean count
exp <- sum(dpois(x = 0, lambda = mu))  # sum the probabilities of a 0 count for each mean
round(exp)                                # predicted number of 0's

# diagnostic plot residuals vs fitted (according to Zuur et al., p.231)
EP <- resid(modelGLMQuasiP, type = "pearson")
DevRes <- resid(modelGLMQuasiP,type="deviance")
mu <- predict(modelGLMQuasiP, type = "response")
EP2 <- E / sqrt( 42255.63 * mu)  
#When we use an overdispersion parameter φ, the variance is adjusted with this
#parameter, and we divide the residuals yi − μi by the square root of φμi.
# From summary(model4)
# -> (Dispersion parameter for quasipoisson family taken to be 43788.3)
E <- MySubset$estimation - mu
plot(x = mu, y = E, main = "Response residuals")
plot(x = mu, y = EP, main = "Pearson residuals")
plot(x = mu, y = EP2,
     main = "Pearson residuals scaled")
plot(x = mu, y = DevRes, main = "Deviance residuals")

## For overdisperion, negative binomial models may even be better than quasi poisson
## negative binomial 
# Package MASS
modelnb <- glm.nb(Y~X)
summary(modelnb)
anova(modelnb)
plot(modelnb)

(ExplainedDeviance <- 100*((modelnb$null.deviance - modelnb$deviance)/modelnb$null.deviance))

mu <- predict(modelnb, type = "response")
rNegBin <- rnegbin(162,mu,0.3381585941) # change theta
# how many zeros does the model predict?
sum(rNegBin < 1) # between 20 and 40

anova(modelGLMQuasiP,modelnb)

# If we have an excess of zeros compared to what our models predict, 
# we can use spceifici that can deal with it
# the two-part model or hurdle() 
#       -> the zeros are assumed to be of structural nature only
#       -> and are modelled with a binomial logit distribution
# the mixed model zeroinfl() 
#       -> the zeros are assumed to be of structural and sampling nature
# See also Zuur et al. Book Chap. 11 

# Here the zeros are of structural nature, therefore we use the hurdle model

# from package pscl
###

modelHurdle1 <- hurdle(Y~X, dist="poisson",link="logit") # default
summary(modelHurdle1)

EP <- resid(modelHurdle1, type = "pearson")
mu <- predict(modelHurdle1, type = "response")
# EP2 <- E / sqrt(7.630148 * mu) # - to check !
E <- MySubset$estimation - mu
plot(x = mu, y = E, main = "Response residuals")
plot(x = mu, y = EP, main = "Pearson residuals")
plot(x = mu, y = EP2,
     main = "Pearson residuals scaled")


ZANB <- hurdle(Y~X, dist = "negbin", link = "logit")
summary(ZANB)

coef(summary(ZANB))

hurdletest(ZANB)

EP <- resid(ZANB, type = "pearson")
mu <- predict(ZANB, type = "response")
# EP2 <- E / sqrt(7.630148 * mu) # - to check ! 
E <- MySubset$estimation - mu
plot(x = mu, y = E, main = "Response residuals")
plot(x = mu, y = EP, main = "Pearson residuals")
plot(x = mu, y = EP2,
     main = "Pearson residuals scaled")

# compare models
lrtest(modelHurdle1, ZANB)
AIC(modelGLMQuasiP, modelHurdle1)
AIC(modelHurdle1,ZANB)
AIC(modelZINB, ZANB)


# comparison with a gls model

mygls1 <- gls(Y~X, weights=varIdent(form= ~1|X))
plot(mygls1)
anova(mygls1)
summary(mygls1)
AIC(mygls1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

