###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

####                    E&B Utrecht         Nathalie Amacker          january 2020                         ####

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Course Best Practice Writing Reproducible Code 

### based on part of the code for Pseudomonas Biocontrol Project

### R Code for the different graphs found in the manuscript

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### Preface ####

# use RProject

# empty global environment
rm (list=ls())

# https://support.microsoft.com/en-ca/help/3103821/general-problem-installing-any-r-package
.libPaths() # query the current location R will use to install new user-contributed libraries
#install required packages & libraries
#install.packages(), library(), require()
#install.packages("ggplot2")
require(ggplot2)

#install.packages("gplots")
library(gplots) # to use heatmap.2
require(RColorBrewer) # for the color "brewer.pal"

#install.packages('nlme')
require(nlme)

#set defaults margins for graphs, and only 1 graph per window (mfrow = c(1,1))
par(mar=c(5,4,4,2)+0.1, mfrow=c(1,1))

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### 0.Import and preparation of dataset ####

# get the raw data from the raw folder
mydatasetP <- read.csv2("data/raw/exp_setup3_counts.csv")

# rename the colomns
colnames(mydatasetP) <- c("day","plate","Row","Col","medium","protozoa","bacteria","state","estimation")

mydatasetP <- subset(mydatasetP, mydatasetP$bacteria!="Blank")
mydatasetP <- subset(mydatasetP, mydatasetP$bacteria!="")
mydatasetP$estimation <- as.numeric(mydatasetP$estimation)

# label information

# Protist:  P1=Cercomonas C5D3; P2=Cercomonas S24D2;
#           P3=Naegleria P2881; P4=Naegleria P145-4;
#           P5= Acanthamoeba C13D2; P6=Vannella P147

#Bacteria:  A=OP50; B=SMMP3; C=SVBP3; D=SVBP6; E=SVBP8
#           F=RPBP2; G=RBBP4; H=RBAN4; I=SVMP4
#

# mybactorder <- factor(mydatasetP$bacteria, 
#                      levels= c("A","F","E","H","G","B","C","I", "D"))
# correspond to -> ("Ecoli","RPBP2","SVBP8","RBAN4","RBBP4","SMMP3", "SVBP3","SVMP4", "SVBP6"))
# this order of bacteria is based on a previous setup

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### 1. Protist Density at different days ####

# (1) descriptive analysis -> boxplot
# (2) statistical test -> anova 
#     response variable:  estimation [active individuals/cm^2] OR [cysts/cm^2]
#                         type of data: count data
#     explanatory variables:  bacterial isolates
#                             protist species
#                             others like well position (Col, Row, Plate)

# Keep only active individals
Active <- subset(mydatasetP, mydatasetP$state=="active")
# Create a dataset for each medium treatment (only using 2%KB)

ActiveKB <- subset(Active, Active$medium=="2%KB")

# 1.1 Day 1 after inoculation ####
# (1)

ActiveKBD1 <- subset(ActiveKB, ActiveKB$day=="1")

mybactorder <- factor(ActiveKBD1$bacteria, 
                      levels= c("NO","A","F","E","H","G","B","C","I", "D"))
boxplot(ActiveKBD1$estimation ~ mybactorder, main="2%KB - Day 1", ylim=c(0,300000))
boxplot(ActiveKBD1$estimation ~ mybactorder, main="2%KB - Day 1", ylim=c(0,10000))

myplot <- ggplot(ActiveKBD1, aes(x=mybactorder,y=estimation))
#myplot + geom_boxplot() + geom_point() + ylim(0,300000) + ggtitle("2%KB - Day 1") +
myplot + geom_boxplot() + geom_point() + ylim(0,10000) + ggtitle("2%KB - Day 1") + 
  xlab("bacterial isolates") + ylab("Protist density estimation [ind./cm^2]") + 
  scale_x_discrete(breaks=c("NO","A","F","E","H","G","B","C","I", "D"),
                   labels=c("NO", "OP50", "RPBP2","SVBP8","RBAN4",
                            "RBBP4", "SMMP3", "SVBP3", "SVMP4","SVBP6"))

# One graph per protist on 2%KB

MyProtist <- c("P1", "P2", "P3", "P4", "P5", "P6")
Mytitle <- c("Cercomonas C5D3", "Cercomonas S24D2","Naegleria P2881", "Naegleria P145-4",
             "Acanthamoeba C13D2", "Vannella P147")

i <- 6

OneprotActiveKBD1 <- subset(ActiveKBD1, ActiveKBD1$protozoa==MyProtist[i])

mybactorder <- factor(OneprotActiveKBD1$bacteria, 
                      levels= c("NO","A","F","E","H","G","B","C","I", "D"))

#windows()
myplot <- ggplot(OneprotActiveKBD1, aes(x=mybactorder ,y=estimation))
myplot + geom_boxplot() + geom_point() + 
  #ylim(0,300000) + 
  ggtitle(Mytitle[i], subtitle=" Day 1, 2%KB") +
  xlab("bacterial isolates") + ylab("Protist density estimation [ind./cm^2]") +
  scale_x_discrete(breaks=c("NO","A","F","E","H","G","B","C","I", "D"),
                   labels=c("NO", "OP50", "RPBP2","SVBP8","RBAN4",
                            "RBBP4", "SMMP3", "SVBP3", "SVMP4","SVBP6"))


# (2)


# For 2%KB
Y <- ActiveKBD1$estimation 
X <- ActiveKBD1$bacteria 

# The function gsl{nlme} fits a linear model using generalized least squares. 
# The errors are allowed to be correlated and/or have unequal variances

# first perform a normal linear model
model1 <- gls(Y~X, na.action=na.omit)
plot(model1)
#if plot shows evidence for heterosedasticity -> go to model 2
summary(model1)
anova(model1)

#
model2 <- gls(Y~X, 
              weights = varIdent(form=~1 | X), 
              # varIdent() -> constant variance function structure
              na.action=na.omit)
# argument "weights" -> optional VarFunc object or one-sided formula describing 
# the within-group heteroscedasticity structure.
plot(model2)
# new plot shows no strong heteroscedasticity

summary(model2)

anova(model2)# Compute analysis of variance (or deviance) tables for one or more fitted model objects
# anova() -> uses type I error type by default 

# check with model is better -> the lowest the AIC value, the better
anova(model1,model2)

# model 3 -> poisson distribution

model3 <- glm(Y~X, family=poisson,na.action=na.omit)
plot(model3)
summary(model3)
anova(model3)

# if we expect overdispersion (-> variance is larger than the mean),
# we can correct the standrad errors using a quasi-GLM model (p.226, Zuur et al. book)
model4 <- glm(Y~X, family=quasipoisson,na.action=na.omit)
plot(model4)
summary(model4)

anova(model3,model4,test="Chi")

# 1.2 Day 3 after inoculation ####
#(1)

ActiveKBD3 <- subset(ActiveKB, ActiveKB$day=="3")

mybactorder <- factor(ActiveKBD3$bacteria, 
                      levels= c("NO","A","F","E","H","G","B","C","I", "D"))
boxplot(ActiveKBD3$estimation ~ mybactorder, main="2%KB - Day 3", ylim=c(0,300000))

myplot <- ggplot(ActiveKBD3, aes(x=mybactorder,y=estimation))
myplot + geom_boxplot() + geom_point() + ylim(0,300000) + ggtitle("2%KB - Day 3") +
  xlab("bacterial isolates") + ylab("Protist density estimation [ind./cm^2]") + 
  scale_x_discrete(breaks=c("NO","A","F","E","H","G","B","C","I", "D"),
                   labels=c("NO", "OP50", "RPBP2","SVBP8","RBAN4",
                            "RBBP4", "SMMP3", "SVBP3", "SVMP4","SVBP6"))


# One graph per protist on 2%KB

MyProtist <- c("P1", "P2", "P3", "P4", "P5", "P6")
Mytitle <- c("Cercomonas C5D3", "Cercomonas S24D2","Naegleria P2881", "Naegleria P145-4",
             "Acanthamoeba C13D2", "Vannella P147")

i <- 6

OneprotActiveKBD3 <- subset(ActiveKBD3, ActiveKBD3$protozoa==MyProtist[i])

mybactorder <- factor(OneprotActiveKBD3$bacteria, 
                      levels= c("NO","A","F","E","H","G","B","C","I", "D"))

#windows()
myplot <- ggplot(OneprotActiveKBD3, aes(x=mybactorder ,y=estimation))
myplot + geom_boxplot() + geom_point() + 
  #ylim(0,300000) + 
  ggtitle(Mytitle[i], subtitle=" Day 3, 2%KB") +
  xlab("bacterial isolates") + ylab("Protist density estimation [ind./cm^2]") +
  scale_x_discrete(breaks=c("NO","A","F","E","H","G","B","C","I", "D"),
                   labels=c("NO", "OP50", "RPBP2","SVBP8","RBAN4",
                            "RBBP4", "SMMP3", "SVBP3", "SVMP4","SVBP6"))

# (2)

# For 2%KB
Y <- ActiveKBD3$estimation 
X <- ActiveKBD3$bacteria 

# The function gsl{nlme} fits a linear model using generalized least squares. 
# The errors are allowed to be correlated and/or have unequal variances

model1 <- gls(Y~X, na.action=na.omit)
plot(model1)
#if plot shows evidence for heterosedasticity -> go to model 2
summary(model1)
anova(model1)

#
model2 <- gls(Y~X, 
              weights = varIdent(form=~1 | X), 
              # varIdent() -> constant variance function structure
              na.action=na.omit)
# argument "weights" -> optional VarFunc object or one-sided formula describing 
# the within-group heteroscedasticity structure.
plot(model2)
# new plot shows no strong heteroscedasticity

summary(model2)

anova(model2)# Compute analysis of variance (or deviance) tables for one or more fitted model objects
# anova() -> uses type I error type by default 

# check with model is better -> the lowest the AIC value, the better
anova(model1,model2)

# model 3 -> poisson distribution

model3 <- glm(Y~X, family=poisson,na.action=na.omit)
plot(model3)
summary(model3)
anova(model3)

# if we expect overdispersion (-> variance is larger than the mean),
# we can correct the standrad errors using a quasi-GLM model (p.226, Zuur et al. book)
model4 <- glm(Y~X, family=quasipoisson,na.action=na.omit)
plot(model4)
summary(model4)

anova(model3,model4,test="Chi")


# 1.3 Day 5 after inoculation ####
#(1)

ActiveKBD5 <- subset(ActiveKB, ActiveKB$day=="5")

mybactorder <- factor(ActiveKBD5$bacteria, 
                      levels= c("NO","A","F","E","H","G","B","C","I", "D"))
boxplot(ActiveKBD5$estimation ~ mybactorder, main="2%KB - Day 5", ylim=c(0,300000))

myplot <- ggplot(ActiveKBD5, aes(x=mybactorder,y=estimation))
#myplot <- ggplot(ActiveKBD5, aes(x=mybactorder,y=estimation, fill=protozoa))
myplot + geom_boxplot() + geom_point() + ylim(0,300000) + ggtitle("2%KB - Day 5") +
  xlab("bacterial isolates") + ylab("Protist density estimation [ind./cm^2]") + 
  scale_x_discrete(breaks=c("NO","A","F","E","H","G","B","C","I", "D"),
                   labels=c("NO", "OP50", "RPBP2","SVBP8","RBAN4",
                            "RBBP4", "SMMP3", "SVBP3", "SVMP4","SVBP6"))

# One graph per protist on 2%KB

MyProtist <- c("P1", "P2", "P3", "P4", "P5", "P6")
Mytitle <- c("Cercomonas C5D3", "Cercomonas S24D2","Naegleria P2881", "Naegleria P145-4",
             "Acanthamoeba C13D2", "Vannella P147")

i <- 6

OneprotActiveKBD5 <- subset(ActiveKBD5, ActiveKBD5$protozoa==MyProtist[i])

mybactorder <- factor(OneprotActiveKBD5$bacteria, 
                      levels= c("NO","A","F","E","H","G","B","C","I", "D"))

#windows()
myplot <- ggplot(OneprotActiveKBD5, aes(x=mybactorder ,y=estimation))
myplot + geom_boxplot() + geom_point() + 
  #ylim(0,300000) + 
  ggtitle(Mytitle[i], subtitle=" Day 5, 2%KB") +
  xlab("bacterial isolates") + ylab("Protist density estimation [ind./cm^2]") +
  scale_x_discrete(breaks=c("NO","A","F","E","H","G","B","C","I", "D"),
                   labels=c("NO", "OP50", "RPBP2","SVBP8","RBAN4",
                            "RBBP4", "SMMP3", "SVBP3", "SVMP4","SVBP6"))


# (2)

# For 2%KB
Y <- ActiveKBD5$estimation 
X <- ActiveKBD5$bacteria 

# The function gsl{nlme} fits a linear model using generalized least squares. 
# The errors are allowed to be correlated and/or have unequal variances

model1 <- gls(Y~X, na.action=na.omit)
plot(model1)
#if plot shows evidence for heterosedasticity -> go to model 2
summary(model1)
anova(model1)

#
model2 <- gls(Y~X, 
              weights = varIdent(form=~1 | X), 
              # varIdent() -> constant variance function structure
              na.action=na.omit)
# argument "weights" -> optional VarFunc object or one-sided formula describing 
# the within-group heteroscedasticity structure.
plot(model2)
# new plot shows no strong heteroscedasticity

summary(model2)

anova(model2)# Compute analysis of variance (or deviance) tables for one or more fitted model objects
# anova() -> uses type I error type by default 

# check with model is better -> the lowest the AIC value, the better
anova(model1,model2)
# model 3 -> poisson distribution

model3 <- glm(Y~X, family=poisson,na.action=na.omit)
plot(model3)
summary(model3)
anova(model3)

# if we expect overdispersion (-> variance is larger than the mean),
# we can correct the standrad errors using a quasi-GLM model (p.226, Zuur et al. book)
model4 <- glm(Y~X, family=quasipoisson,na.action=na.omit)
plot(model4)
summary(model4)

anova(model3,model4,test="Chi")

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Heatmap vizualisation 

# Preparation of data at day 3 OR day 5

# Calculation of the mean per treatment (Treatment A = Prot X with Bacteria Y)
mymeansP <- matrix(NA,10,6)  # creation of a new matrix to collect the means
mybact <- c("NO","A","F","E","H","G","B","C","I", "D")
myprot<- myprotist <- c("P1", "P2", "P3", "P4", "P5", "P6")
colnames(mymeansP) <- myprot; row.names(mymeansP) <- mybact # specifiy the colums and rows names

mydata <- subset(mydatasetP, mydatasetP$medium=="2%KB")
mydata <- subset(mydata, mydata$state=="active")
mydata <- subset(mydata, mydata$day=="3")
#mydata <- subset(mydata, mydata$day=="5")

for (j in 1:6){
  one_prot <-  subset(mydata, mydata$protozoa==myprot[j],select=c("protozoa", "bacteria", "estimation"))
  for ( i in 1:10){
    
    one_bact <- subset(one_prot, one_prot$bacteria==mybact[i], select=c("estimation"))
    e <- mean(one_bact$estimation )
    mymeansP[i,j] <- round(mean(one_bact$estimation))
  }
}

windows()
heatmap.2(t(mymeansP), col= colorRampPalette(brewer.pal(7, "RdYlBu"))(25), 
          denscol="black",linecol=NULL,tracecol=NULL, scale="row")

