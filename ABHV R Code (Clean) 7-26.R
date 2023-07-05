library("MASS", lib.loc="C:/Program Files/R/R-3.6.0/library")
library("lattice", lib.loc="C:/Program Files/R/R-3.6.0/library")
library("boot", lib.loc="C:/Program Files/R/R-3.6.0/library")
library("car", lib.loc="~/R/win-library/3.6")
library("emmeans", lib.loc="~/R/win-library/3.6")
library("lme4", lib.loc="~/R/win-library/3.6")
library("zoo", lib.loc="~/R/win-library/3.6")
library("tidyr", lib.loc="~/R/win-library/3.6")
library("multcomp", lib.loc="~/R/win-library/3.6")
library("foreign", lib.loc="~/R/win-library/3.6")
library("msm", lib.loc="~/R/win-library/3.6")
library("ggplot2", lib.loc="~/R/win-library/3.6")
library("effects", lib.loc="~/R/win-library/3.6")
library("lmerTest", lib.loc="~/R/win-library/3.6")
#library("dplyr", lib.loc="~/R/win-library/3.6")


#Read in data file with auto-headings and blanks/ N/As set to blank ("")
#Laptop
#myd <- read.csv("C:/Users/Thomas/Google Drive/Grad/Lab/Projects/Phase II Projects - Analysis & Write-up/Masters/Results/Analyses/ABHV2018.csv", na.strings="\"\"")
#Home
myd <- read.csv("C:/Users/Kiersten/Google Drive/Grad/Lab/Projects/Phase II Projects - Analysis & Write-up/Masters/Results/Analyses/ABHV2018.csv", na.strings="\"\"")
View(myd)


# ETHANOL #####

###Data Frame Preparation###

##Subset for Substance
mydetoh <- subset(myd, Substance == "Ethanol")
View(mydetoh)

##Rescaling
#Rescale concentration to avoid issues with eigen values
mydetoh$recoded.conc <- car::recode(mydetoh$Concentration, "5 =.05; 20 =.20; 40 =.40")

##Subset for Consumption Pattern Variables
mydetoh.noCTRL <- subset(mydetoh, Condition != "CTRL")
View(mydetoh.noCTRL)

##Centering
#center concentration to avoid issues with variance inflation
mydetoh$c.conc <- mydetoh$recoded.conc-mean(mydetoh$recoded.conc)
#center TOTAL.ETOH.Swap.Consumed..g.kg. to avoid issues with variance inflation
mydetoh.noCTRL$c.totale <- mydetoh.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.-mean(mydetoh.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.)
#center concentration for noCTRL subset to avoid issues with variance inflation
mydetoh.noCTRL$c.conc <- mydetoh.noCTRL$recoded.conc-mean(mydetoh.noCTRL$recoded.conc)
#center MAC and ROC for noCTRL subset to avoid issues with variance inflation
mydetoh.noCTRL$c.MAC <- mydetoh.noCTRL$MAC-mean(mydetoh.noCTRL$MAC)
mydetoh.noCTRL$c.ROC <- mydetoh.noCTRL$ROC-mean(mydetoh.noCTRL$ROC)
mydetoh.noCTRL$c.MAC3 <- mydetoh.noCTRL$MAC3-mean(mydetoh.noCTRL$MAC3)
mydetoh.noCTRL$c.ROC3 <- mydetoh.noCTRL$ROC3-mean(mydetoh.noCTRL$ROC3)

###Variable Coding Adjustment###

#adjust Age to contrast coding
contrasts(mydetoh$Age)=contr.sum(2)
contrasts(mydetoh$Age)
#Returns:
#           [,1]
#Adolescent    1
#Adult        -1

#adjust Condition to contrast coding
contrasts(mydetoh$Condition)=contr.sum(2)
contrasts(mydetoh$Condition)
#Returns:
#     [,1]
#CTRL    1
#EtOH   -1

contrasts(mydetoh.noCTRL$Age)=contr.sum(2)
contrasts(mydetoh.noCTRL$Age)

contrasts(mydetoh.noCTRL$Condition)=contr.sum(2)
contrasts(mydetoh.noCTRL$Condition)

## ETHANOL AVERSIVES ANALYSES#####

### Ethanol Aversives GLMER (with EtOH vs CTRL)####
Eavers <-glmer(Total.Aversive ~ c.conc*Age*Condition 
                  + (c.conc|RatID), data=mydetoh, family=poisson)
summary(Eavers)

#Post Hocs & Planned Contrasts

#Checking to see if Adolescent vs Adult IAE rats were different without correction as this is the only relevant comparison here
Eavers.emm.c<- emmeans(Eavers,~ Condition)
summary(Eavers.emm.c, type = "response")

### Ethanol Aversives GLMER (EtOH Group Only: Total EtOH Consumed) ######

EaversTot <-glmer(Total.Aversive ~ c.conc*Age*c.totale 
               + (c.conc|RatID), data=mydetoh.noCTRL, family=poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss1 <- getME(EaversTot,c("theta","fixef"))
EaversTot <- update(EaversTot,start=ss1,control=glmerControl(optCtrl=list(maxfun=2e6)))
summary(EaversTot)

#Post Hocs & Planned Concrasts
EaversTot.emm.a<- emmeans(EaversTot,~ Age)
summary(EaversTot.emm.a, type = "response")


### Ethanol Aversives GLMER (EtOH Group Only: MAC & ROC) ######

EaversMR <-glmer(Total.Aversive ~ c.conc+Age+c.MAC+c.ROC
                 + c.conc:Age
                 + c.conc:c.MAC
                 + c.conc:c.ROC
                 + Age:c.MAC
                 + Age:c.ROC
                 + c.conc:Age:c.MAC
                 + c.conc:Age:c.ROC
                 + (c.conc|RatID), data=mydetoh.noCTRL, family=poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss3 <- getME(EaversMR,c("theta","fixef"))
EaversMR <- update(EaversMR,start=ss3,control=glmerControl(optCtrl=list(maxfun=2e6)))
summary(EaversMR)
vif(EaversMR)
AIC(EaversMR, EaversTot)
BIC(EaversMR, EaversTot)

#Make and save residual distribution plot
#make PNG file
png("EaversMR Res Plot.png", width = 300, height = 300)
#plot residual density function
plot(density(residuals(EaversMR)), 
     main="", xlab="", frame= FALSE)
#Add normal distribution to the residual plot for comparison to check assumption of normality
EaversMR.res = residuals(EaversMR)
EavMR.m = mean(EaversMR.res)
EavMR.std = sqrt(var(EaversMR.res))
curve(dnorm(x, mean=EavMR.m, sd=EavMR.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
#close the file
dev.off()


### Ethanol Aversives GLMER (EtOH Group Only: MAC3 & ROC3) ######

EaversMR3 <-glmer(Total.Aversive ~ c.conc+Age+c.MAC3+c.ROC3
                 + c.conc:Age
                 + c.conc:c.MAC3
                 + c.conc:c.ROC3
                 + Age:c.MAC3
                 + Age:c.ROC3
                 + c.conc:Age:c.MAC3
                 + c.conc:Age:c.ROC3
                 + (c.conc|RatID), data=mydetoh.noCTRL, family=poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss12 <- getME(EaversMR3,c("theta","fixef"))
EaversMR3 <- update(EaversMR3,start=ss12,control=glmerControl(optCtrl=list(maxfun=2e9)))
summary(EaversMR3)
vif(EaversMR3)
AIC(EaversMR3, EaversMR, EaversTot)
BIC(EaversMR3, EaversMR, EaversTot)

#Make and save residual distribution plot
#make PNG file
png("EaversMR3 Res Plot.png", width = 300, height = 300)
#plot residual density function
plot(density(residuals(EaversMR3)), 
     main="", xlab="", frame= FALSE)
#Add normal distribution to the residual plot for comparison to check assumption of normality
EaversMR3.res = residuals(EaversMR3)
EavMR3.m = mean(EaversMR3.res)
EavMR3.std = sqrt(var(EaversMR3.res))
curve(dnorm(x, mean=EavMR3.m, sd=EavMR3.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
#close the file
dev.off()


## ETHANOL HEDONICS ######

### Ethanol Hedonics GLMER (with EtOH vs CTRL)####
Ehed <-glmer(Total.Hedonic...MM.~c.conc*Age*Condition
                + (c.conc|RatID), data=mydetoh, family=poisson) #Either syntax works for Intercept & Slope inclusion
summary(Ehed)

#Post Hocs & Planned Contrasts

#Use emmeans to get means for Conditions and summary toe back-transform using 'type="response"'
Ehed.emm.c <- emmeans(Ehed,~ Condition)
summary(Ehed.emm.c, type = "response")

#Getting bs for Condition. Remember that c.conc was rescaled so move the decimal to the left 2 times.
Ehed.emm.c_c <- emtrends(Ehed, ~c.conc*Condition, var="c.conc")
Ehed.emm.c_c


### Ethanol Hedonics GLMER (EtOH Group Only: Total EtOH Consumed) ######

EhedTot <-glmer(Total.Hedonic...MM. ~ c.conc*Age*c.totale 
                  + (c.conc|RatID), data=mydetoh.noCTRL, family=poisson)
summary(EhedTot)

#Post Hocs and Planned Contrasts
EhedTot.emm.a <- emmeans(EhedTot,~ Age)
summary(EhedTot.emm.a, type = "response")


### Ethanol Hedonics GLMER (EtOH Group Only: MAC & ROC) ######

EhedMR <-glmer(Total.Hedonic...MM. ~ c.conc+Age+c.MAC+c.ROC
                 + c.conc:Age
                 + c.conc:c.MAC
                 + c.conc:c.ROC
                 + Age:c.MAC
                 + Age:c.ROC
                 + c.conc:Age:c.MAC
                 + c.conc:Age:c.ROC
                 + (c.conc|RatID), data=mydetoh.noCTRL, family=poisson)
summary(EhedMR)
vif(EhedMR)
AIC(EhedMR, EhedTot)
BIC(EhedMR, EhedTot)


#Make and save residual distribution plot
#make PNG file
png("EhedMR Res Plot.png", width = 300, height = 300)
#plot residual density function
plot(density(residuals(EhedMR)), 
     main="", xlab="", frame= FALSE)
#Add normal distribution to the residual plot for comparison to check assumption of normality
EhedMR.res = residuals(EhedMR)
EhedMR.m = mean(EhedMR.res)
EhedMR.std = sqrt(var(EhedMR.res))
curve(dnorm(x, mean=EhedMR.m, sd=EhedMR.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
#close the file
dev.off()


### Ethanol Hedonics GLMER (EtOH Group Only: MAC3 & ROC3) ######

EhedMR3 <-glmer(Total.Hedonic...MM. ~ c.conc+Age+c.MAC3+c.ROC3
               + c.conc:Age
               + c.conc:c.MAC3
               + c.conc:c.ROC3
               + Age:c.MAC3
               + Age:c.ROC3
               + c.conc:Age:c.MAC3
               + c.conc:Age:c.ROC3
               + (c.conc|RatID), data=mydetoh.noCTRL, family=poisson)
summary(EhedMR3)
vif(EhedMR3)
AIC(EhedMR3, EhedMR, EhedTot)
BIC(EhedMR3, EhedMR, EhedTot)


#Make and save residual distribution plot
#make PNG file
png("EhedMR3 Res Plot.png", width = 300, height = 300)
#plot residual density function
plot(density(residuals(EhedMR3)), 
     main="", xlab="", frame= FALSE)
#Add normal distribution to the residual plot for comparison to check assumption of normality
EhedMR3.res = residuals(EhedMR3)
EhedMR3.m = mean(EhedMR3.res)
EhedMR3.std = sqrt(var(EhedMR3.res))
curve(dnorm(x, mean=EhedMR3.m, sd=EhedMR3.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
#close the file
dev.off()

##MR3 Models no longer appear after this point: Similar AICs, with BICs favoring the less complex model.
#Use less complex Total Ethanol variable for interpretation from here on out.

# SUCROSE #####

###Data Frame Preparation###

##subset for Substance
mydsuc <- subset(myd, Substance == "Sucrose")
View(mydsuc)
##Subset for Consumption Pattern Variables
mydsuc.noCTRL <- subset(mydsuc, Condition != "CTRL")
View(mydsuc.noCTRL)

##Rescaling
#convert concentration to molarity and center to avoid issues with scaling & vif
mydsuc$molarity <-recode(mydsuc$Concentration, ".34=.01; 3.4=.1; 34=1")
mydsuc$c.molarity <-mydsuc$molarity-mean(mydsuc$molarity)
#convert concentration to molarity and center to avoid issues with scaling & vif
mydsuc.noCTRL$molarity <-recode(mydsuc.noCTRL$Concentration, ".34=.01; 3.4=.1; 34=1")
mydsuc.noCTRL$c.molarity <-mydsuc.noCTRL$molarity-mean(mydsuc.noCTRL$molarity)

##Centering
#center TOTAL.ETOH.Swap.Consumed..g.kg. to avoid issues with variance inflation
mydsuc.noCTRL$c.totale <- mydsuc.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.-mean(mydsuc.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.)
#center MAC and ROC for noCTRL subset to avoid issues with variance inflation
mydsuc.noCTRL$c.MAC <- mydsuc.noCTRL$MAC-mean(mydsuc.noCTRL$MAC)
mydsuc.noCTRL$c.ROC <- mydsuc.noCTRL$ROC-mean(mydsuc.noCTRL$ROC)

###Variable Coding Adjustment###

#adjust Age and Condition to contrast coding for both datasets
contrasts(mydsuc$Age)=contr.sum(2)
contrasts(mydsuc$Age)

contrasts(mydsuc$Condition)=contr.sum(2)
contrasts(mydsuc$Condition)

contrasts(mydsuc.noCTRL$Age)=contr.sum(2)
contrasts(mydsuc.noCTRL$Age)

contrasts(mydsuc.noCTRL$Condition)=contr.sum(2)
contrasts(mydsuc.noCTRL$Condition)

## SUCROSE AVERSIVES #####

### Sucrose Aversives GLMER (with EtOH vs CTRL)####
Savers <-glmer(Total.Aversive ~ c.molarity*Age*Condition 
               + (c.molarity|RatID), data=mydsuc, family=poisson)
# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss4 <- getME(Savers,c("theta","fixef"))
Savers <- update(Savers,start=ss4,control=glmerControl(optCtrl=list(maxfun=2e6)))
summary(Savers)

#Post Hocs & Planned Contrasts
Savers.emm.a <- emmeans(Savers, ~ Age)
summary(Savers.emm.a, type="response")


### Sucrose Aversives GLMER (EtOH Group Only: Total EtOH Consumed) ######

SaversTot <-glmer(Total.Aversive ~ c.molarity*Age*c.totale 
                  + (c.molarity|RatID), data=mydsuc.noCTRL, family=poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss5 <- getME(SaversTot,c("theta","fixef"))
SaversTot <- update(SaversTot,start=ss5,control=glmerControl(optCtrl=list(maxfun=2e6)))
summary(SaversTot)

##Post Hocs & Planned Contrasts

#Age*Total Ethanol Manual Calculation
#Adolescent
Savers.Ado.b <-fixef(SaversTot)[4]+fixef(SaversTot)[7]
Savers.Ado.b
#Adult
Savers.Adu.b <-fixef(SaversTot)[4]-fixef(SaversTot)[7]
Savers.Adu.b


### Sucrose Aversives GLMER (EtOH Group Only: MAC & ROC) ######
#####CONVERGENCE FAILED#####

SaversMR <-glmer(Total.Aversive ~ c.molarity+Age+c.MAC+c.ROC
                 + c.molarity:Age
                 + c.molarity:c.MAC
                 + c.molarity:c.ROC
                 + Age:c.MAC
                 + Age:c.ROC
                 + c.molarity:Age:c.MAC
                 + c.molarity:Age:c.ROC
                 + (c.molarity|RatID), data=mydsuc.noCTRL, family=poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss6 <- getME(SaversMR,c("theta","fixef"))
SaversMR <- update(SaversMR,start=ss6,control=glmerControl(optCtrl=list(maxfun=2e9)))
summary(SaversMR)
vif(SaversMR)
AIC(SaversMR, SaversTot)
BIC(SaversMR, SaversTot)


#Make and save residual distribution plot
#make PNG file
png("SaversMR Res Plot.png", width = 300, height = 300)
#plot residual density function
plot(density(residuals(SaversMR)), 
     main="", xlab="", frame= FALSE)
#Add normal distribution to the residual plot for comparison to check assumption of normality
SaversMR.res = residuals(SaversMR)
SavMR.m = mean(SaversMR.res)
SavMR.std = sqrt(var(SaversMR.res))
curve(dnorm(x, mean=SavMR.m, sd=SavMR.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
#close the file
dev.off()


## SUCROSE HEDONICS #####

### Sucrose Hedonics GLMER (with EtOH vs CTRL)####
Shed <-glmer(Total.Hedonic...MM. ~ c.molarity*Age*Condition 
               + (c.molarity|RatID), data=mydsuc, family=poisson)
summary(Shed)

#Post Hocs & Planned Contrasts

#Age*Molarity
#Adolescent
Shed.Ado.b <-fixef(Shed)[2]+fixef(Shed)[5]
Shed.Ado.b
#Adult
Shed.Adu.b <-fixef(Shed)[2]-fixef(Shed)[5]
Shed.Adu.b

Shed.emm.a <- emmeans(Shed, ~ Age)
summary(Shed.emm.a, type="response")

Shed.emm.m_a <- emtrends(Shed, ~c.molarity*Age, var="c.molarity")
Shed.emm.m_a


### Sucrose Hedonics GLMER (EtOH Group Only: Total EtOH Consumed) ######

ShedTot <-glmer(Total.Hedonic...MM. ~ c.molarity*Age*c.totale 
                  + (c.molarity|RatID), data=mydsuc.noCTRL, family=poisson)
summary(ShedTot)

#Post Hocs & Planned Contrasts
#Age*Molarity
#Adolescent
ShedTot.Ado.b <-fixef(ShedTot)[2]+fixef(ShedTot)[5]
ShedTot.Ado.b
#Adult
ShedTot.Adu.b <-fixef(ShedTot)[2]-fixef(ShedTot)[5]
ShedTot.Adu.b

#using emtrends instead
ShedTot.emm.a_mol<- emtrends(ShedTot, ~ Age, var = "c.molarity")
ShedTot.emm.a_mol


### Sucrose Hedonics GLMER (EtOH Group Only: MAC & ROC) ######

ShedMR <-glmer(Total.Hedonic...MM. ~ c.molarity+Age+c.MAC+c.ROC
                 + c.molarity:Age
                 + c.molarity:c.MAC
                 + c.molarity:c.ROC
                 + Age:c.MAC
                 + Age:c.ROC
                 + c.molarity:Age:c.MAC
                 + c.molarity:Age:c.ROC
                 + (c.molarity|RatID), data=mydsuc.noCTRL, family=poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss7 <- getME(ShedMR,c("theta","fixef"))
ShedMR <- update(ShedMR,start=ss7,control=glmerControl(optCtrl=list(maxfun=2e9)))
summary(ShedMR)
vif(ShedMR)
AIC(ShedMR, ShedTot)
BIC(ShedMR, ShedTot)


#Make and save residual distribution plot
#make PNG file
png("ShedMR Res Plot.png", width = 300, height = 300)
#plot residual density function
plot(density(residuals(ShedMR)), 
     main="", xlab="", frame= FALSE)
#Add normal distribution to the residual plot for comparison to check assumption of normality
ShedMR.res = residuals(ShedMR)
ShedMR.m = mean(ShedMR.res)
ShedMR.std = sqrt(var(ShedMR.res))
curve(dnorm(x, mean=ShedMR.m, sd=ShedMR.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
#close the file
dev.off()


# WATER #####

###Data Frame Preparation###

#Load new dataset made in excel to get rid of additional variables in Substance column. subset(), and select(filter(),cols) will not work for this.
#Laptop
mydh2o <- read.csv("C:/Users/Thomas/Google Drive/Grad/Lab/Projects/Phase II Projects - Analysis & Write-up/Masters/Results/Analyses/ABHV2018H2O.csv", na.strings="\"\"")
#Home computer
#mydh2o <- read.csv("C:/Users/Kiersten/Google Drive/Grad/Lab/Projects/Phase II Projects - Analysis & Write-up/Masters/Results/Analyses/ABHV2018H2O.csv", na.strings="\"\"")
View(mydh2o)

#Adjust contrasts to sum-to-zero
contrasts(mydh2o$Substance)=contr.sum(2)
contrasts(mydh2o$Substance)

contrasts(mydh2o$Age)=contr.sum(2)
contrasts(mydh2o$Age)

contrasts(mydh2o$Condition)=contr.sum(2)
contrasts(mydh2o$Condition)

##Filter for Consumption Pattern Variables
library("dplyr", lib.loc="~/R/win-library/3.6")
mydh2o.noCTRL <- filter(mydh2o, Condition != "CTRL")
View(mydh2o.noCTRL)
detach("package:dplyr", unload=TRUE)

##Rescaling

##Centering
#center TOTAL.ETOH.Swap.Consumed..g.kg. to avoid issues with variance inflation
mydh2o.noCTRL$c.totale <- mydh2o.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.-mean(mydh2o.noCTRL$TOTAL.ETOH.Swap.Consumed..g.kg.)
#center MAC and ROC for noCTRL subset to avoid issues with variance inflation
mydh2o.noCTRL$c.MAC <- mydh2o.noCTRL$MAC-mean(mydh2o.noCTRL$MAC)
mydh2o.noCTRL$c.ROC <- mydh2o.noCTRL$ROC-mean(mydh2o.noCTRL$ROC)

###Variable Coding Adjustment###

#Adjust contrasts to sum-to-zero
contrasts(mydh2o.noCTRL$Substance)=contr.sum(2)
contrasts(mydh2o.noCTRL$Substance)

contrasts(mydh2o.noCTRL$Age)=contr.sum(2)
contrasts(mydh2o.noCTRL$Age)


## WATER AVERSIVES #####

### Water Aversives GLMER (with EtOH vs CTRL)####
Havers <-glmer(Total.Aversive ~ Substance*Age*Condition 
               + (1|RatID), data=mydh2o, family=poisson)
summary(Havers)

#Post Hocs & Planned Contrasts

#Condition
Havers.emm.c <- emmeans(Havers, ~ Condition)
summary(Havers.emm.c, type = "response")
#Substance*Age
Havers.emm.s_a <- emmeans(Havers, ~ Substance*Age)
#run Z-tests no adjustment for familywise error rate
pairs(Havers.emm.s_a, adjust="None")
Havers.emm.s_a
#Visualizing the differences
plot(Havers.emm.s_a)

#Age*Condition
Havers.emm.s_c <- emmeans(Havers, ~ Substance*Condition)
#run Z-tests no adjustment for familywise error rate
pairs(Havers.emm.s_c, adjust="None")
Havers.emm.s_c
#Visualizing the differences
plot(Havers.emm.s_c)

#Substance*Age*Condition
Havers.emm.s_a_c <- emmeans(Havers, ~ Substance*Age*Condition)
#run Z-tests no adjustment for familywise error rate
pairs(Havers.emm.s_a_c, adjust="None")
Havers.emm.s_a_c
#Visualizing the differences
plot(Havers.emm.s_a_c)


### Water Aversives GLMER (EtOH Group Only: Total EtOH Consumed) ######

HaversTot <-glmer(Total.Aversive ~ Substance*Age*c.totale 
                  + (1|RatID), data=mydh2o.noCTRL, family=poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss8 <- getME(HaversTot,c("theta","fixef"))
HaversTot <- update(HaversTot,start=ss8,control=glmerControl(optCtrl=list(maxfun=2e6)))
summary(HaversTot)

#Post Hocs & Planned Contrasts

#Substance*Age
HaversTot.emm.s_a <- emmeans(HaversTot, ~ Substance*Age)
#run Z-tests no adjustment for familywise error rate
pairs(HaversTot.emm.s_a, adjust="None")
HaversTot.emm.s_a
#Visualizing the differences
plot(HaversTot.emm.s_a)

#Substance*c.totale
HaversTot.emt.s_c <- emtrends(HaversTot, ~ Substance, var="c.totale")
#run Z-tests no adjustment for familywise error rate
pairs(HaversTot.emt.s_c, adjust="None")
HaversTot.emt.s_c
#Visualizing the differences
plot(HaversTot.emt.s_c)

#Substance*Age*c.totale
HaversTot.emt.s_a_c <- emtrends(HaversTot, ~ Substance*Age, var = "c.totale")
#run Z-tests no adjustment for familywise error rate
pairs(HaversTot.emt.s_a_c, adjust="None")
#pairs looks alot like Substance*Age but it is different, it is the comparisons of slopes
HaversTot.emt.s_a_c
#Visualizing the differences
plot(HaversTot.emt.s_a_c)


### Water Aversives GLMER (EtOH Group Only: MAC & ROC) ######

HaversMR <-glmer(Total.Aversive ~ Substance+Age+c.MAC+c.ROC
                 + Substance:Age
                 + Substance:c.MAC
                 + Substance:c.ROC
                 + Age:c.MAC
                 + Age:c.ROC
                 + Substance:Age:c.MAC
                 + Substance:Age:c.ROC
                 + (1|RatID), data=mydh2o.noCTRL, family=poisson)

# Model did not converge, used code below to extend # of iterations and start from where the previous model left off.
ss9 <- getME(HaversMR,c("theta","fixef"))
HaversMR <- update(HaversMR,start=ss9,control=glmerControl(optCtrl=list(maxfun=2e9)))
summary(HaversMR)
vif(HaversMR)
AIC(HaversMR, HaversTot)
BIC(HaversMR, HaversTot)


#Make and save residual distribution plot
#make PNG file
png("HaversMR Res Plot.png", width = 300, height = 300)
#plot residual density function
plot(density(residuals(HaversMR)), 
     main="", xlab="", frame= FALSE)
#Add normal distribution to the residual plot for comparison to check assumption of normality
HaversMR.res = residuals(HaversMR)
HavMR.m = mean(HaversMR.res)
HavMR.std = sqrt(var(HaversMR.res))
curve(dnorm(x, mean=HavMR.m, sd=HavMR.std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
#close the file
dev.off()


## WATER HEDONICS #####

### Water Hedonics GLMER (with EtOH vs CTRL)####
Hhed <-glmer(Total.Hedonic...MM. ~ Substance*Age*Condition 
               + (1|RatID), data=mydh2o, family=poisson)
summary(Hhed)

#Post Hocs & Planned Contrasts

#Substance*Condition
Hhed.emm.s_c <- emmeans(Hhed, ~ Substance*Condition)
#run Z-tests no adjustment for familywise error rate
pairs(Hhed.emm.s_c, adjust="None")
Hhed.emm.s_c
#Visualizing the differences
plot(Hhed.emm.s_c)


### Water Hedonics GLMER (EtOH Group Only: Total EtOH Consumed) ######

HhedTot <-glmer(Total.Hedonic...MM. ~ Substance*Age*c.totale 
                  + (1|RatID), data=mydh2o.noCTRL, family=poisson)
summary(HhedTot)

#Post Hocs & Planned Contrasts
#Substance*c.totale
HhedTot.emt.s_c <- emtrends(HhedTot, ~ Substance, var="c.totale")
#run Z-tests no adjustment for familywise error rate
pairs(HhedTot.emt.s_c, adjust="None")
HhedTot.emt.s_c
#Visualizing the differences
plot(HhedTot.emt.s_c)

#Substance*Age*c.totale
HhedTot.emt.s_a_c <- emtrends(HhedTot, ~ Substance*Age, var = "c.totale")
#run Z-tests no adjustment for familywise error rate
pairs(HhedTot.emt.s_a_c, adjust="None")
#pairs looks alot like Substance*Age but it is different, it is the comparisons of slopes
HhedTot.emt.s_a_c
#Visualizing the differences
plot(HhedTot.emt.s_a_c)