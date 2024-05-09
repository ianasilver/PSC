# Getting Started ####
# Written by:
# Started on: 4.6.22
# R Version: 4.1.2 (2021-11-01)
# Platform: x86_64-pc-linux-gnu (64-bit)


library(foreign)
library(ggplot2)
library(dplyr)
library(lme4)
library(ggridges)
library(extrafont)
library(Hmisc)
library(psych)
library(lmerTest)
library(pbkrtest)
library(MuMIn)
library(pastecs)
library(pracma)
library(matrixStats)
library(purrr)
library(mice)
library(VIM)
library(tidyverse)
library(egg)
library(data.table)
library(doParallel)
library(parallel)
library(foreach)
library(doRNG)
library(mice)
library("foreign")
library("moments")
library("pastecs")
library("psych")
library("lavaan")
library("ggplot2")
library("extrafont")
library("data.table")
library(dplyr)
library(tidyverse)
library("sjmisc")

options(scipen=100, digits=12)
options(max.print = 500000000)


setwd("")
A<-fread("Data/Clean Data.csv", sep = ",", fill=TRUE,  na.strings="NA")
names(A)
table(A$AGE00)

# Limiting Data Frame ####

names(A)

table(A$I09)


B<-(A[, c("R0000100",
          "A00","A01","A02","A03","A04","A05","A06","A07","A08","A09","A10",
          "I00","I01","I02","I03","I04","I05","I06","I07","I08","I09","I10",
          "RMHI00_1","MHI00_2","RMHI00_3","MHI00_4","RMHI00_5","MHI00",
          "RMHI02_1","MHI02_2","RMHI02_3","MHI02_4","RMHI02_5","MHI02",
          "RMHI04_1","MHI04_2","RMHI04_3","MHI04_4","RMHI04_5","MHI04",
          "RMHI06_1","MHI06_2","RMHI06_3","MHI06_4","RMHI06_5","MHI06",
          "RMHI08_1","MHI08_2","RMHI08_3","MHI08_4","RMHI08_5","MHI08",
          "RMHI10_1","MHI10_2","RMHI10_3","MHI10_4","RMHI10_5","MHI10",
          "PIB16","PATIB16","MATIB16","Hispanic",  
          "White","Black","AIndian","Asian","Other","Male", "Sample", "BBPHH","CC1Y", "HT97", "HHNW97",
          "BMEA97", "BFEA97", "HEALTH97","RHEALTH97","PHEALTH97","RPHEALTH97","SSAS97", "THAS97", "PFAS97", "LFS97","ABFS97",
          "TGAS97", "RTGAS97","DFAS97", "RDFAS97","PPSM97", "PPDR97", "PPIG97", "PPID97", "PPHS97", "FIC97","Weight01",  
          "FINCH","INCH", "Height01","DOW01")])
# Descriptive Statistics ####

sink("Real Data Example/Results/Descriptive Statistics.txt")
stat.desc(B)
sink()



# Missing Case Analysis ####

names(B)

names(B)
stat.desc(B)

Z<-B[complete.cases(B[,c("A00","A01","A02","A03","A04","A05","A06","A07","A08","A09","A10",
                         "I00","I01","I02","I03","I04","I05","I06","I07","I08","I09","I10",
                         "MHI00","MHI02","MHI04","MHI06","MHI08","MHI10","PIB16","Hispanic",  
                         "White","Black","AIndian","Asian","Other","Male", "Sample","BBPHH","CC1Y", "HT97")]),]
names(Z)

Z$CCDI<-1

Z.1<-Z[, c("R0000100","CCDI")]


Z.1<-Z.1[, c("R0000100","CCDI")]

MCDF<-merge(B, Z.1, by = c("R0000100"), all.x = T)

MCDF$CCDI<-as.numeric(MCDF$CCDI)

table(MCDF$CCDI, useNA="always")

MCDF$CCDI[is.na(MCDF$CCDI)]<-0

table(MCDF$CCDI, useNA="always")

nlevels(MCDF$CCDI)

library(finalfit)

jpeg("Real Data Example/Results/Missing Values Plot.jpeg", width = 500, height = 500, quality = 100)
missing_plot(MCDF)
dev.off()

jpeg("Real Data Example/Results/Missing Pattern Plot.jpeg", width = 1000, height = 1000, quality = 100)
missing_pattern(MCDF)
dev.off()

sink("Real Data Example/Results/Missing Pattern Results.txt")
missing_pattern(MCDF)
sink()


names(MCDF)

sink("Real Data Example/Results/Missing Case Analysis (t_test).txt")
stat.desc(MCDF[which(MCDF$CCDI == 1),])
stat.desc(MCDF[which(MCDF$CCDI == 0),])
print("# t-test")
print("# Complete Cases (0 = incomeplete cases; 1 = complete cases)")
table(MCDF$CCDI)
lapply(MCDF[, c("A00","A01","A02","A03","A04","A05","A06","A07","A08","A09","A10",
                "I00","I01","I02","I03","I04","I05","I06","I07","I08","I09","I10",
                "MHI00","MHI02","MHI04","MHI06","MHI08","MHI10","PIB16","Hispanic",  
                "White","Black","AIndian","Asian","Other","Male", "Sample","BBPHH","CC1Y", "HT97")], function(x, na.rm = FALSE) t.test(x~MCDF$CCDI))
sink()

library(effsize)
sink("Real Data Example/Results/Missing Case Analysis (Cohen's d).txt")
print("# Year_1")
print("# Complete Cases (0 = incomeplete cases; 1 = complete cases)")
table(MCDF$CCDI)
lapply(MCDF[, c("A00","A01","A02","A03","A04","A05","A06","A07","A08","A09","A10",
                "I00","I01","I02","I03","I04","I05","I06","I07","I08","I09","I10",
                "MHI00","MHI02","MHI04","MHI06","MHI08","MHI10","PIB16","Hispanic",  
                "White","Black","AIndian","Asian","Other","Male", "Sample","BBPHH","CC1Y", "HT97")], function(x, na.rm = FALSE) cohen.d(x~MCDF$CCDI))
sink()


# Model 1: Period Specific Change Model ####
## PSC Model ####
names(B)

B$MHI00z<-scale(B$MHI00)
B$MHI02z<-scale(B$MHI02)
B$MHI04z<-scale(B$MHI04)
B$MHI06z<-scale(B$MHI06)
B$MHI08z<-scale(B$MHI08)
B$MHI10z<-scale(B$MHI10)

F1<-'

# Difference Terms Arrest

DA1=~1*A00+2*A01
DA2=~1*A01+2*A02
DA3=~1*A02+2*A03
DA4=~1*A03+2*A04
DA5=~1*A04+2*A05
DA6=~1*A05+2*A06
DA7=~1*A06+2*A07
DA8=~1*A07+2*A08
DA9=~1*A08+2*A09
DA10=~1*A09+2*A10

# Intercept and Slopes Arrest Observed
I_NA=~1*A00+1*A01+1*A02+1*A03+1*A04+1*A05+1*A06+1*A07+1*A08+1*A09+1*A10

# Difference Terms MHI

DMHI1=~1*MHI00z+3*MHI02z
DMHI2=~1*MHI02z+3*MHI04z
DMHI3=~1*MHI04z+3*MHI06z
DMHI4=~1*MHI06z+3*MHI08z
DMHI5=~1*MHI08z+3*MHI10z

# Intercept and Slopes MHI Observed 
I_MHI=~1*MHI00z+1*MHI02z+1*MHI04z+1*MHI06z+1*MHI08z+1*MHI10z

I_MHI~~I_NA

I_DA=~1*DA1+1*DA2+1*DA3+1*DA4+1*DA5+1*DA6+1*DA7+1*DA8+1*DA9+1*DA10
I_DMHI=~1*DMHI1+1*DMHI2+1*DMHI3+1*DMHI4+1*DMHI5

I_DA~~I_DMHI

# Regressions

DMHI2~DA1+DA2+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              
DMHI3~DA3+DA4+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              
DMHI4~DA5+DA6+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              
DMHI5~DA7+DA8+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              

DA3~DMHI1+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              
DA4~DMHI1+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              
DA5~DMHI2+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              
DA6~DMHI2+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              
DA7~DMHI3+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              
DA8~DMHI3+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              
DA9~DMHI4+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              
DA10~DMHI4+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
              

'

M1<-sem(F1,  orthogonal = T, fixed.x =F, data=B, estimator = "ML",missing = "ML")
summary(M1, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T)

sink("Real Data Example/Results/PSC Model.txt")
summary(M1, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T)
sink()




## LCS Model ####

names(B)
F2<-'

# Phantoms
PA00=~1*A00
PA01=~1*A01
PA02=~1*A02
PA03=~1*A03
PA04=~1*A04
PA05=~1*A05
PA06=~1*A06
PA07=~1*A07
PA08=~1*A08
PA09=~1*A09
PA10=~1*A10

PMHI00z=~1*MHI00z
PMHI02z=~1*MHI02z
PMHI04z=~1*MHI04z
PMHI06z=~1*MHI06z
PMHI08z=~1*MHI08z
PMHI10z=~1*MHI10z

# Difference Terms
DPMHI02z=~1*PMHI02z
DPMHI04z=~1*PMHI04z
DPMHI06z=~1*PMHI06z
DPMHI08z=~1*PMHI08z
DPMHI10z=~1*PMHI10z

DPA01=~1*PA01
DPA02=~1*PA02
DPA03=~1*PA03
DPA04=~1*PA04
DPA05=~1*PA05
DPA06=~1*PA06
DPA07=~1*PA07
DPA08=~1*PA08
DPA09=~1*PA09
DPA10=~1*PA10

# Autoregressive Paths
PA01~1*PA00
PA02~1*PA01
PA03~1*PA02
PA04~1*PA03
PA05~1*PA04
PA06~1*PA05
PA07~1*PA06
PA08~1*PA07
PA09~1*PA08
PA10~1*PA09

PMHI02z~1*PMHI00z
PMHI04z~1*PMHI02z
PMHI06z~1*PMHI04z
PMHI08z~1*PMHI06z
PMHI10z~1*PMHI08z

# Difference Paths
DPA01~PA00
DPA02~PA01
DPA03~PA02
DPA04~PA03
DPA05~PA04
DPA06~PA05
DPA07~PA06
DPA08~PA07
DPA09~PA08
DPA10~PA09

DPMHI02z~PMHI00z
DPMHI04z~PMHI02z
DPMHI06z~PMHI04z
DPMHI08z~PMHI06z
DPMHI10z~PMHI08z

# Regression Paths
DPA03~DPMHI02z+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
DPA04~DPMHI02z+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
DPA05~DPMHI04z+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
DPA06~DPMHI04z+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
DPA07~DPMHI06z+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
DPA08~DPMHI06z+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
DPA09~DPMHI08z+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
DPA10~DPMHI08z+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97

DPMHI02z~DPA01+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
DPMHI04z~DPA02+DPA03+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
DPMHI06z~DPA04+DPA05+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
DPMHI08z~DPA06+DPA07+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97
DPMHI10z~DPA08+DPA09+PIB16+Hispanic+White+Black+AIndian+Asian+Male+Sample+BBPHH+CC1Y+HT97


# Growth
I_NA=~1*DPA01+1*DPA02+1*DPA03+1*DPA04+1*DPA05+1*DPA06+1*DPA07+1*DPA08+1*DPA09+1*DPA10
I_MHI=~1*DPMHI02z+1*DPMHI04z+1*DPMHI06z+1*DPMHI08z+1*DPMHI10z

I_NA~~1*I_NA
I_MHI~~1*I_MHI
'

M2<-sem(F2,  orthogonal = T, fixed.x = F, data=B, estimator = "ML",missing = "ML")
summary(M2, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T)

sink("Real Data Example/Results/Latent Change-Difference Score Model.txt")
summary(M2, fit.measures= TRUE, standardized = TRUE, ci = TRUE, rsquare = T)
sink()