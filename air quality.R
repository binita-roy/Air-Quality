
### Loading the required librarires

library (readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plm)
library(Hmisc)
library(stargazer)
library(car)

### Loading the dataset

air <- read_excel("D:\\Projects\\Effect of Gas Emission on Air Quality\\Air Quality Dataset.xlsx",
                    sheet = "Analysis", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric",
                                                      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                      "numeric", "numeric"))

view(air)

### Correlation Table

## Creating the function for creating correlation table

cor <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  require(xtable)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

Cormat <- cor(air[,3:14], method = "spearman", removeTriangle = "upper")

print(Cormat)
write.csv(Cormat, file = "Correlation Table_air quality_spearman.csv")

### Descriptive Statistics

## Creating a descriptive Statistic keeping year-wise distribution

mean <- aggregate(air[,6:14], by= list (Year=air$Year), mean, na.rm=TRUE)
max <- aggregate(air[,6:14], by= list (Year=air$Year), max, na.rm=TRUE)
min <- aggregate(air[,6:14], by= list (Year=air$Year), min, na.rm=TRUE)
sd <- aggregate(air[,6:14], by= list (Year=air$Year), sd, na.rm=TRUE)

ds_air <- cbind (mean, max, min, sd)

view(ds_air)

write.csv(ds_air, file = "Descriptive Statistics_air quality.csv")

## Creating plot according to control variables

air_recode <- air %>% mutate(DSf = recode_factor(DS, 
                                              `0` = "Least Developed",
                                              `1` = "Less Developed",
                                              `2` = "More Developed"
), IGf = recode_factor(IG,
                       `0` = "Low Income",
                       `1` = "Lower Middle Income",
                       `2` = "Upper Middle Income",
                       `3` = "High Income"))
view (air_recode)

# Creating Plot according to Development status

# PM2.5 air pollution, mean annual exposure

air_ds_PME_summary <- air_recode %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_PME_DS = mean(PME),  # calculates the mean of each group
            sd_PME_DS = sd(PME), # calculates the standard deviation of each group
            n_PME = n(),  # calculates the sample size per group
            SE_PME_DS = sd(PME)/sqrt(n())) # calculates the standard error of each group
View(air_ds_PME_summary)

air_DS_PME_plot <-  ggplot(data=air_ds_PME_summary, aes(x= DSf
                                                      ,y= mean_PME_DS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development status", y = "PM2.5 air pollution, mean annual exposure (micrograms per cubic meter)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_PME_DS - sd_PME_DS, ymax = mean_PME_DS + sd_PME_DS), width=0.2)
plot(air_DS_PME_plot)

# PM2.5 air pollution, population exposed to levels exceeding WHO guideline value

air_ds_PMWHO_summary <- air_recode %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_PMWHO_DS = mean(PMWHO),  # calculates the mean of each group
            sd_PMWHO_DS = sd(PMWHO), # calculates the standard deviation of each group
            n_PMWHO = n(),  # calculates the sample size per group
            SE_PMWHO_DS = sd(PMWHO)/sqrt(n())) # calculates the standard error of each group
View(air_ds_PMWHO_summary)

air_DS_PMWHO_plot <-  ggplot(data=air_ds_PMWHO_summary, aes(x= DSf
                                                        ,y= mean_PMWHO_DS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development status", y = "PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_PMWHO_DS - sd_PMWHO_DS, ymax = mean_PMWHO_DS + sd_PMWHO_DS), width=0.2)
plot(air_DS_PMWHO_plot)

# CO2 emissions

air_ds_COE_summary <- air_recode %>% # the names of the new data frame and the data frame to be summarised
  group_by(DSf) %>%   # the grouping variable
  summarise(mean_COE_DS = mean(COE),  # calculates the mean of each group
            sd_COE_DS = sd(COE), # calculates the standard deviation of each group
            n_COE = n(),  # calculates the sample size per group
            SE_COE_DS = sd(COE)/sqrt(n())) # calculates the standard error of each group
View(air_ds_COE_summary)

air_DS_COE_plot <-  ggplot(data=air_ds_COE_summary, aes(x= DSf
                                                            ,y= mean_COE_DS)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Development status", y = "CO2 Emissions  (metric tons per capita)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_COE_DS - sd_COE_DS, ymax = mean_COE_DS + sd_COE_DS), width=0.2)
plot(air_DS_COE_plot)

# Creating Plot according to Income Group

# PM2.5 air pollution, mean annual exposure

air_ig_PME_summary <- air_recode %>% # the names of the new data frame and the data frame to be summarised
  group_by(IGf) %>%   # the grouping variable
  summarise(mean_PME_IG = mean(PME),  # calculates the mean of each group
            sd_PME_IG = sd(PME), # calculates the standard deviation of each group
            n_IG = n(),  # calculates the sample size per group
            SE_PME_IG = sd(PME)/sqrt(n())) # calculates the standard error of each group
View(air_ig_PME_summary)

air_IG_PME_plot <-  ggplot(data=air_ig_PME_summary, aes(x= IGf
                                                        ,y= mean_PME_IG)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "green")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Income Group", y = "PM2.5 air pollution, mean annual exposure (micrograms per cubic meter)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_PME_IG - sd_PME_IG, ymax = mean_PME_IG + sd_PME_IG), width=0.2)
plot(air_IG_PME_plot)

# PM2.5 air pollution, population exposed to levels exceeding WHO guideline value

air_ig_PMWHO_summary <- air_recode %>% # the names of the new data frame and the data frame to be summarised
  group_by(IGf) %>%   # the grouping variable
  summarise(mean_PMWHO_IG = mean(PMWHO),  # calculates the mean of each group
            sd_PMWHO_IG = sd(PMWHO), # calculates the standard deviation of each group
            n_PMWHO = n(),  # calculates the sample size per group
            SE_PMWHO_IG = sd(PMWHO)/sqrt(n())) # calculates the standard error of each group
View(air_ig_PMWHO_summary)

air_ig_PMWHO_plot <-  ggplot(data=air_ig_PMWHO_summary, aes(x= IGf
                                                            ,y= mean_PMWHO_IG)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "green")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Income Group", y = "PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_PMWHO_IG - sd_PMWHO_IG, ymax = mean_PMWHO_IG + sd_PMWHO_IG), width=0.2)
plot(air_ig_PMWHO_plot)

# CO2 emissions

air_ig_COE_summary <- air_recode %>% # the names of the new data frame and the data frame to be summarised
  group_by(IGf) %>%   # the grouping variable
  summarise(mean_COE_IG = mean(COE),  # calculates the mean of each group
            sd_COE_IG = sd(COE), # calculates the standard deviation of each group
            n_COE_IG = n(),  # calculates the sample size per group
            SE_COE_IG = sd(COE)/sqrt(n())) # calculates the standard error of each group
View(air_ig_COE_summary)

air_IG_COE_plot <-  ggplot(data=air_ig_COE_summary, aes(x= IGf
                                                        ,y= mean_COE_IG)) +
  geom_bar(stat="identity", position = "dodge", width = 0.5, fill = c("orchid4", "red4", "blue4", "green")) +
  scale_x_discrete(expand = waiver(),position = "bottom") + labs (x = "Income Group", y = "CO2 Emissions  (metric tons per capita)")+ theme(axis.text.x = element_text(angle = 40, hjust = 1.0, vjust = 1.0), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mean_COE_IG - sd_COE_IG, ymax = mean_COE_IG + sd_COE_IG), width=0.2)
plot(air_IG_COE_plot)


### Panel Data Regression

attach(air)

pdata <- pdata.frame(air, index= c("Countries", "Year"))

## Plotting the Changes of the Dependent Variables over the time period

scatterplot(PME~Year|Countries, data=pdata, ylab = "PM2.5 air pollution, mean annual exposure ( micrograms per cubic meter)")
scatterplot(PMWHO~Year|Countries, data=pdata, ylab = "PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)")


## Plotting the Changes of the Dependent Variables according to Control Variables

DSFact <- factor(pdata$DS, levels=0:2, labels=c("Least Developed", "Less Developed", "More Developed"))
View(DSFact)

RegionFact <- factor(pdata$Region, levels=0:21, labels=c("EAf", "MAf", "SAf", "WAf",
                                                        "NAf", "WAs", "CAs", "SAs",
                                                        "EAs", "SEAs", "Car", "CA",
                                                        "SAm", "Aus/NZ", "Mel", "Mic",
                                                        "Pol", "EE", "NE", "SE",
                                                        "WE", "NA"))
RegionFact

IGFact <- factor(pdata$IG, levels=0:3, labels=c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"))
View(IGFact)

scatterplot(PME~DSFact|Year, data=pdata, ylab = "PM2.5 air pollution, mean annual exposure ( micrograms per cubic meter)", xlab = "Development Status")
scatterplot(PMWHO~DSFact|Year, data=pdata, ylab = "PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)", xlab = "Development Status")


scatterplot(PME~RegionFact|Year, data=pdata, ylab = "PM2.5 air pollution, mean annual exposure ( micrograms per cubic meter)", xlab = "Region")
scatterplot(PMWHO~RegionFact|Year, data=pdata, ylab = "PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)", xlab = "Region")



scatterplot(PME~IGFact|Year, data=pdata, ylab = "PM2.5 air pollution, mean annual exposure ( micrograms per cubic meter)", xlab = "Income Group")
scatterplot(PMWHO~IGFact|Year, data=pdata, ylab = "PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)", xlab = "Income Group")


## Regression models

# OLS Model

pooling_PME_1 <- plm(PME~ factor(pdata$DS) + factor(pdata$IG) + COE + AME + MEE + ANOE + NOE + ME + NOEE, data = pdata, model = "pooling")
summary(pooling_PME_1)

pooling_PMWHO_1 <- plm(PMWHO~ factor(pdata$DS) + factor(pdata$IG) + COE + AME + MEE + ANOE + NOE + ME + NOEE, data = pdata, model = "pooling")
summary(pooling_PMWHO_1)

# Fixed Effect models

fixed_PME_1 <- plm(PME~ factor(pdata$DS) + factor(pdata$IG) + COE + AME + MEE + ANOE + NOE + ME + NOEE, data = pdata, model = "within")
summary(fixed_PME_1)

fixed_PMWHO_1 <- plm(PMWHO~ factor(pdata$DS) + factor(pdata$IG) + COE + AME + MEE + ANOE + NOE + ME + NOEE, data = pdata, model = "within")
summary(fixed_PMWHO_1)

# Random Effect models

random_PME_1 <- plm(PME~ factor(pdata$DS) + factor(pdata$IG) + COE + AME + MEE + ANOE + NOE + ME + NOEE, data = pdata, model = "random")
summary(random_PME_1)

random_PMWHO_1 <- plm(PMWHO~ factor(pdata$DS) + factor(pdata$IG) + COE + AME + MEE + ANOE + NOE + ME + NOEE, data = pdata, model = "random")
summary(random_PMWHO_1)

# Tidy up the results

Comp1 <- stargazer(pooling_PME_1, fixed_PME_1, random_PME_1,  
                   title="Panel regression Models of PM2.5 air pollution, mean annual exposure ( micrograms per cubic meter)", dep.var.labels=c("PM2.5 air pollution, mean annual exposure ( micrograms per cubic meter)"),
                    type="text",
                   column.labels=c("Pooled OLS", "Fixed Effect Model", "Random Effect Model"), ci.level = 0.95,
                   df=TRUE, digits=4, align = TRUE, out = 'Comparison of PME Models.doc')

Comp2 <- stargazer(pooling_PMWHO_1, fixed_PMWHO_1, random_PMWHO_1,  
                   title="Panel regression Models of PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)", dep.var.labels=c("PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)"),
                   type="text",
                   column.labels=c("Pooled OLS", "Fixed Effect Model", "Random Effect Model"), ci.level = 0.95,
                   df=TRUE, digits=4, align = TRUE, out = 'Comparison of PMWHO Models.doc')

## Selecting the suitable model

# LM Test for Random Effects Model vs OLS Model

plmtest(pooling_PME_1, type = "bp", effect = "twoways")

plmtest(pooling_PMWHO_1, type = "bp", effect = "twoways")

## LM Test for Fixed Effects Model vs OLS Model

pFtest(fixed_PME_1, pooling_PME_1)

pFtest(fixed_PMWHO_1, pooling_PMWHO_1)

## Hausman Test for Random Model vs Fixed Model (If p < 0.05, Fixed effect Model should be chosen, otherwise Random effect Model)

phtest(random_PME_1, fixed_PME_1)

phtest(random_PMWHO_1, fixed_PMWHO_1)