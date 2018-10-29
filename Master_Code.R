# Data Cleanup
```{r}
library(readr)
dat <- read.csv("~/Desktop/Stats 141SL/Final Project - Intelligence & Connectivity/intelligence and connectivity by age.csv")
dim(dat)

# Variables
par(mfrow=c(2,2))
hist(dat$AGE) #Right skewed uniform with highs around age 20-30
hist(dat$SCHOOL_YRS) #Left skewed unimodal-bimodal centered around 15 school-years
table(dat$GENDER) #Male skewed

# Separating 'Age' into 2 categories to counter biased distribution
dat$AGE1 <- dat$AGE
dat$AGE1[dat$AGE1 > 30] <- "old"
dat$AGE1[dat$AGE1 <= 30] <- "young"
dat$AGE1 <- as.factor(dat$AGE1)
dat$AGE1 <- factor(dat$AGE1, levels=c("young", "old"))
table(dat$AGE1)

# Defining fMRI, MRI measures 
fmri <- subset(dat, select = c(Visual_Global_Efficiency, Somatomotor_Global_Efficiency, Dorsal_Attention_Global_Efficiency, Ventral_Attention_Global_Efficiency, Limbic_Global_Efficiency, Frontoparietal_Global_Efficiency, Default_Mode_Global_Efficiency)) #subset all fMRI measures into a new dataset

mri <- subset(dat, select = c(Left.Amygdala, Right.Amygdala, Left.Caudate, Right.Caudate, Left.Accumbens.area, Right.Accumbens.area, TotalGrayVol, CortexVol, Left.Hippocampus, Right.Hippocampus, WM.hypointensities, non.WM.hypointensities, Left.Putamen, Right.Putamen, Left.Pallidum, Right.Pallidum)) 
#subset all MRI measures into a new dataset#Taking the average values provide a more accurate description for the MRI and fMRI measures.
```

Overall, most of our data is somewhat skewed and since our dataset is small, this may affect our analysis and conclusions. But we will continue on with the analysis and be sure to tag this issue.

# Exploratory Analysis
## Scatterplot Matricies
```{r}
#Do by next time: make scatterplot matrix looking at the two different outcomes (VWM_G- verbal working memory, SWM_G- spatial working memory)

library(car)
library(corrplot)

par(mfrow=c(2,2))
plot(dat[,c(13,14, 1:6)])
plot(dat[,c(13,14, 7:12)])
plot(dat[,c(13,14, 15:18)])
plot(dat[,c(13,14, 19:24)])
plot(dat[,c(13,14, 25:30)])
plot(dat[,c(13,14, 31:36)])
plot(dat[,c(13,14, 37:42)])

par(mfrow=c(2,2))
plot(SWM_G ~ VWM_G, data = dat)
plot(VWM_G ~ SWM_G, data = dat)
plot(SWM_G ~ RI_G, data = dat)
plot(VWM_G ~ RI_G, data = dat)
```

## Random Forest 
```{r}
library(randomForestSRC)
verbal <- rfsrc(VWM_G ~ . , data = dat, importance = TRUE)
verbal_df <- verbal$importance

spatial <- rfsrc(SWM_G ~ . , data = dat,importance = TRUE)
spatial_df <- spatial$importance

df <- data.frame(verbal_df, spatial_df)
v_name <- "Sig VWM_G Predictors"
s_name <- "Sig SWM_G Predictors "
names(df) <- c(v_name, s_name)

df$`Sig VWM_G Predictors` <- factor(df$`Sig VWM_G Predictors`)
df$`Sig SWM_G Predictors ` <- factor(df$`Sig SWM_G Predictors `)

#signficant predictors for both variables
df <- df[order(as.character(df$`Sig SWM_G Predictors `), as.character(df$`Sig VWM_G Predictors`), decreasing = TRUE), ]
head(df, n = 10)

```

# Question 1: Predicting Working Memory
## Multivariate Model 
```{r}
#We fit the model with all fMRI measures, MRI measures, demographic covariates, and the interaction effects between the covariates.
model <- lm(cbind(VWM_G, SWM_G) ~ Visual_Global_Efficiency+Somatomotor_Global_Efficiency+Dorsal_Attention_Global_Efficiency+Ventral_Attention_Global_Efficiency+Limbic_Global_Efficiency+Frontoparietal_Global_Efficiency+Default_Mode_Global_Efficiency+Left.Amygdala+Right.Amygdala+Left.Caudate+Right.Caudate+Left.Accumbens.area+Right.Accumbens.area+TotalGrayVol+CortexVol+CorticalWhiteMatterVol+Left.Putamen+Right.Putamen+Left.Pallidum+Right.Pallidum+Left.Hippocampus+Right.Hippocampus+WM.hypointensities+non.WM.hypointensities+AGE+GENDER+ETHNICITY+SCHOOL_YRS+AGE*GENDER+AGE*ETHNICITY+AGE*SCHOOL_YRS+GENDER*ETHNICITY+GENDER*SCHOOL_YRS+ETHNICITY*SCHOOL_YRS , data=dat)
summary(model)

#Use Type II multiple ANOVA test to see which predictors are jointly significant/contribute to the model.
anova(model)

## Create a new model with only the jointly significant predictors according to the results of the Type II multiple ANOVA test.
model2 <- lm(cbind(VWM_G, SWM_G) ~ Limbic_Global_Efficiency+Default_Mode_Global_Efficiency+Right.Pallidum+AGE+ETHNICITY+SCHOOL_YRS ,data=dat)
summary(model2) #We can thus decide to follow through with our updated model with only significant predictors. 
anova(model, model2) #We notice that the p-value of 0.2144 is evidence that our new model fits just as well as our previous model.
```
Since VMW_G and SWM_G are highly correlated to each other, we wanted to combine both variables into one single response variable: average working memory score. 

## Linear model with combined working memory measure
```{r}
#Creating new respsonse variable: Working Memory
attach(dat)
MRI_tot <- Left.Amygdala + Right.Amygdala + Left.Caudate	+ Right.Caudate	+ Left.Accumbens.area + Right.Accumbens.area + TotalGrayVol + CortexVol + CorticalWhiteMatterVol + Left.Putamen + Right.Putamen + Left.Pallidum + Right.Pallidum + Left.Hippocampus + Right.Hippocampus + WM.hypointensities + non.WM.hypointensities

fMRI_tot <- Visual_Global_Efficiency + Somatomotor_Global_Efficiency + Dorsal_Attention_Global_Efficiency + Ventral_Attention_Global_Efficiency + Limbic_Global_Efficiency + Frontoparietal_Global_Efficiency + Default_Mode_Global_Efficiency

fMRI_avg <- (Visual_Global_Efficiency + Somatomotor_Global_Efficiency + Dorsal_Attention_Global_Efficiency + Ventral_Attention_Global_Efficiency + Limbic_Global_Efficiency + Frontoparietal_Global_Efficiency + Default_Mode_Global_Efficiency) / 7 

MRI_avg <- (Left.Amygdala + Right.Amygdala + Left.Caudate	+ Right.Caudate	+ Left.Accumbens.area + Right.Accumbens.area + TotalGrayVol + CortexVol + CorticalWhiteMatterVol + Left.Putamen + Right.Putamen + Left.Pallidum + Right.Pallidum + Left.Hippocampus + Right.Hippocampus + WM.hypointensities + non.WM.hypointensities) / 17

WorkingMem <- 0.5*(SWM_G + VWM_G)
detach(dat)

#Full model
model3 <- lm(WorkingMem ~
Visual_Global_Efficiency+Somatomotor_Global_Efficiency+Dorsal_Attention_Global_Efficiency+Ventral_Attention_Global_Efficiency+Limbic_Global_Efficiency+Frontoparietal_Global_Efficiency+Default_Mode_Global_Efficiency+Left.Amygdala+Right.Amygdala+Left.Caudate+Right.Caudate+Left.Accumbens.area+Right.Accumbens.area+TotalGrayVol+CortexVol+CorticalWhiteMatterVol+Left.Putamen+Right.Putamen+Left.Pallidum+Right.Pallidum+Left.Hippocampus+Right.Hippocampus+WM.hypointensities+non.WM.hypointensities+AGE+GENDER+ETHNICITY+SCHOOL_YRS+AGE*GENDER+AGE*ETHNICITY+AGE*SCHOOL_YRS+GENDER*ETHNICITY+GENDER*SCHOOL_YRS+ETHNICITY*SCHOOL_YRS, data = dat)
summary(model3)  #sig: Limbic_Global_Efficiency
anova(model3) #Type II multiple ANOVA test shows us that predictors (Limbic_Global_Efficiency, Right.Amygdala, Left.Putamen, WM.hypointensities, non.WM.hypointensities, AGE, GENDER, SCHOOL_YRS) are jointly significant/contribute to the model.
vif(model3) #identified and took out the variables that were >5 and kept the ones that were sig in the anova(model) (AGE+GENDER+SCHOOL_YRS)

## New reduced model with combined VMW_G, SWM_G measure (workingMem)
final_model <- lm(WorkingMem ~ 
Limbic_Global_Efficiency + Right.Amygdala+Left.Putamen + non.WM.hypointensities + AGE + ETHNICITY + SCHOOL_YRS, data =dat)
summary(final_model) # sig: Limbic_Global_Efficiency, Right.Amygdala, AGE, ETHNICITY, SCHOOL_YRS
anova(model3, final_model, test = "Chisq") #P-value of 0.3394 indicates we should keep our null hypothesis that the models do not differ significantly. Because of this, we keep our simpler, reduced model.

par(mfrow=c(2,2))
plot(fitted(model), residuals(model), main = "Multivariate Full Model")
plot(fitted(model2),residuals(model2), main = "Multivariate Reduced Model")
plot(fitted(model3), residuals(model3), main = "Linear Regression Full Model")
plot(fitted(final_model), residuals(final_model), main= "Final Model")
```

# Question 2: Working Memory, Cognitive Ability, MRI, fMRI Behavior ~ Age
```{r}
#Checking distribution of new variables
hist(WorkingMem) #Left skewed unimodal centered around 0
par(mfrow=c(1,2))
hist(fMRI_avg) #Right skewed unimodal centered around 0.48
hist(MRI_avg) #Right skewed unimodal centered around -0.05
```

```{R}
#Given a small dataset, our new variables reflect the same skewness as the original data. 
cog <- subset(dat, select = c(VWM_G, SWM_G))
cogmri <- as.data.frame(c(cog, mri)) #add cognitive measures to MRI measures
cogfmri <- as.data.frame(c(cog, fmri)) #add cognitive measures to fMRI measures

corrplot(round(cor(cogmri[,unlist(lapply(cogmri, is.numeric))], use="pairwise.complete.obs"),3))
corrplot(round(cor(cogfmri[,unlist(lapply(cogfmri, is.numeric))], use="pairwise.complete.obs"),3))
```

```{r}
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
g1 <- ggplot(dat, aes(AGE, fMRI_avg, color=WorkingMem)) + geom_point() + geom_smooth(method = 'lm') #There is not too much change on fMRI measures as a person age, with slight increases in variation between the ages of 20-30 and 40-50.
g2 <- ggplot(dat, aes(AGE, MRI_avg, color=WorkingMem)) + geom_point() + geom_smooth(method = 'lm') #With MRI, there is an observable decrease in the measures that contribute to it. At around age 33, the MRI measure falls below 0 and continues to decrease as a person ages. Similar to fMRI, there are slight increases in variation between the ages of 20-30 and 40-50.
ggarrange(g1, g2,
          common.legend = TRUE, legend = "bottom",
          labels = c("fMRI ~ Age", "MRI ~ Age"),
          hjust = - 0.8,
          ncol = 2, nrow = 1)


p1 <- ggplot(dat, aes(AGE, WorkingMem, color=fMRI_avg)) + geom_point() + geom_smooth(method = 'lm') #It is clear from the data that issues with working memory are exaggerated with age as the average verbal and spatial working memory dips below 0 at age 42.
p2 <- ggplot(dat, aes(AGE, VWM_G)) + geom_point() + geom_smooth(method = 'lm')
p3 <- ggplot(dat, aes(AGE, SWM_G)) + geom_point() + geom_smooth(method = 'lm')

ggarrange(
  p1,               
  ggarrange(p2, p3, ncol = 2, labels = c("VMW_G", "SWM_G"), font.label = list(size = 12, color = "blue"), hjust = -2.5, vjust = 1), 
  nrow = 2, 
  labels = "Working Memory", font.label = list(color = "blue", vjust = 2)) 

g3 <- ggplot(dat, aes(WorkingMem, fMRI_avg, color=AGE)) + geom_point() + geom_smooth(method = 'lm')
g4 <- ggplot(dat, aes(WorkingMem, MRI_avg, color=AGE)) + geom_point() + geom_smooth(method = 'lm')
g5 <- ggplot(dat, aes(fMRI_avg, WorkingMem, color=AGE)) + geom_point() + geom_smooth(method = 'lm')
g6 <- ggplot(dat, aes(MRI_avg, WorkingMem, color=AGE)) + geom_point() + geom_smooth(method = 'lm')
ggarrange(g3, g4, g5, g6,
          labels = c("A", "B", "C", "D"),
          common.legend = TRUE, legend = "bottom")

ggplot(dat, aes(AGE, WorkingMem, color=DX)) + geom_point() + geom_smooth(method = 'lm', lwd=1.5)

ggarrange(
  ggplot(dat, aes(VWM_G, fMRI_avg, color=AGE)) + geom_point() + geom_smooth(method = 'lm'),
  ggplot(dat, aes(VWM_G, MRI_avg, color=AGE)) + geom_point() + geom_smooth(method = 'lm'),
  ggplot(dat, aes(SWM_G, fMRI_avg, color=AGE)) + geom_point() + geom_smooth(method = 'lm'),
  ggplot(dat, aes(SWM_G, MRI_avg, color=AGE)) + geom_point() + geom_smooth(method = 'lm'),
  labels = c("1", "2", "3", "4"),
  common.legend = TRUE,
  legend = "top")
```

# Question 3: Cognitive Ability ~ Psychiatric Diagnosis 
```{r}
interaction.plot(dat$AGE1, dat$DX, WorkingMem, trace.label = "Diagnosis", xlab = "AGE", ylab = "Working Memory", col = c("darkgreen", "red", "blue", "purple"), main = "Interaction plot of Working Memory and Diagnosis over Age")
#Here, we see a general decrease in working memory as age increases. There are not many intersection points, which suggests minimal interaction between working memory and diagnosis. Those diagnosed with schizophrenia, however, start off with much lower working memory than those not diagnosed with schizophrenia. Those diagnosed with schizophrenia see less of a decrease in working memory as they age than those in the other groups, although this could be because they start off with much lower working memory to begin with.

interaction.plot(dat$AGE1, dat$DX, dat$SWM_G, trace.label = "Diagnosis", xlab = "AGE", ylab = "Spatial Working Memory", col = c("darkgreen", "red", "blue", "purple"), main = "Interaction plot of Spatial Working Memory and Diagnosis over Age")
interaction.plot(dat$AGE1, dat$DX, dat$VWM_G, trace.label = "Diagnosis", xlab = "AGE", ylab = "Verbal Working Memory", col = c("darkgreen", "red", "blue", "purple"), main = "Interaction plot of Verbal Working Memory and Diagnosis over Age")
#When looking at Verbal and Spatial Working memory separately, we see similar behavior as with the combined measure. This makes sense, as we have found the two to be highly correlated, which is why we combined them.

interaction.plot(dat$AGE1, dat$DX, MRI_avg, trace.label = "Diagnosis", xlab = "AGE", ylab = "MRI", col = c("darkgreen", "red", "blue", "purple"), main = "Interaction plot of MRI and Diagnosis over Age")
interaction.plot(dat$AGE1, dat$DX, fMRI_avg, trace.label = "Diagnosis", xlab = "AGE", ylab = "fMRI", col = c("darkgreen", "red", "blue", "purple"), main = "Interaction plot of fMRI and Diagnosis over Age")
```
