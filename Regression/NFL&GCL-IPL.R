
library(tidyverse)
library(dplyr)

# BMI, BUN.Cr, Adiponectin, Insulin, Leptin, age, gender
data <- read.delim("data_cleaned.txt")
dat <- data %>% select(IRB, CSF_NFL_OD, CSF_NFL_OS, age, gender, BMI, BUN.Cr, Adiponectin, Insulin, Leptin, NEFA) %>%
  pivot_longer(cols=CSF_NFL_OD:CSF_NFL_OS, names_to="CSF_NFL", values_to="Measure")

library(lmerTest)
m1 <- lmer(Measure~(1|IRB)+age+gender+BMI+BUN.Cr+Adiponectin+Insulin+Leptin+NEFA, data=dat)
summary(m1)

# Select predictors
newlist <- c("age","SBP","HbA1c","BUN.Cr","Cholesterol","Ketone","Adiponectin","Leptin","RAGE","NEFA")
met_cor <- cor(data[newlist], use = "pairwise.complete.obs")
corrplot(met_cor, type = "upper", method = "circle")

# Run regressions for NFL
preds <- c("IRB","age","gender","SBP","HbA1c","BUN.Cr","Cholesterol","Ketone","Adiponectin","Leptin","RAGE","NEFA")
cols <- c(1:22, grep("NFL", colnames(data)))
regdata <- data[,cols]

locs <- c("CSF", "SI", "SO", "NI", "NO", "II", "IO", "TI", "TO")
datal <- list()
model_NFL <- list()
CI_NFL <- list()

for (i in 1:length(locs)){
  dataw <- cbind(regdata[,preds], regdata[,grep(locs[i], colnames(regdata))])
  datal[[i]] <- pivot_longer(dataw, cols=c(tail(colnames(dataw),2)[1],tail(colnames(dataw),2)[2]),
                      names_to=paste("NFL", locs[i], sep="_"), values_to="Measure") %>% drop_na()
  library(lmerTest)
  model_NFL[[i]] <- lmer(Measure~(1|IRB)+age+gender+SBP+HbA1c+BUN.Cr+Cholesterol+Ketone+Adiponectin+Leptin+RAGE+NEFA,
                         data=datal[[i]])
  CI_NFL[[i]] <- round(confint(model_NFL[[i]]),3)
}

library(sjPlot)

# Generate Effect size plot for NFL

plot_models(title = "NFL Thickness Values Effect Size for Each Predictor By Region Adjusted by FDR",
            model_NFL[[1]], model_NFL[[2]], model_NFL[[3]],
            model_NFL[[4]], model_NFL[[5]], model_NFL[[6]],
            model_NFL[[7]], model_NFL[[8]], model_NFL[[9]],
            dot.size = 1,
            spacing = 0.5,
            show.values = TRUE,
            p.adjust = c("fdr"))

# Generate Effect size plot for NFL (Regions with statistically significant results)

plot_models(title = "NFL Thickness Values Effect Size for Each Predictor For NO (Blue) and TI (Red) Regions Adjusted by FDR",
            model_NFL[[5]],
            model_NFL[[8]],
            dot.size = 1,
            spacing = 0.5,
            show.values = TRUE,
            p.adjust = c("fdr"))



# Run regressions for GCL.IPL
cols2 <- c(1:22, grep("GCL.IPL", colnames(data)))
regdata2 <- data[,cols2]

datal2 <- list()
model_GI <- list()
CI_GI <- list()

for (i in 1:length(locs)){
  dataw <- cbind(regdata2[,preds], regdata2[,grep(locs[i], colnames(regdata2))])
  datal2[[i]] <- pivot_longer(dataw, cols=c(tail(colnames(dataw),2)[1],tail(colnames(dataw),2)[2]),
                             names_to=paste("GCL_IPL", locs[i], sep="_"), values_to="Measure") %>% drop_na()
  library(lmerTest)
  model_GI[[i]] <- lmer(Measure~(1|IRB)+age+gender+SBP+HbA1c+BUN.Cr+Cholesterol+Ketone+Adiponectin+Leptin+RAGE+NEFA,
                         data=datal2[[i]])
  CI_GI[[i]] <- round(confint(model_GI[[i]]),3)
}

# Generate Effect size plot for GCL.IPL

plot_models(title = "GCL.IPL Thickness Values Effect Size for Each Predictor By Region Adjusted by FDR (Every Interval Overlaps With Zero)",
            model_GI[[1]], model_GI[[2]], model_GI[[3]],
            model_GI[[4]], model_GI[[5]], model_GI[[6]],
            model_GI[[7]], model_GI[[8]], model_GI[[9]],
            dot.size = 1,
            spacing = 0.5,
            show.values = FALSE,
            p.adjust = c("fdr"),
            show.legend = FALSE)


# Adjust p-values
models <- c(model_GI, model_NFL)
adps <- sapply(models, function(x) coefficients(summary(x))[9,5])
leps <- sapply(models, function(x) coefficients(summary(x))[10,5])
raps <- sapply(models, function(x) coefficients(summary(x))[11,5])
adpadj <- p.adjust(adps, method="fdr")
lepadj <- p.adjust(leps, method="fdr")
rapadj <- p.adjust(raps, method="fdr")

data.frame(adp=adps, adpa=adpadj, lep=leps, lepa=lepadj, rap=raps, rapa=rapadj)


# Total thickness
data <- data %>% rename(NO_TOT_OS=NO_TOS_OS)
cols3 <- c(1:22, grep("TOT", colnames(data)))
regdata3 <- data[,cols3]

datal3 <- list()
model_tot <- list()
CI_tot <- list()

for (i in 1:length(locs)){
  dataw <- cbind(regdata3[,preds], regdata3[,grep(locs[i], colnames(regdata3))])
  datal3[[i]] <- pivot_longer(dataw, cols=c(tail(colnames(dataw),2)[1],tail(colnames(dataw),2)[2]),
                              names_to=paste("TOT", locs[i], sep="_"), values_to="Measure") %>% drop_na()
  library(lmerTest)
  model_tot[[i]] <- lmer(Measure~(1|IRB)+age+gender+SBP+HbA1c+BUN.Cr+Cholesterol+Ketone+Adiponectin+Leptin+RAGE,
                        data=datal3[[i]])
  CI_tot[[i]] <- round(confint(model_tot[[i]]),3)
}

