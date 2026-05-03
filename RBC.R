##Fájl betöltése##
library(readxl)
file_path <- "c:\\Users\\Zsanibaba\\Desktop\\Mester\\4. félév\\Diplomamunka III\\Mérési adatok\\Linearitás mérési adatok.xlsx"
rbc_data <- read_excel(file_path, sheet = "RBC")
wbc_data <- read_excel(file_path, sheet = "WBC")
sec_data <- read_excel(file_path, sheet = "SEC")

##Ellenőrzés##
head(rbc_data)
str(rbc_data)

data_list <- list(
  RBC = rbc_data,
  WBC = wbc_data,
  SEC = sec_data)
library(mcr)
?mcreg

rbc_data$manual_mean <- rowMeans(rbc_data[, c("Manual 1", "Manual 2")])
rbc_data$manual_sd <- apply(rbc_data[, c("Manual 1", "Manual 2")], 1, sd) ###nincs rá szükség végül

rbc_data$measurement_mean <- rowMeans(rbc_data[, c("Rep1", "Rep2", "Rep3", "Rep4", "Rep5", "Rep6", "Rep7","Rep8", "Rep9","Rep10")])
rbc_data$measurement_sd <- apply(rbc_data[, c("Rep1", "Rep2", "Rep3", "Rep4", "Rep5", "Rep6", "Rep7","Rep8", "Rep9","Rep10")], 1, sd)

#####OLS#######
#RBC#

rbc_ols <-mcreg(
  x = rbc_data$manual_mean,
  y = rbc_data$measurement_mean,
  alpha = 0.05,
  method.reg = "LinReg",
  method.ci = "bootstrap",
  method.bootstrap.ci = "quantile",
  slope.measure = "radian",
  )
summary(rbc_ols)
#grafikus 1. 
plot(rbc_ols, main='RBC OLS')
#grafikus 2.
coef_ols <- rbc_ols@para
intercept <- coef_ols[1,1]
slope <- coef_ols[2,1]

r2 <- cor(rbc_data$manual_mean,
          rbc_data$measurement_mean,
          use = "complete.obs")^2

plot(rbc_data$manual_mean,
     rbc_data$measurement_mean,
     main = "RBC Linearitás OLS (mcr LinReg)",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept, b = slope, col = "red", lwd = 2)

eq <- paste0("y = ",
             round(slope, 3), "x + ",
             round(intercept, 3),
             "\nR² = ", round(r2, 3))

legend("topleft", legend = eq, bty = "n")

####WLS mcr##

rbc_wls <-mcreg(
  x = rbc_data$manual_mean,
  y = rbc_data$measurement_mean,
  error.ratio = 1,
  alpha = 0.05,
  method.reg = "WLinReg",
  method.ci = "bootstrap",
  method.bootstrap.ci = "quantile",
  slope.measure = "radian",
  )

summary(rbc_wls)
# grafikus 1. 
plot(rbc_wls, main='RBC WLS')
#grafikus 2.
coef_wls <- rbc_wls@para
intercept_wls <- coef_wls[1,1]
slope_wls <- coef_wls[2,1]

r2 <- cor(rbc_data$manual_mean, rbc_data$measurement_mean)^2
eq_wls <- paste0("WLS: y = ", round(slope_wls, 3), "x + ", round(intercept_wls, 3),
                 "\nR² = ", round(r2, 3))

plot(rbc_data$manual_mean,
     rbc_data$measurement_mean,
     main = "RBC Linearitás WLS",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept_wls, b = slope_wls, col = "red", lwd = 2)
text(
  x = min(rbc_data$manual_mean) + 0.3 * diff(range(rbc_data$manual_mean)),
  y = max(rbc_data$measurement_mean) - 0.1 * diff(range(rbc_data$measurement_mean)),
  labels = eq_wls,
  adj = c(0, 1),
  col = "black"
)

##############Összehasonlítás###########
# OLS paraméterek
coef_ols <- rbc_ols@para
int_ols <- coef_ols[1,1]
slope_ols <- coef_ols[2,1]

# WLS paraméterek
coef_wls <- rbc_wls@para
int_wls <- coef_wls[1,1]
slope_wls <- coef_wls[2,1]

plot(rbc_data$manual_mean,
     rbc_data$measurement_mean,
     main = "RBC Linearitás OLS vs WLS (mcr)",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = int_ols, b = slope_ols, col = "blue", lwd = 2)
abline(a = int_wls, b = slope_wls, col = "red", lwd = 2)

legend("bottomright",
       legend = c("OLS (mcreg)", "WLS"),
       col = c("blue", "red"),
       lwd = 2,
       bty = "n")

eq_ols <- paste0("OLS: y = ",
                 round(slope_ols, 3), "x + ",
                 round(int_ols, 3))

eq_wls <- paste0("WLS: y = ",
                 round(slope_wls, 3), "x + ",
                 round(int_wls, 3))
r2 <- cor(rbc_data$manual_mean,
          rbc_data$measurement_mean)^2
eq_full <- paste0(eq_ols, "\n", eq_wls, "\nR² = ", round(r2, 3))

text(
  x = min(rbc_data$manual_mean) + 0.3 * diff(range(rbc_data$manual_mean)), 
  y = max(rbc_data$measurement_mean) - 0.1 * diff(range(rbc_data$measurement_mean)), 
  labels = eq_full,
  adj = c(0, 1),
  col = "black")

compareFit(rbc_ols, rbc_wls)

########################Deming########################################################################
lambda <- mean(rbc_data$measurement_sd^2, na.rm = TRUE) / mean(rbc_data$manual_sd^2, na.rm = TRUE)

## mcr Deming###
rbc_deming<- mcreg(
  x = rbc_data$manual_mean,
  y = rbc_data$measurement_mean,
  error.ratio = lambda,
  alpha = 0.05,
  mref.name = "Manual",
  mtest.name = "Measurement",
  method.reg = "Deming",
  method.ci = "bootstrap",
  slope.measure = "radian",
  )
summary(rbc_deming)

###Grafikus 1.##
plot(rbc_deming, main='RBC Deming')

##Grafikus 2.###
coef_deming <- rbc_deming@para
intercept_deming <- coef_deming[1,1]
slope_deming <- coef_deming[2,1]
r2 <- cor(rbc_data$manual_mean, rbc_data$measurement_mean)^2
eq_deming <- paste0("Deming: y = ", round(slope_deming, 3), "x + ", round(intercept_deming, 3),
                    "\nR² = ", round(r2, 3))

plot(rbc_data$manual_mean,
     rbc_data$measurement_mean,
     main = "RBC Linearitás Deming",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept_deming, b = slope_deming, col = "red", lwd = 2)
text(
  x = min(rbc_data$manual_mean) + 0.3 * diff(range(rbc_data$manual_mean)),
  y = max(rbc_data$measurement_mean) - 0.1 * diff(range(rbc_data$measurement_mean)),
  labels = eq_deming,
  adj = c(0, 1),
  col = "black"
)

#####Súlyozott Deming####
rbc_wdeming<- mcreg(
  x = rbc_data$manual_mean,
  y = rbc_data$measurement_mean,
  error.ratio = lambda,
  alpha = 0.05,
  mref.name = "Manual",
  mtest.name = "Measurement",
  method.reg = "WDeming",
  method.ci = "bootstrap",
  slope.measure = "radian",
  )
summary(rbc_wdeming)

#Grafikus 1. 
plot(rbc_wdeming, main='RBC WDeming')

##Grafikus 2. 
coef_wdeming <- rbc_wdeming@para
intercept_wdeming <- coef_wdeming[1,1]
slope_wdeming <- coef_wdeming[2,1]
r2 <- cor(rbc_data$manual_mean, rbc_data$measurement_mean)^2
eq_wdeming <- paste0("WDeming: y = ", round(slope_wdeming, 3), "x + ", round(intercept_wdeming, 3),
                     "\nR² = ", round(r2, 3))

plot(rbc_data$manual_mean,
     rbc_data$measurement_mean,
     main = "RBC Linearitás WDeming",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept_wdeming, b = slope_wdeming, col = "red", lwd = 2)
text(
  x = min(rbc_data$manual_mean) + 0.3 * diff(range(rbc_data$manual_mean)),
  y = max(rbc_data$measurement_mean) - 0.1 * diff(range(rbc_data$measurement_mean)),
  labels = eq_wdeming,
  adj = c(0, 1),
  col = "black"
)

#MethComp Deming#
library(MethComp)
?MethComp

rbc_MC_deming<-Deming(
  x=rbc_data$manual_mean,
  y=rbc_data$measurement_mean,
  vr = lambda,  #error ratio
  boot = 1000,
  keep.boot = TRUE,
  alpha = 0.05
)
summary(rbc_MC_deming)


#grafikus 1. 

plot(
  rbc_MC_deming,
  main = "RBC Deming Regression (MethComp)",
  xlab = "Manual",
  ylab = "Measurement",
)

#grafikus 2.

intercept_MC_deming <- -9.095911
slope_MC_deming<- 1.042811

plot(
  rbc_data$manual_mean,
  rbc_data$measurement_mean,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "RBC Deming Regression (MethComp)"
)

abline(a = 0, b = 1, col = "grey", lwd = 2, lty = 2) #1:1 identity line, ha egyezne teljesen a két módszer

abline(a = intercept_MC_deming, b = slope_MC_deming,       col = "red", lwd = 2)

##### Deming regresszió SimplyAgree ###########
library(SimplyAgree)

df_rbc <- data.frame(
  Manual = rbc_data$manual_mean,
  Measurement = rbc_data$measurement_mean)

rbc_deming_SA <- dem_reg(
  Measurement ~ Manual,
  data = df_rbc,     
  error.ratio = lambda,   
  conf.level = 0.95,     
  weighted = FALSE     
)

summary(rbc_deming_SA)
# Grafikus 1. 
plot(rbc_deming_SA)


#Grafikus 2.
intercept_SA <- rbc_deming_SA$coefficients["(Intercept)"]
slope_SA     <- rbc_deming_SA$coefficients["Manual"]
r2_SA <- cor(df_rbc$Manual, df_rbc$Measurement)^2

eq_SA <- paste0(  "SimplyAgree Deming: y = ",
                  round(slope_SA, 3), "x + ", round(intercept_SA, 3),
                  "\nR² = ", round(r2_SA, 3)
)

plot(
  df_rbc$Manual, df_rbc$Measurement,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "RBC Deming regresszió (SimplyAgree)"
)

abline(a = intercept_SA, b = slope_SA, col = "red", lwd = 2)

text(
  x = min(df_rbc$Manual) + 0.30 * diff(range(df_rbc$Manual)),
  y = max(df_rbc$Measurement) - 0.10 * diff(range(df_rbc$Measurement)),
  labels = eq_SA,
  adj = c(0, 1),
  col = "black"
)


weights = 1 / (rbc_data$manual_sd^2 + rbc_data$measurement_sd^2)     ##„weighted OLS vagy weighted functional Deming hibrid”, nem variancia, hanem nagyobb szórás kisebb súlyt kap, kis szórású nagyobbat.

rbc_wdem_SA <-dem_reg(
  Measurement ~ Manual,
  data = df_rbc,
  weighted = TRUE,
  weights = weights
)
summary(rbc_wdem_SA)
## Grafikus 1.
plot(rbc_wdem_SA)

##Grafikus 2.
intercept_wdem_SA <- rbc_wdem_SA$coefficients["(Intercept)"]
slope_wdem_SA     <- rbc_wdem_SA$coefficients["Manual"]
r2_wdem_SA <- cor(df_rbc$Manual, df_rbc$Measurement)^2

eq_wdem_SA <- paste0(  "SA Weighted Deming: y = ",
                       round(slope_wdem_SA, 3), "x + ", round(intercept_wdem_SA, 3),
                       "\nR² = ", round(r2_wdem_SA, 3)
)

plot(
  df_rbc$Manual, df_rbc$Measurement,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "RBC Weighted Deming regresszió (SimplyAgree)"
)

abline(a = intercept_SA, b = slope_SA, col = "red", lwd = 2)

text(
  x = min(df_rbc$Manual) + 0.30 * diff(range(df_rbc$Manual)),
  y = max(df_rbc$Measurement) - 0.10 * diff(range(df_rbc$Measurement)),
  labels = eq_wdem_SA,
  adj = c(0, 1),
  col = "black"
)

##### Passing Bablok mcr####

rbc_paba_mcr <- mcreg(
  x = rbc_data$manual_mean,
  y = rbc_data$measurement_mean,
  error.ratio = 1,             
  method.reg = "PaBa",
  method.ci = "bootstrap",
  mref.name = "Manual",
  mtest.name = "Measurement",
  alpha = 0.05
)
summary(rbc_paba_mcr)

#Grafikus 1. 
plot(rbc_paba_mcr)

#Grafikus 2. 
coef_paba <- rbc_paba_mcr@para
intercept_paba <- coef_paba[1,1]
slope_paba <- coef_paba[2,1]
r2 <- cor(rbc_data$manual_mean, rbc_data$measurement_mean)^2
eq_paba <- paste0("Passing Bablok: y = ", round(slope_paba, 3), "x + ", round(intercept_paba, 3),
                  "\nR² = ", round(r2, 3))

plot(rbc_data$manual_mean,
     rbc_data$measurement_mean,
     main = "RBC Passing-Bablok (mcr)",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept_paba, b = slope_paba, col = "red", lwd = 2)
text(
  x = min(rbc_data$manual_mean) + 0.3 * diff(range(rbc_data$manual_mean)),
  y = max(rbc_data$measurement_mean) - 0.1 * diff(range(rbc_data$measurement_mean)),
  labels = eq_paba,
  adj = c(0, 1),
  col = "black"
)

#####Methcomp PAssing-Bablok######
library(MethComp)

rbc_paba_MC <- PBreg(
  x = rbc_data$manual_mean,
  y = rbc_data$measurement_mean,
  conf.level = 0.05       
)
summary(rbc_paba_MC)

rbc_paba_MC$coefficients

#Grafikus 1
plot(rbc_paba_MC, main='RBC Passing-Bablok (MethComp)')

#Grafikus 2

r2 <- cor(rbc_data$manual_mean, rbc_data$measurement_mean)^2

eq_paba <- paste0(  "Passing-Bablok: y = ",  round(slope_paba, 3), "x + ",  round(intercept_paba, 3),
                    "\nR² = ", round(r2, 3))

plot(
  rbc_data$manual_mean,
  rbc_data$measurement_mean,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "RBC Passing–Bablok Regression (MethComp)"
)

abline(a = intercept_paba, b = slope_paba, col = "red", lwd = 2)

text(
  x = min(rbc_data$`Manual Average`) + 0.3 * diff(range(rbc_data$`Manual Average`)),
  y = max(rbc_data$measurement_mean) - 0.1 * diff(range(rbc_data$measurement_mean)),
  labels = eq_paba,
  adj = c(0, 1),
  col = "black"
)

#####mcr Bland Altman###### 

BA_mcr<-MCResult.plotDifference(
  rbc_deming,
  xlab = "Átlag (Manual és Measurement)",
  ylab = "Különbség (Measurement - Manual)",
  ref.line.col = "black",
  bias.line.col = "red",    
  loa.line.col = "blue",    
  plot.type = 3,            # klasszikus BA diagram
  main = "RBC – Bland–Altman Plot (mcr)",
  cex = 1.0,
  )

rbc_mean   <- (rbc_data$manual_mean + rbc_data$measurement_mean) / 2
rbc_diff   <- rbc_data$measurement_mean - rbc_data$manual_mean

# 2. Bias és LOA-k
bias_rbc   <- mean(rbc_diff, na.rm = TRUE)
sd_rbc     <- sd(rbc_diff, na.rm = TRUE)

loa_upper  <- bias_rbc + 1.96 * sd_rbc
loa_lower  <- bias_rbc - 1.96 * sd_rbc

bias_rbc; sd_rbc; loa_lower; loa_upper


plot(
  rbc_mean, rbc_diff,
  pch = 16,
  xlab = "Átlag (Manual és Measurement)",
  ylab = "Különbség (Measurement – Manual)",
  main = "RBC – Bland–Altman Plot",
  cex = 1.0,
  ylim = c(loa_lower - 5, loa_upper + 5)   # <<< EZ A FONTOS RÉSZ
)


abline(h = 0,          col = "grey40", lwd = 2, lty = 2)
abline(h = bias_rbc,   col = "red",    lwd = 3)
abline(h = loa_upper,  col = "blue",   lwd = 2, lty = 2)
abline(h = loa_lower,  col = "blue",   lwd = 2, lty = 2)

legend(
  "topright",
  legend = c(
    paste0("Bias = ", round(bias_rbc, 2)),
    paste0("Upper LOA = ", round(loa_upper, 2)),
    paste0("Lower LOA = ", round(loa_lower, 2))
  ),
  bg = "white",       # háttérdoboz
  bty = "o",
  cex = 0.8,
  text.col = c("red", "blue", "blue")
)

####MethComp Bland-Altman####
library(MethComp)
manual_long <- data.frame(
  item = rep(1:nrow(rbc_data), each = 2),
  meth = "Manual",
  repl = rep(1:2, times = nrow(rbc_data)),
  y = as.vector(t(rbc_data[, c("Manual 1", "Manual 2")]))
)
measurement_long <- data.frame(
  item = rep(1:nrow(rbc_data), each = 10),
  meth = "Measurement",
  repl = rep(1:10, times = nrow(rbc_data)),
  y = as.vector(t(rbc_data[, paste0("Rep", 1:10)]))
)
rbc_long <- rbind(manual_long, measurement_long)
rbc_meth <- Meth(rbc_long)

BA.est(rbc_meth)
BA.plot(rbc_meth, graph = "mean.diff", CI = TRUE, main='RBC Bland-Altman(MC)')

###blandr Bland-Altman####
library(blandr)
library(ggplot2)

blandr.draw(
  rbc_data$manual_mean,
  rbc_data$measurement_mean,
  plotTitle = "RBC – Bland–Altman Plot (blandr)")

blandr.display.and.draw(
  method1 = rbc_data$manual_mean,
  method2 = rbc_data$measurement_mean,
  plotter = "ggplot",
  method1name = "Manual",
  method2name = "Measurement",
  plotTitle = "RBC – Bland–Altman (blandr)",
  sig.level = 0.95,
  point_size = 1.8
)
ba_rbc <- blandr.statistics(
  rbc_data$manual_mean,
  rbc_data$measurement_mean)

p <- blandr.draw(
  rbc_data$manual_mean,
  rbc_data$measurement_mean,
  plotter = "ggplot",
  ciDisplay = TRUE,
  ciShading = TRUE,
  annotate = FALSE,
  plotTitle = "RBC – Bland–Altman Plot (blandr)"
)

p +
  annotate(
    "label",
    x = max(rbc_data$manual_mean) * 0.55,
    y = ba_rbc$upperLOA,
    label = paste0(
      "Upper LOA = ", round(ba_rbc$upperLOA, 1), "\n",
      "CI: ", round(ba_rbc$upperLOA_lowerCI, 1), " – ", round(ba_rbc$upperLOA_upperCI, 1)
    ),
    fill = "white", color = "darkgreen", size = 4, label.size = 0.3
  ) +
  annotate(
    "label",
    x = max(rbc_data$manual_mean) * 0.55,
    y = ba_rbc$bias,
    label = paste0(
      "Bias = ", round(ba_rbc$bias, 1), "\n",
      "CI: ", round(ba_rbc$biasLowerCI, 1), " – ", round(ba_rbc$biasUpperCI, 1)
    ),
    fill = "white", color = "blue", size = 4, label.size = 0.3
  ) +
  annotate(
    "label",
    x = max(rbc_data$manual_mean) * 0.55, y = ba_rbc$lowerLOA, label = paste0(  "Lower LOA = ", round(ba_rbc$lowerLOA, 1), "\n",
                                                                                "CI: ", round(ba_rbc$lowerLOA_lowerCI, 1), " – ", round(ba_rbc$lowerLOA_upperCI, 1)
    ),    fill = "white", color = "red3", size = 4, label.size = 0.3  )


###blandr method comparison - Nem Bland–Altman módszer, hanem egy kiegészítő diagnosztikai blokk

rbc_blandr_metodcomparison<- blandr.method.comparison(
  method1 = rbc_data$manual_mean,
  method2 = rbc_data$measurement_mean
)

blandr.plot.ggplot(
  statistics.results = ba_rbc,
  method1name = "Manual",
  method2name = "Measurement",
  plotTitle = "RBC – Bland–Altman Plot (blandr.ggplot)",
  x.plot.mode = "means",
  y.plot.mode = "difference",
  plotProportionalBias = TRUE,
  plotProportionalBias.se = TRUE,
  assume.differences.are.normal = TRUE)
  
  limits<-blandr.plot.limits(  statistics.results = ba_rbc, lowest_y_axis = FALSE, highest_y_axis = FALSE)
  
  blandr.plot.rplot(
    statistics.results = ba_rbc,
    plot.limits=limits,
    method1name = "Manual",
    method2name = "Measurement",
    plotTitle = "Bland-Altman plot for comparison of 2 methods",
    )
  
  #####SimplyAgree#####
  ##LoA
  
  library(SimplyAgree)
  
  agreement_limit(
    x = "manual_mean",
    y = "measurement_mean",
    data = rbc_data,
    data_type = "simple",          
    loa_calc = "blandaltman",      
    )
  
  #### prop.bias-sal jobb, ismétléseket figyelembe véve: 
  library(dplyr)
  library(tidyr)
  
  rbc_manual_long <- rbc_data %>%
    select(id = 1, `Manual 1`, `Manual 2`) %>%
    pivot_longer(cols = starts_with("Manual"),
                 names_to = "manual_repl",
                 values_to = "x") %>%   # x = manual measurement
    mutate(manual_repl = as.numeric(gsub("Manual ", "", manual_repl)))
  
  rbc_meas_long <- rbc_data %>%
    select(id = 1, starts_with("Rep")) %>%
    pivot_longer(cols = starts_with("Rep"),
                 names_to = "meas_repl",
                 values_to = "y") %>%   # y = machine measurement
    mutate(meas_repl = as.numeric(gsub("Rep", "", meas_repl)))
  
  
  rbc_reps <- rbc_manual_long %>%
    left_join(rbc_meas_long, by = "id")
  
  ####proportional bias corrected LOA,
  agreement_limit(
    x = "x",
    y = "y",
    id = "id",
    data = rbc_reps,
    data_type = "reps",          
    loa_calc = "mover",          
    prop_bias = TRUE,           
    agree.level = 0.95,
    alpha = 0.05
  )
  