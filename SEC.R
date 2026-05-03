##Fájl betöltése##
library(readxl)
file_path <- "c:\\Users\\Zsanibaba\\Desktop\\Mester\\4. félév\\Diplomamunka III\\Mérési adatok\\Linearitás mérési adatok.xlsx"
rbc_data <- read_excel(file_path, sheet = "RBC")
wbc_data <- read_excel(file_path, sheet = "WBC")
sec_data <- read_excel(file_path, sheet = "SEC")

##Ellenőrzés##
head(sec_data)
str(sec_data)

data_list <- list(
  RBC = rbc_data,
  WBC = wbc_data,
  SEC = sec_data)
library(mcr)
?mcreg
#####OLS#######

sec_data$manual_mean <- rowMeans(sec_data[, c("Manual 1", "Manual 2", "Manual 3", "Manual 4")])
sec_data$manual_sd <- apply(sec_data[, c("Manual 1", "Manual 2","Manual 3","Manual 4")], 1, sd)

sec_data$measurement_mean <- rowMeans(sec_data[, c("Rep1", "Rep2", "Rep3", "Rep4", "Rep5", "Rep6", "Rep7","Rep8", "Rep9","Rep10", "Rep11", "Rep12", "Rep13", "Rep14", "Rep15")])
sec_data$measurement_sd <- apply(sec_data[, c("Rep1", "Rep2", "Rep3", "Rep4", "Rep5", "Rep6", "Rep7","Rep8", "Rep9","Rep10","Rep11", "Rep12", "Rep13", "Rep14", "Rep15")], 1, sd)
lambda <- mean((sec_data$manual_sd^2) / 4, na.rm = TRUE) /  mean((sec_data$measurement_sd^2) / 15, na.rm = TRUE)

sec_ols <-mcreg(
  x = sec_data$manual_mean,
  y = sec_data$measurement_mean,
  error.ratio = 1,
  alpha = 0.05,
  method.reg = "LinReg",
  method.ci = "bootstrap",
  method.bootstrap.ci = "quantile",
  slope.measure = "radian",
 )
summary(sec_ols)
#grafikus 1. 
plot(sec_ols, main='SEC OLS')
#grafikus 2.
coef_ols <- sec_ols@para
intercept <- coef_ols[1,1]
slope <- coef_ols[2,1]

r2 <- cor(sec_data$manual_mean,
          sec_data$measurement_mean,
          use = "complete.obs")^2

plot(sec_data$manual_mean,
     sec_data$measurement_mean,
     main = "SEC Linearitás OLS (mcr LinReg)",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept, b = slope, col = "red", lwd = 2)

eq <- paste0("y = ",
             round(slope, 3), "x + ",
             round(intercept, 3),
             "\nR² = ", round(r2, 3))

legend("topleft", legend = eq, bty = "n")

####WLS######
sec_wls <-mcreg(
  x = sec_data$manual_mean,
  y = sec_data$measurement_mean,
  error.ratio = 1,
  alpha = 0.05,
  method.reg = "WLinReg",
  method.ci = "bootstrap",
  method.bootstrap.ci = "quantile",
  slope.measure = "radian",
  )

summary(sec_wls)
# grafikus 1. 
plot(sec_wls, main='SEC WLS')
#grafikus 2.
coef_wls <- sec_wls@para
intercept_wls <- coef_wls[1,1]
slope_wls <- coef_wls[2,1]

r2 <- cor(sec_data$manual_mean, sec_data$`Measurement average`)^2
eq_wls <- paste0("WLS: y = ", round(slope_wls, 3), "x + ", round(intercept_wls, 3),
                 "\nR² = ", round(r2, 3))

plot(sec_data$manual_mean,
     sec_data$`Measurement average`,
     main = "SEC Linearitás WLS",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept_wls, b = slope_wls, col = "red", lwd = 2)
text(
  x = min(sec_data$manual_mean) + 0.3 * diff(range(sec_data$manual_mean)),
  y = max(sec_data$`Measurement average`) - 0.1 * diff(range(sec_data$measurement_mean)),
  labels = eq_wls,
  adj = c(0, 1),
  col = "black"
)

##############Összehasonlítás###########
# OLS paraméterek
coef_ols <- sec_ols@para
int_ols <- coef_ols[1,1]
slope_ols <- coef_ols[2,1]

# WLS paraméterek
coef_wls <- sec_wls@para
int_wls <- coef_wls[1,1]
slope_wls <- coef_wls[2,1]

plot(sec_data$manual_mean,
     sec_data$measurement_mean,
     main = "SEC Linearitás OLS vs WLS (mcr)",
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
r2 <- cor(sec_data$`Manual Average`,
          sec_data$`Measurement average`)^2
eq_full <- paste0(eq_ols, "\n", eq_wls, "\nR² = ", round(r2, 3))

text(
  x = min(sec_data$`Manual Average`) + 0.3 * diff(range(sec_data$manual_mean)), 
  y = max(sec_data$`Measurement average`) - 0.1 * diff(range(sec_data$measurement_mean)), 
  labels = eq_full,
  adj = c(0, 1),
  col = "black")

compareFit(sec_ols, sec_wls)
#############Deming mcr############x
sec_deming<- mcreg(
  x = sec_data$manual_mean,
  y = sec_data$measurement_mean,
  error.ratio = lambda,
  alpha = 0.05,
  mref.name = "Manual",
  mtest.name = "Measurement",
  method.reg = "Deming",
  method.ci = "bootstrap",
  slope.measure = "radian",
 )
summary(sec_deming)
##Grafikus 1. 
plot(sec_deming, main='SEC Deming')
###Grafikus 2. 
coef_deming <- sec_deming@para
intercept_deming <- coef_deming[1,1]
slope_deming <- coef_deming[2,1]
r2<- cor(sec_data$manual_mean, sec_data$measurement_mean)^2
eq_deming <- paste0("Deming: y = ", round(slope_deming, 3), "x + ", round(intercept_deming, 3),
                    "\nR² = ", round(r2, 3))

plot(sec_data$manual_mean,
     sec_data$measurement_mean,
     main = "SEC Linearitás Deming",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept_deming, b = slope_deming, col = "red", lwd = 2)
text(
  x = min(sec_data$manual_mean) + 0.3 * diff(range(sec_data$manual_mean)),
  y = max(sec_data$measurement_mean) - 0.1 * diff(range(sec_data$measurement_mean)),
  labels = eq_deming,
  adj = c(0, 1),
  col = "black"
)
#####WDeming###
sec_wdeming<- mcreg(
  x = sec_data$manual_mean,
  y = sec_data$measurement_mean,
  error.ratio = lambda,
  alpha = 0.05,
  mref.name = "Manual",
  mtest.name = "Measurement",
  method.reg = "WDeming",
  method.ci = "bootstrap",
  slope.measure = "radian",
 )
summary(sec_wdeming)

#Grafikus 1. 
plot(sec_wdeming, main='SEC WDeming')

##Grafikus 2. 
coef_wdeming <- sec_wdeming@para
intercept_wdeming <- coef_wdeming[1,1]
slope_wdeming <- coef_wdeming[2,1]
r2 <- cor(sec_data$manual_mean, sec_data$measurement_mean)^2
eq_wdeming <- paste0("WDeming: y = ", round(slope_wdeming, 3), "x + ", round(intercept_wdeming, 3),
                     "\nR² = ", round(r2, 3))

plot(sec_data$manual_mean,
     sec_data$measurement_mean,
     main = "SEC Linearitás WDeming",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept_wdeming, b = slope_wdeming, col = "red", lwd = 2)
text(
  x = min(sec_data$manual_mean) + 0.3 * diff(range(sec_data$manual_mean)),
  y = max(sec_data$measurement_mean) - 0.1 * diff(range(sec_data$measurement_mean)),
  labels = eq_wdeming,
  adj = c(0, 1),
  col = "black"
)

#MethComp Deming#
library(MethComp)
?MethComp

sec_MC_deming<-Deming(
  x=sec_data$manual_mean,
  y=sec_data$measurement_mean,
  vr = lambda,  
  boot = 1000,
  keep.boot = TRUE,
  alpha = 0.05
)
summary(sec_MC_deming)
#grafikus 1. 

plot(
  sec_MC_deming,
  main = "SEC Deming Regression (MethComp)",
  xlab = "Manual",
  ylab = "Measurement",
)

#grafikus 2.

intercept_MC_deming <- 1.4625413
slope_MC_deming<- 0.9662915

plot(
  sec_data$manual_mean,
  sec_data$measurement_mean,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "SEC Deming Regression (MethComp)"
)

abline(a = 0, b = 1, col = "grey", lwd = 2, lty = 2) #1:1 identity line, ha egyezne teljesen a két módszer

abline(a = intercept_MC_deming, b = slope_MC_deming,       col = "red", lwd = 2)

##### Deming regresszió SimplyAgree######
library(SimplyAgree)

df_sec <- data.frame(
  Manual = sec_data$manual_mean,
  Measurement = sec_data$measurement_mean)

sec_deming_SA <- dem_reg(
  Measurement ~ Manual,
  data = df_sec,     
  error.ratio = lambda,   
  conf.level = 0.95,     
  weighted = FALSE     
)

summary(sec_deming_SA)
# Grafikus 1. 
plot(sec_deming_SA)
#Grafikus 2.
intercept_SA <- sec_deming_SA$coefficients["(Intercept)"]
slope_SA     <- sec_deming_SA$coefficients["Manual"]
r2_SA <- cor(df_sec$Manual, df_sec$Measurement)^2

eq_SA <- paste0(  "SimplyAgree Deming: y = ",
                  round(slope_SA, 3), "x + ", round(intercept_SA, 3),
                  "\nR² = ", round(r2_SA, 3)
)

plot(
  df_sec$Manual, df_sec$Measurement,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "SEC Deming regresszió (SimplyAgree)"
)

abline(a = intercept_SA, b = slope_SA, col = "red", lwd = 2)

text(
  x = min(df_sec$Manual) + 0.20 * diff(range(df_sec$Manual)),
  y = max(df_sec$Measurement) - 0.10 * diff(range(df_sec$Measurement)),
  labels = eq_SA,
  adj = c(0, 1),
  col = "black"
)

###Súlyozott Deming Simply Agree spec.###
weights = 1 / (sec_data$manual_sd^2 + sec_data$measurement_sd^2)     ##„weighted OLS vagy weighted functional Deming hibrid”, nem variancia, hanem nagyobb szórás kisebb súlyt kap, kis szórású nagyobbat.

sec_wdem_SA <-dem_reg(
  Measurement ~ Manual,
  data = df_sec,
  weighted = TRUE,
  weights = weights
)
summary(sec_wdem_SA)
## Grafikus 1.
plot(sec_wdem_SA)

##Grafikus 2.
intercept_wdem_SA <- sec_wdem_SA$coefficients["(Intercept)"]
slope_wdem_SA     <- sec_wdem_SA$coefficients["Manual"]
r2_wdem_SA <- cor(df_sec$Manual, df_sec$Measurement)^2

eq_wdem_SA <- paste0(  "SA Weighted Deming: y = ",
                       round(slope_wdem_SA, 3), "x + ", round(intercept_wdem_SA, 3),
                       "\nR² = ", round(r2_wdem_SA, 3)
)

plot(
  df_sec$Manual, df_sec$Measurement,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "SEC Weighted Deming regresszió (SimplyAgree)"
)

abline(a = intercept_SA, b = slope_SA, col = "red", lwd = 2)

text(
  x = min(df_sec$Manual) + 0.20 * diff(range(df_sec$Manual)),
  y = max(df_sec$Measurement) - 0.10 * diff(range(df_sec$Measurement)),
  labels = eq_wdem_SA,
  adj = c(0, 1),
  col = "black"
)
##### Passing Bablok mcr

sec_paba_mcr <- mcreg(
  x = sec_data$manual_mean,
  y = sec_data$measurement_mean,
  error.ratio = 1,             
  method.reg = "PaBa",
  method.ci = "bootstrap",
  mref.name = "Manual",
  mtest.name = "Measurement",
  alpha = 0.05
)
summary(sec_paba_mcr)

#Grafikus 1. 
plot(sec_paba_mcr)

#Grafikus 2. 
coef_paba <- sec_paba_mcr@para
intercept_paba <- coef_paba[1,1]
slope_paba <- coef_paba[2,1]
r2 <- cor(sec_data$manual_mean, sec_data$measurement_mean)^2
eq_paba <- paste0("Passing Bablok: y = ", round(slope_paba, 3), "x + ", round(intercept_paba, 3),
                  "\nR² = ", round(r2, 3))

plot(sec_data$manual_mean,
     sec_data$measurement_mean,
     main = "SEC Passing-Bablok (mcr)",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept_paba, b = slope_paba, col = "red", lwd = 2)
text(
  x = min(sec_data$manual_mean) + 0.3 * diff(range(sec_data$manual_mean)),
  y = max(sec_data$measurement_mean) - 0.1 * diff(range(sec_data$measurement_mean)),
  labels = eq_paba,
  adj = c(0, 1),
  col = "black"
)

#####Methcomp PAssing-Bablok
library(MethComp)

sec_paba_MC <- PBreg(
  x = sec_data$manual_mean,
  y = sec_data$measurement_mean,
  conf.level = 0.05       # 95%-os CI
)
summary(sec_paba_MC)

sec_paba_MC$coefficients
#Grafikus 1.
plot(sec_paba_MC, main='SEC Passing-Bablok (MethComp)')

#Grafikus 2

eq_paba <- paste0(  "Passing-Bablok: y = ",  round(slope_paba, 3), "x + ",  round(intercept_paba, 3),
                    "\nR² = ", round(r2, 3))

plot(
  sec_data$manual_mean,
  sec_data$measurement_mean,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "SEC Passing–Bablok Regression (MethComp)"
)

abline(a = intercept_paba, b = slope_paba, col = "red", lwd = 2)

text(
  x = min(sec_data$manual_mean) + 0.3 * diff(range(sec_data$manual_mean)),
  y = max(sec_data$measurement_mean) - 0.1 * diff(range(sec_data$measurement_mean)),
  labels = eq_paba,
  adj = c(0, 1),
  col = "black"
)
#####mcr Bland Altman###### 

MCResult.plotDifference(
  sec_deming,
  xlab = "Átlag (Manual és Measurement)",
  ylab = "Különbség (Measurement - Manual)",
  ref.line.col = "black",
  bias.line.col = "red",    
  loa.line.col = "blue",    
  plot.type = 3,            # klasszikus BA diagram
  main = "SEC – Bland–Altman Plot (mcr)",
  cex = 1.0,
  )

sec_mean   <- (sec_data$manual_mean + sec_data$measurement_mean) / 2
sec_diff   <- sec_data$measurement_mean - sec_data$manual_mean

# 2. Bias és LOA-k
bias_sec   <- mean(sec_diff, na.rm = TRUE)
sd_sec     <- sd(sec_diff, na.rm = TRUE)

loa_upper  <- bias_sec + 1.96 * sd_sec
loa_lower  <- bias_sec - 1.96 * sd_sec

bias_sec; sd_sec; loa_lower; loa_upper


plot(
  sec_mean, sec_diff,
  pch = 16,
  xlab = "Átlag (Manual és Measurement)",
  ylab = "Különbség (Measurement – Manual)",
  main = "SEC – Bland–Altman Plot (mcr)",
  cex = 1.0,
  ylim = c(loa_lower - 5, loa_upper + 5)   # <<< EZ A FONTOS RÉSZ
)


abline(h = 0,          col = "grey40", lwd = 2, lty = 2)
abline(h = bias_sec,   col = "red",    lwd = 3)
abline(h = loa_upper,  col = "blue",   lwd = 2, lty = 2)
abline(h = loa_lower,  col = "blue",   lwd = 2, lty = 2)

legend(
  "topright",
  legend = c(
    paste0("Bias = ", round(bias_sec, 2)),
    paste0("Upper LOA = ", round(loa_upper, 2)),
    paste0("Lower LOA = ", round(loa_lower, 2))
  ),
  bg = "white",       # háttérdoboz
  bty = "o",
  cex = 0.8,
  text.col = c("red", "blue", "blue")
)
########MethComp Bland-Altman#####
library(MethComp)
manual_long <- data.frame(
  item = rep(1:nrow(sec_data), each = 2),
  meth = "Manual",
  repl = rep(1:2, times = nrow(sec_data)),
  y = as.vector(t(sec_data[, c("Manual 1", "Manual 2")]))
)
measurement_long <- data.frame(
  item = rep(1:nrow(sec_data), each = 15),
  meth = "Measurement",
  repl = rep(1:15, times = nrow(sec_data)),
  y = as.vector(t(sec_data[, paste0("Rep", 1:15)]))
)
sec_long <- rbind(manual_long, measurement_long)
sec_meth <- Meth(sec_long)

BA.est(sec_meth)
BA.plot(sec_meth, graph = "mean.diff", CI = TRUE, main='SEC Bland-Altman(MC)')



###blandr Bland-Altman####

library(blandr)
library(ggplot2)

blandr.draw(
  sec_data$manual_mean,
  sec_data$measurement_mean,
  plotTitle = "SEC – Bland–Altman Plot (blandr)",
)

blandr.display.and.draw(
  method1 = sec_data$manual_mean,
  method2 = sec_data$measurement_mean,
  plotter = "ggplot",
  method1name = "Manual",
  method2name = "Measurement",
  plotTitle = "SEC – Bland–Altman (blandr)",
  sig.level = 0.95,
  point_size = 1.8
)

ba_sec <- blandr.statistics(
  sec_data$manual_mean,
  sec_data$measurement_mean)

p <- blandr.draw(
  sec_data$manual_mean,
  sec_data$measurement_mean,
  plotter = "ggplot",
  ciDisplay = TRUE,
  ciShading = TRUE,
  annotate = FALSE,
  plotTitle = "SEC – Bland–Altman Plot (blandr)"
)

p +
  annotate(
    "label",
    x = max(sec_data$manual_mean) * 0.55,
    y = ba_sec$upperLOA,
    label = paste0(
      "Upper LOA = ", round(ba_sec$upperLOA, 1), "\n",
      "CI: ", round(ba_sec$upperLOA_lowerCI, 1), " – ", round(ba_sec$upperLOA_upperCI, 1)
    ),
    fill = "white", color = "darkgreen", size = 4, label.size = 0.3
  ) +
  annotate(
    "label",
    x = max(sec_data$manual_mean) * 0.55,
    y = ba_sec$bias,
    label = paste0(
      "Bias = ", round(ba_sec$bias, 1), "\n",
      "CI: ", round(ba_sec$biasLowerCI, 1), " – ", round(ba_sec$biasUpperCI, 1)
    ),
    fill = "white", color = "blue", size = 4, label.size = 0.3
  ) +
  annotate(
    "label",
    x = max(sec_data$manual_mean) * 0.55, y = ba_sec$lowerLOA, label = paste0(  "Lower LOA = ", round(ba_sec$lowerLOA, 1), "\n",
                                                                                "CI: ", round(ba_sec$lowerLOA_lowerCI, 1), " – ", round(ba_sec$lowerLOA_upperCI, 1)
    ),    fill = "white", color = "red3", size = 4, label.size = 0.3  )

###blandr method comparison - Nem Bland–Altman módszer, hanem egy kiegészítő diagnosztikai blokk

sec_blandr_metodcomparison<- blandr.method.comparison(
  method1 = sec_data$manual_mean,
  method2 = sec_data$measurement_mean
)

blandr.plot.ggplot(
  statistics.results = ba_sec,
  method1name = "Manual",
  method2name = "Measurement",
  plotTitle = "SEC – Bland–Altman Plot (blandr.ggplot)",
  x.plot.mode = "means",
  y.plot.mode = "difference",
  plotProportionalBias = TRUE,
  plotProportionalBias.se = TRUE,
  assume.differences.are.normal = TRUE)

limits<-blandr.plot.limits(statistics.results = ba_sec, lowest_y_axis = FALSE, highest_y_axis = FALSE)

blandr.plot.rplot(
  statistics.results = ba_sec,
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
  data = sec_data,
  data_type = "simple",          
  loa_calc = "blandaltman",      
  )
