##Fájl betöltése##
library(readxl)
file_path <- "c:\\Users\\Zsanibaba\\Desktop\\Mester\\4. félév\\Diplomamunka III\\Mérési adatok\\Linearitás mérési adatok.xlsx"
rbc_data <- read_excel(file_path, sheet = "RBC")
wbc_data <- read_excel(file_path, sheet = "WBC")
sec_data <- read_excel(file_path, sheet = "SEC")

##Ellenőrzés##
head(wbc_data)
str(wbc_data)

data_list <- list(
  RBC = rbc_data,
  WBC = wbc_data,
  SEC = sec_data)
library(mcr)
?mcreg

wbc_data$manual_mean <- rowMeans(wbc_data[, c("Manual 1", "Manual 2")])
wbc_data$manual_sd <- apply(wbc_data[, c("Manual 1", "Manual 2")], 1, sd)
wbc_data$measurement_mean <- rowMeans(wbc_data[, c("Rep1", "Rep2", "Rep3", "Rep4", "Rep5", "Rep6", "Rep7","Rep8", "Rep9","Rep10")])
wbc_data$measurement_sd <- apply(wbc_data[, c("Rep1", "Rep2", "Rep3", "Rep4", "Rep5", "Rep6", "Rep7","Rep8", "Rep9","Rep10")], 1, sd)

lambda <- mean(wbc_data$measurement_sd^2, na.rm = TRUE) / mean(wbc_data$manual_sd^2, na.rm = TRUE)

#####OLS#######
wbc_ols <-mcreg(
  x = wbc_data$manual_mean,
  y = wbc_data$measurement_mean,
  error.ratio = 1,
  alpha = 0.05,
  method.reg = "LinReg",
  method.ci = "bootstrap",
  method.bootstrap.ci = "quantile",
  slope.measure = "radian",
)
summary(wbc_ols)
#grafikus 1. 
plot(wbc_ols, main= 'WBC OLS')
#grafikus 2.
coef_ols <- wbc_ols@para
intercept <- coef_ols[1,1]
slope <- coef_ols[2,1]

r2 <- cor(wbc_data$manual_mean,
          wbc_data$measurement_mean,
          use = "complete.obs")^2

plot(wbc_data$manual_mean,
     wbc_data$measurement_mean,
     main = "WBC Linearitás OLS (mcr LinReg)",
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
##Súlyozás###
wbc_wls <-mcreg(
  x = wbc_data$manual_mean,
  y = wbc_data$measurement_mean,
  alpha = 0.05,
  method.reg = "WLinReg",
  method.ci = "bootstrap",
  method.bootstrap.ci = "quantile",
  slope.measure = "radian",
)

summary(wbc_wls)
# grafikus 1. 
plot(wbc_wls, main='WBC WLS')
#grafikus 2.
coef_wls <- wbc_wls@para
intercept_wls <- coef_wls[1,1]
slope_wls <- coef_wls[2,1]
r2 <- cor(wbc_data$manual_mean, wbc_data$measurement_mean)^2
eq_wls <- paste0("WLS: y = ", round(slope_wls, 3), "x + ", round(intercept_wls, 3),
                 "\nR² = ", round(r2, 3))

plot(wbc_data$manual_mean,
     wbc_data$measurement_mean,
     main = "WBC Linearitás WLS",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept_wls, b = slope_wls, col = "red", lwd = 2)

text(
  x = min(wbc_data$manual_mean) + 0.3 * diff(range(wbc_data$manual_mean)),
  y = max(wbc_data$measurement_mean) - 0.1 * diff(range(wbc_data$measurement_mean)),
  labels = eq_wls,
  adj = c(0, 1),
  col = "black"
)

##############Összehasonlítás###########
# OLS paraméterek
coef_ols <- wbc_ols@para
int_ols <- coef_ols[1,1]
slope_ols <- coef_ols[2,1]

# WLS paraméterek
coef_wls <- wbc_wls@para
int_wls <- coef_wls[1,1]
slope_wls <- coef_wls[2,1]

plot(wbc_data$manual_mean,
     wbc_data$measurement_mean,
     main = "WBC Linearitás OLS vs WLS (mcr)",
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
r2 <- cor(wbc_data$manual_mean,
          wbc_data$measurement_mean)^2
eq_full <- paste0(eq_ols, "\n", eq_wls, "\nR² = ", round(r2, 3))
text(
  x = min(wbc_data$manual_mean) + 0.2 * diff(range(rbc_data$manual_mean)), 
  y = max(wbc_data$measurement_mean) - 0.1 * diff(range(rbc_data$measurement_mean)), 
  labels = eq_full,
  adj = c(0, 1),
  col = "black")

compareFit(wbc_ols, wbc_wls)
#################Deming mcr##########

wbc_deming<- mcreg(
  x = wbc_data$manual_mean,
  y = wbc_data$measurement_mean,
  error.ratio = lambda,
  alpha = 0.05,
  mref.name = "Manual",
  mtest.name = "Measurement",
  method.reg = "Deming",
  method.ci = "bootstrap",
  slope.measure = "radian",
  )
summary(wbc_deming)

#Grafikus 1. 
plot(wbc_deming)
#Grafikus 2.
coef_deming <- wbc_deming@para
intercept_deming <- coef_deming[1,1]
slope_deming <- coef_deming[2,1]
r2 <- cor(wbc_data$manual_mean, wbc_data$measurement_mean)^2
eq_deming <- paste0("Deming: y = ", round(slope_deming, 3), "x + ", round(intercept_deming, 3),
                    "\nR² = ", round(r2, 3))

plot(wbc_data$manual_mean,
     wbc_data$measurement_mean,
     main = "WBC Linearitás Deming",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept_deming, b = slope_deming, col = "red", lwd = 2)
text(
  x = min(wbc_data$manual_mean) + 0.3 * diff(range(wbc_data$manual_mean)),
  y = max(wbc_data$measurement_mean) - 0.1 * diff(range(wbc_data$measurement_mean)),
  labels = eq_deming,
  adj = c(0, 1),
  col = "black"
)

###WDeming####
wbc_wdeming<- mcreg(
  x = wbc_data$manual_mean,
  y = wbc_data$measurement_mean,
  error.ratio = lambda,
  alpha = 0.05,
  mref.name = "Manual",
  mtest.name = "Measurement",
  method.reg = "WDeming",
  method.ci = "bootstrap",
  slope.measure = "radian",
  )
summary(wbc_wdeming)

#MethComp Deming#
library(MethComp)
?MethComp

wbc_MC_deming<-Deming(
  x=wbc_data$manual_mean,
  y=wbc_data$measurement_mean,
  vr = lambda,  #error ratio
  boot = 1000,
  keep.boot = TRUE,
  alpha = 0.05
)
summary(wbc_MC_deming)

#grafikus 1. csak szórásképek ad a MethComp

plot(
  wbc_MC_deming,
  main = "WBC Deming Regression (MethComp)",
  xlab = "Manual",
  ylab = "Measurement",
)

#grafikus 2.

intercept_MC_deming <-  10.2197369
slope_MC_deming<- 0.9931677

plot(
  wbc_data$manual_mean,
  wbc_data$measurement_mean,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "WBC Deming Regression (MethComp)"
)

abline(a = 0, b = 1, col = "grey", lwd = 2, lty = 2) #1:1 identity line, ha egyezne teljesen a két módszer

abline(a = intercept_MC_deming, b = slope_MC_deming,       col = "red", lwd = 2)

##### Deming regresszió SimplyAgree#######
library(SimplyAgree)

df_wbc <- data.frame(
  Manual = wbc_data$manual_mean,
  Measurement = wbc_data$measurement_mean)

wbc_deming_SA <- dem_reg(
  Measurement ~ Manual,
  data = df_wbc,     
  error.ratio = lambda,   
  conf.level = 0.95,     
  weighted = FALSE     
)

summary(wbc_deming_SA)
# Grafikus 1. 
plot(wbc_deming_SA)


#Grafikus 2.
intercept_SA <- wbc_deming_SA$coefficients["(Intercept)"]
slope_SA     <- wbc_deming_SA$coefficients["Manual"]
r2_SA <- cor(df_wbc$Manual, df_wbc$Measurement)^2

eq_SA <- paste0(  "SimplyAgree Deming: y = ", round(slope_SA, 3), "x + ", round(intercept_SA, 3),
                  "\nR² = ", round(r2_SA, 3))

plot(
  df_wbc$Manual, df_wbc$Measurement,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "WBC Deming regresszió (SimplyAgree)"
)

abline(a = intercept_SA, b = slope_SA, col = "red", lwd = 2)

text(
  x = min(df_wbc$Manual) + 0.30 * diff(range(df_wbc$Manual)),
  y = max(df_wbc$Measurement) - 0.10 * diff(range(df_wbc$Measurement)),
  labels = eq_SA,
  adj = c(0, 1),
  col = "black")

#######Súlyozott Deming Simply Agreee##########
weights = 1 / (wbc_data$manual_sd^2 + wbc_data$measurement_sd^2)     ##„weighted OLS vagy weighted functional Deming hibrid”, nem variancia, hanem nagyobb szórás kisebb súlyt kap, kis szórású nagyobbat.

wbc_wdem_SA <-dem_reg(
  Measurement ~ Manual,
  data = df_wbc,
  weighted = TRUE,
  weights = weights
)
summary(wbc_wdem_SA)
## Grafikus 1.
plot(wbc_wdem_SA)

##Grafikus 2.
intercept_wdem_SA <- wbc_wdem_SA$coefficients["(Intercept)"]
slope_wdem_SA     <- wbc_wdem_SA$coefficients["Manual"]
r2_wdem_SA <- cor(df_wbc$Manual, df_wbc$Measurement)^2

eq_wdem_SA <- paste0(  "SA Weighted Deming: y = ",
                       round(slope_wdem_SA, 3), "x + ", round(intercept_wdem_SA, 3),
                       "\nR² = ", round(r2_wdem_SA, 3)
)

plot(
  df_wbc$Manual, df_wbc$Measurement,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "RBC Weighted Deming regresszió (SimplyAgree)"
)

abline(a = intercept_SA, b = slope_SA, col = "red", lwd = 2)

text(
  x = min(df_wbc$Manual) + 0.30 * diff(range(df_wbc$Manual)),
  y = max(df_wbc$Measurement) - 0.10 * diff(range(df_wbc$Measurement)),
  labels = eq_wdem_SA,
  adj = c(0, 1),
  col = "black"
)
##### Passing Bablok mcr#######

wbc_paba_mcr <- mcreg(
  x = wbc_data$manual_mean,
  y = wbc_data$measurement_mean,
  method.reg = "PaBa",
  method.ci = "bootstrap",
  mref.name = "Manual",
  mtest.name = "Measurement",
  alpha = 0.05
)
summary(wbc_paba_mcr)


#Grafikus 1. 
plot(wbc_paba_mcr)

#Grafikus 2. 
coef_paba <- wbc_paba_mcr@para
intercept_paba <- coef_paba[1,1]
slope_paba <- coef_paba[2,1]
r2 <- cor(wbc_data$manual_mean, wbc_data$measurement_mean)^2
eq_paba <- paste0("Passing Bablok: y = ", round(slope_paba, 3), "x + ", round(intercept_paba, 3),
                  "\nR² = ", round(r2, 3))

plot(wbc_data$manual_mean,
     wbc_data$measurement_mean,
     main = "WBC Passing-Bablok (mcr)",
     xlab = "Manual average",
     ylab = "Measurement average",
     pch = 16)

abline(a = intercept_paba, b = slope_paba, col = "red", lwd = 2)
text(
  x = min(wbc_data$manual_mean) + 0.3 * diff(range(wbc_data$manual_mean)),
  y = max(wbc_data$measurement_mean) - 0.1 * diff(range(wbc_data$measurement_mean)),
  labels = eq_paba,
  adj = c(0, 1),
  col = "black"
)

#####Methcomp PAssing-Bablok##
library(MethComp)

wbc_paba_MC <- PBreg(
  x = wbc_data$manual_mean,
  y = wbc_data$measurement_mean,
  conf.level = 0.05       
)
summary(wbc_paba_MC)

wbc_paba_MC$coefficients
#Grafikus 1
plot(wbc_paba_MC, main='WBC Passing-Bablok (MethComp)')
#Grafikus 2

r2 <- cor(wbc_data$manual_mean, wbc_data$measurement_mean)^2

eq_paba <- paste0(  "Passing-Bablok: y = ",  round(slope_paba, 3), "x + ",  round(intercept_paba, 3),
                    "\nR² = ", round(r2, 3))
plot(
  wbc_data$manual_mean,
  wbc_data$measurement_mean,
  pch = 16,
  xlab = "Manual",
  ylab = "Measurement",
  main = "WBC Passing–Bablok Regression (MethComp)"
)

abline(a = intercept_paba, b = slope_paba, col = "red", lwd = 2)

text(
  x = min(wbc_data$manual_mean) + 0.3 * diff(range(wbc_data$manual_mean)),
  y = max(wbc_data$measurement_mean) - 0.1 * diff(range(wbc_data$measurement_mean)),
  labels = eq_paba,
  adj = c(0, 1),
  col = "black"
)

########Bland-Altman mcr#######

MCResult.plotDifference(
  wbc_deming,
  xlab = "Átlag (Manual és Measurement)",
  ylab = "Különbség (Measurement - Manual)",
  ref.line.col = "black",
  bias.line.col = "red",    
  loa.line.col = "blue",    
  plot.type = 3,            # klasszikus BA diagram
  main = "WBC Bland–Altman Plot (mcr)",
  cex = 1.0,
  )

wbc_mean   <- (wbc_data$manual_mean + wbc_data$measurement_mean) / 2
wbc_diff   <- wbc_data$measurement_mean - wbc_data$manual_mean

# 2. Bias és LOA-k
bias_wbc   <- mean(wbc_diff, na.rm = TRUE)
sd_wbc     <- sd(wbc_diff, na.rm = TRUE)

loa_upper  <- bias_wbc + 1.96 * sd_wbc
loa_lower  <- bias_wbc - 1.96 * sd_wbc

bias_wbc; sd_wbc; loa_lower; loa_upper

plot(
  wbc_mean, wbc_diff,
  pch = 16,
  xlab = "Átlag (Manual és Measurement)",
  ylab = "Különbség (Measurement – Manual)",
  main = "WBC – Bland–Altman Plot",
  cex = 1.0,
  ylim = c(loa_lower - 10, loa_upper + 10)   
)


abline(h = 0,          col = "grey40", lwd = 2, lty = 2)
abline(h = bias_wbc,   col = "red",    lwd = 3)
abline(h = loa_upper,  col = "blue",   lwd = 2, lty = 2)
abline(h = loa_lower,  col = "blue",   lwd = 2, lty = 2)

legend(
  "topright",
  legend = c(
    paste0("Bias = ", round(bias_wbc, 2)),
    paste0("Upper LOA = ", round(loa_upper, 2)),
    paste0("Lower LOA = ", round(loa_lower, 2))
  ),
  bg = "white",       # háttérdoboz
  bty = "o",
  cex = 0.8,
  text.col = c("red", "blue", "blue")
)

######MethComp BlandAltman#### nem sikerült kivitelezni
library(MethComp)

# 1. Manual mérések hosszú formátumban
manual_long <- data.frame(
  item = rep(1:nrow(wbc_data), each = 2),
  meth = "Manual",
  repl = rep(1:2, times = nrow(wbc_data)),
  y = as.vector(t(wbc_data[, c("Manual 1", "Manual 2")]))
)

# 2. Measurement mérések hosszú formátumban
measurement_long <- data.frame(
  item = rep(1:nrow(wbc_data), each = 10),
  meth = "Measurement",
  repl = rep(1:10, times = nrow(wbc_data)),
  y = as.vector(t(wbc_data[, paste0("Rep", 1:10)]))
)

# 3. Összefűzés MethComp formátumba
wbc_long <- rbind(manual_long, measurement_long)

# 4. Meth objektum készítése
wbc_meth <- Meth(wbc_long)

BA.est(wbc_meth)
BA.plot(wbc_meth, graph = "mean.diff", CI = TRUE, main='WBC Bland-Altman (MC)')

###blandr Bland-Altman####

library(blandr)
library(ggplot2)

blandr.draw(
  wbc_data$manual_mean,
  wbc_data$measurement_mean,
  plotTitle = "WBC – Bland–Altman Plot (blandr)",
)

blandr.display.and.draw(
  method1 = wbc_data$manual_mean,
  method2 = wbc_data$measurement_mean,
  plotter = "ggplot",
  method1name = "Manual",
  method2name = "Measurement",
  plotTitle = "WBC – Bland–Altman (blandr)",
  sig.level = 0.95,
  point_size = 1.8
)

ba_wbc <- blandr.statistics(
  wbc_data$manual_mean,
  wbc_data$measurement_mean)

p <- blandr.draw(
  wbc_data$manual_mean,
  wbc_data$measurement_mean,
  plotter = "ggplot",
  ciDisplay = TRUE,
  ciShading = TRUE,
  annotate = FALSE,
  plotTitle = "WBC – Bland–Altman Plot (blandr)"
)

p +
  annotate(
    "label",
    x = max(wbc_data$manual_mean) * 0.55,
    y = ba_wbc$upperLOA,
    label = paste0(
      "Upper LOA = ", round(ba_wbc$upperLOA, 1), "\n",
      "CI: ", round(ba_wbc$upperLOA_lowerCI, 1), " – ", round(ba_wbc$upperLOA_upperCI, 1)
    ),
    fill = "white", color = "darkgreen", size = 4, label.size = 0.3
  ) +
  annotate(
    "label",
    x = max(wbc_data$manual_mean) * 0.55,
    y = ba_wbc$bias,
    label = paste0(
      "Bias = ", round(ba_wbc$bias, 1), "\n",
      "CI: ", round(ba_wbc$biasLowerCI, 1), " – ", round(ba_wbc$biasUpperCI, 1)
    ),
    fill = "white", color = "blue", size = 4, label.size = 0.3
  ) +
  annotate(
    "label",
    x = max(wbc_data$manual_mean) * 0.55, y = ba_wbc$lowerLOA, label = paste0(  "Lower LOA = ", round(ba_wbc$lowerLOA, 1), "\n",
                                                                                "CI: ", round(ba_wbc$lowerLOA_lowerCI, 1), " – ", round(ba_wbc$lowerLOA_upperCI, 1)
    ),    fill = "white", color = "red3", size = 4, label.size = 0.3  )


###blandr method comparison - Nem Bland–Altman módszer, hanem egy kiegészítő diagnosztikai blokk

wbc_blandr_metodcomparison<- blandr.method.comparison(
  method1 = wbc_data$manual_mean,
  method2 = wbc_data$measurement_mean
)

blandr.plot.ggplot(
  statistics.results = ba_wbc,
  method1name = "Manual",
  method2name = "Measurement",
  plotTitle = "WBC Bland–Altman Plot (blandr)",
  x.plot.mode = "means",
  y.plot.mode = "difference",
  plotProportionalBias = TRUE,
  plotProportionalBias.se = TRUE,
  assume.differences.are.normal = TRUE)

limits<-blandr.plot.limits(  statistics.results = ba_wbc, lowest_y_axis = FALSE, highest_y_axis = FALSE)

blandr.plot.rplot(
  statistics.results = ba_wbc,
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
  data = wbc_data,
  data_type = "simple",          
  loa_calc = "blandaltman",
 )

#### prop.bias-sal jobb, ismétléseket figyelembe véve: 
library(dplyr)
library(tidyr)

wbc_manual_long <- wbc_data %>%
  select(id = 1, `Manual 1`, `Manual 2`) %>%
  pivot_longer(cols = starts_with("Manual"),
               names_to = "manual_repl",
               values_to = "x") %>%   # x = manual measurement
  mutate(manual_repl = as.numeric(gsub("Manual ", "", manual_repl)))

wbc_meas_long <- wbc_data %>%
  select(id = 1, starts_with("Rep")) %>%
  pivot_longer(cols = starts_with("Rep"),
               names_to = "meas_repl",
               values_to = "y") %>%   # y = machine measurement
  mutate(meas_repl = as.numeric(gsub("Rep", "", meas_repl)))


wbc_reps <- wbc_manual_long %>%
  left_join(wbc_meas_long, by = "id")

####proportional bias corrected LOA,
agreement_limit(
  x = "x",
  y = "y",
  id = "id",
  data = wbc_reps,
  data_type = "reps",          
  loa_calc = "mover",          
  prop_bias = TRUE,           
  agree.level = 0.95,
  alpha = 0.05
)





