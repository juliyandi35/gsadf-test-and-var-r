# Generate example time series data
# remotes::install_github("deanfantazzini/bubble")
library(bubble)
library(readxl)
Techno <- read_excel("Historis harga IDXTechno 26012021-11102024.xlsx")
Techno <- Techno$Terakhir
Techno <- rev(Techno)
IHSG <- read_excel("Historis harga IHSG 26012021-11102024.xlsx")
IHSG <- IHSG$Terakhir
IHSG <- rev(IHSG)
Data <- data.frame(Techno = Techno, IHSG = IHSG)
head(Data)

library(ggplot2)
library(plotly)
library(dplyr)
ggplot(Data) +
  aes(x = 1:length(Techno), y = Techno) +
  geom_point(col = "#ED125F") +
  ggthemes::theme_economist_white() + ggtitle("IDXTechno")

ggplot(Data) +
  aes(x = 1:length(IHSG), y = IHSG) +
  geom_point(col = "#ED125F") +
  ggthemes::theme_economist_white() + ggtitle("IHSG")

r0=0.01+1.8/sqrt(length(Data$Techno))
r0
gsadf_test <-GSADF_Y(Data$Techno, r0,6,"BIC",8)

qe <-c(0.90,0.95,0.99)    #quantiles
m <- 10
t=length(Data$Techno)
clust_number=7
cv_gsadf<- CV_GSADF(qe,m,t,r0, clust_number=7, "CV Plot.png")
cat(gsadf_test$gsadf, cv_gsadf$gsadf_cv, sep = "\t")

dateStampData.Frame <- ts(cbind(gsadf_test$bsadfs, cv_gsadf$gsadf_cv)) 

ts.plot(dateStampData.Frame, plot.type = "single", col=c("blue", "red"))

#install.packages("exuber")
library(exuber)
library(exuberdata)
radf_sim <- radf(Data)
install_exuberdata()
summary(radf_sim)
autoplot(radf_sim)

# Date-Stamping
datestamped_result <- datestamp(radf_sim, option = "gsadf")  
datestamped_result

# VAR
Data$IHSG <- log(Data$IHSG)
Data$Techno <- log(Data$Techno)

ggplot(Data) +
  aes(x = 1:length(Techno), y = Techno) +
  geom_point(col = "#ED125F") +
  ggthemes::theme_economist_white() + ggtitle("IDXTechno After Transformation")

ggplot(Data) +
  aes(x = 1:length(IHSG), y = IHSG) +
  geom_point(col = "#ED125F") +
  ggthemes::theme_economist_white() + ggtitle("IHSG After Transformation")

library(tseries)
adf.test(Data$IHSG, k = 1)
adf.test(Data$Techno, k = 1)

adf.test(diff(Data$IHSG), k = 1)
adf.test(diff(Data$Techno), k = 1)

library(lmtest)
grangertest(diff(IHSG) ~ diff(Techno), Data, order = 4)
grangertest(diff(Techno) ~ diff(IHSG), Data, order = 4)

library(vars)
VARselect(Data[c("IHSG", "Techno")])

Model_VAR <- VAR(y = Data[c("IHSG", "Techno")], p = 6, ic = "AIC")
summary(Model_VAR)

serial.test(Model_VAR) # H0: Tidak terdapat korelasi serial
arch.test(Model_VAR, 10, 10) # H0: Ragam Galat Konstan
normality.test(Model_VAR) # H0: Galat berdistribusi normal

## Impulse Response Functions (IRF)
IRF_IHSG1 <- irf(Model_VAR,
                impulse = "IHSG",
                response = "IHSG")
plot(IRF_IHSG1, main = "Shock IHSG to IHSG")

IRF_IHSG2 <- irf(Model_VAR,
                impulse = "IHSG",
                response = "Techno")
plot(IRF_IHSG2, main = "Shock IHSG to Techno")

IRF_Techno1 <- irf(Model_VAR,
                impulse = "Techno",
                response = "Techno")
plot(IRF_Techno1, main = "Shock from Techno to Techno")

IRF_Techno2 <- irf(Model_VAR,
                impulse = "Techno",
                response = "IHSG")
plot(IRF_Techno2, main = "Shock from Techno to IHSG")

## Variance Decomposition
VD <- fevd(Model_VAR)
plot(VD)

## Peramalan
fc <- predict(Model_VAR)

fanchart(fc, names = "IHSG")
fanchart(fc, names = "Techno")

exp(fc$fcst$IHSG)
exp(fc$fcst$Techno)
