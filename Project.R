library(readr)
library(MASS)
library(car)
library(olsrr)

Life <- read_csv("Life Expectancy Data.csv")
nrow(Life)
life<- na.omit(Life, invert = FALSE)
nrow(life)

fit <-lm(life$`Life expectancy`~.,data = life[,-1])
fit <-lm(life$`Life expectancy` ~ Year + `Adult Mortality` + 
    `infant deaths` + `percentage expenditure` + BMI + 
    `HIV/AIDS` + `Income composition of resources` + 
    Schooling, data = life[,-1])
summary(fit)

win.graph(width=30, height=15,pointsize=12)
par(mfrow=c(2,2))
plot(fit)

boxcox(fit)
hist(resid(fit))
qqnorm(resid(fit))
qqline(resid(fit))
plot(fitted.values(fit), resid(fit))
abline(h=10, col = "red")
abline(h=-10, col = "red")

test <- life[,-c(1,2,3)]
fit3<-lapply(test, as.numeric)
View(test)
win.graph(width=30, height=15,pointsize=12)
pairs(fit3[c(1,10:15)], pch = 10)

summary(fit)
step <- stepAIC(fit, direction = "both")
step$anova

fit2 <- lm(life$`Life expectancy` ~ Year + `Adult Mortality` + 
    `infant deaths` + `percentage expenditure` + BMI + 
    `HIV/AIDS` + `Income composition of resources` + 
    Schooling, data = life[,-1])

AIC(fit2)
summary(fit2)

win.graph(width=30, height=15,pointsize=12)
par(mfrow=c(2,2))
plot(fit2)

win.graph(width=30, height=15,pointsize=12)
par(mfrow=c(2,2))
boxcox(fit2)
hist(resid(fit2))
qqnorm(resid(fit2))
qqline(resid(fit2))
plot(fitted(fit2), resid(fit2))
abline(h=10, col = "red")
abline(h=-10, col = "red")

win.graph(width=30, height=15,pointsize=12)
par(mfrow=c(4,4))
avPlots(lm(life$`Life expectancy` ~ Year + Status + `Adult Mortality` + 
             `infant deaths` + Alcohol + `percentage expenditure` + BMI + 
             `under-five deaths` + `Total expenditure` + Diphtheria + 
             `HIV/AIDS` + `thinness 5-9 years` + `Income composition of resources` + 
             Schooling, data = life[,-1]))

win.graph(width=30, height=15,pointsize=12)
influencePlot(fit2)
im <-influence.measures(fit2)
influence(fit2)

#------------------------------
# Reduced model transformation with a power of 1.6
#------------------------------
p2 <- life
p2[,4] = p2$`Life expectancy`^(1.6)
fitt <- lm(life$`Life expectancy` ~ Year + `Adult Mortality` + 
             `infant deaths` + `percentage expenditure` + BMI + 
             `HIV/AIDS` + `Income composition of resources` + 
             Schooling, data = p2[,-1])

AIC(fitt)
BIC(fitt)
summary(fitt)

win.graph(width=30, height=15,pointsize=12)
par(mfrow=c(2,2))
plot(fitt)

win.graph(width=30, height=15,pointsize=12)
par(mfrow=c(2,2))

hist(resid(fitt))
qqnorm(resid(fitt))
qqline(resid(fitt))
plot(fitted(fitt), resid(fitt))
abline(h=10, col = "red")
abline(h=-10, col = "red")

#--------------------------------
# Outlayer for each of the test
#--------------------------------

life[which(abs(dffits(fit2)) > 2/sqrt(14/1636)),]
life[which(cooks.distance(fit2) >1),]
life[which(hatvalues(fit2) > (28/1636)),]
life[which(abs(dfbeta(fit2)) > (2/sqrt(1636))),]
life[which(covratio(fit2) > (1 + 3*24/1636)),]
life[which(covratio(fit2) < (1 - 3*24/1636)),]

#-------------------------------
# Interaction term
#-------------------------------
fiti <-lm(life$`Life expectancy`~(Year + `Adult Mortality` + 
             `infant deaths` + `percentage expenditure` + BMI + 
            `HIV/AIDS` + `Income composition of resources` + 
             Schooling)^2,data = life[,-1])
summary(fiti)

win.graph(width=30, height=15,pointsize=12)
plot(fiti)

win.graph(width=30, height=15,pointsize=12)
par(mfrow=c(2,2))
boxcox(fiti)
hist(resid(fiti))
qqnorm(resid(fiti))
qqline(resid(fiti))
plot(fitted.values(fiti), resid(fiti))
abline(h=10, col = "red")
abline(h=-10, col = "red")

testi <- life[,-c(1,2,3)]
fit4i<-lapply(testi, as.numeric)
win.graph(width=30, height=15,pointsize=12)
pairs(fit4i[c(1,10:15)], pch = 10)

step <- stepAIC(fiti, direction = "both")
step$anova

fit2i <- lm(life$`Life expectancy` ~ Year + `Adult Mortality` + `infant deaths` + 
    `percentage expenditure` + BMI + `HIV/AIDS` + `Income composition of resources` + 
    Schooling + Year:`percentage expenditure` + Year:`HIV/AIDS` + 
    Year:`Income composition of resources` + Year:Schooling + 
    `Adult Mortality`:`infant deaths` + `Adult Mortality`:`percentage expenditure` + 
    `Adult Mortality`:BMI + `Adult Mortality`:`HIV/AIDS` + `Adult Mortality`:`Income composition of resources` + 
    `Adult Mortality`:Schooling + `infant deaths`:BMI + `infant deaths`:`HIV/AIDS` + 
    `infant deaths`:`Income composition of resources` + `infant deaths`:Schooling + 
    `percentage expenditure`:BMI + `percentage expenditure`:`Income composition of resources` + 
    BMI:`Income composition of resources` + BMI:Schooling + `HIV/AIDS`:`Income composition of resources` + 
    `HIV/AIDS`:Schooling, data = life[,-1])

summary(fit2i)
AIC(fit2i)
BIC(fit2i)
win.graph(width=30, height=15,pointsize=12)
par(mfrow=c(2,2))
boxcox(fit2i)
hist(resid(fit2i))
qqnorm(resid(fit2i))
qqline(resid(fit2i))
plot(fitted(fit2i), resid(fit2i))
abline(h=10, col = "red")
abline(h=-10, col = "red")

win.graph(width=30, height=15,pointsize=12)
influencePlot(fit2i)
influence.measures(fit2i)
influence(fit2i)
#-------------------------------

p2 <- life
p2[,4] = p2$`Life expectancy`^(3/2)

fitp2 <-lm(p2$`Life expectancy`~.,data = p2[,-1])
summary(fitp2)
win.graph(width=30, height=15,pointsize=12)
par(mfrow=c(2,2))
boxcox(fitp2)
hist(resid(fitp2))
qqnorm(resid(fitp2))
qqline(resid(fitp2))
plot(fitted(fitp2), resid(fitp2))
