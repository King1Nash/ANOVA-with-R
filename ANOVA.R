
#ANALYSIS OF VARIANCE

data("InsectSprays")
mydata=InsectSprays
str(mydata)
View(mydata)
#aov test(ANOVA)
#categories(k=6=(A,B,C,D,E,F))
#K-1=6-1=5
#N=72
count(mydata)
#N-K=72-6=66
aov(count~spray,data = mydata)
myaov=aov(count~spray,data = mydata)
summary(myaov)

#post-hoc analysis
TukeyHSD(myaov)
summary.lm(myaov)


rownames(mydata)
rownames(mydata)


require(stats); require(graphics)
n <- length(dr <- c(VADeaths))
n
nam <- names(VADeaths)
nam
d.VAD <- data.frame(
  Drate = dr,
  age = rep(ordered(rownames(VADeaths)), length.out = n),
  gender = gl(2, 5, n, labels = c("M", "F")),
  site =  gl(2, 10, labels = c("rural", "urban")))
view(d.VAD)
coplot(Drate ~ as.numeric(age) | gender * site, data = d.VAD,
       panel = panel.smooth, xlab = "VADeaths data - Given: gender")
summary(aov.VAD <- aov(Drate ~ .^2, data = d.VAD))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
plot(aov.VAD)
par(opar)
