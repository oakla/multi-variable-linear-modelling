# let's read in the data

df <- read.csv("Bank.csv")
head(df)
str(df)

# we only have 1 categorical variable; sex

# lets look at everything other than sex first and those
# variable relate to Bsal

df.nosex <- df[, -3]

pairs(df.nosex[2:6])
# lets try looking at a log transform
pairs(log(df.nosex[2:6]))
# a log transform doesn't do much
# except for maybe the link between age and and experience

# pairs doesn't actually make anything pop out
# lets do some histograms
opar = par(mfrow = c(3, 2))
for (i in 2:6) {
  hist(df.nosex[, i], main = colnames(df.nosex)[i])
}
par(opar)

#There is some obvious positive skew in experience
#also some weird bi-modal stuff going on in age
# senior is kind of normal except for a large cluster at the
#bottom

# Let's look for weirdness
opar = par(mfrow = c(3, 2))
for (i in 2:6) {
  boxplot(df.nosex[, i], main = colnames(df.nosex)[i])
}
par(opar)

#there are some weird points
# one standout is in Bsal
high.Bsal <- subset(df, Bsal > 7000)
high.Bsal
# it's pt 7

# Now lets get the unusually high experience points
high.experience <- subset(df, Exper > 275)
high.experience
#There are 5 unusual points here
# 16, 51, 63, 74, 75

# Okay, lets make a model
fit.main <- lm(Bsal ~ Senior + Age + Educ + Exper, data = df.nosex)
summary(fit.main)
#it seems like the most impoartant factors are Educ,
# Seniority  and experience, in that order.
# Age seems to be most useless

#Lets check age on its own
fit.age <- lm(Bsal ~ Age, data = df.nosex)
summary(fit.age)
# OKAY, weird!
# It's actually *less* significant and the sign of the coefficient
# has reversed

# Maybe age has some tempering effect on Bsal in the context of other variables?
# Lets seeing how age does as we add in the others
# Okay, lets make a model
fit.age.senior <- lm(Bsal ~ Senior + Age, data = df.nosex)
summary(fit.age.senior)
# very low p-value

fit.age.educ <- lm(Bsal ~ Educ + Age, data = df.nosex)
summary(fit.age.educ)
# slightly less low p-value

fit.age.exper <- lm(Bsal ~ Exper + Age, data = df.nosex)
summary(fit.age.exper)
# slightly less low again
# what about log trans of age?
fit.age.exper <- lm(Bsal ~ Exper + log(Age), data = df.nosex)
summary(fit.age.exper)
# slightly less low again

# okay, age is weird... maybe it correlates with sex? have a look later

#but now... lets try the automated variable  finder from lab week 5
sfit <- step(fit.main,
             scope = list(
               lower = . ~ 1,
               upper = . ~ Senior + Age + Educ + Exper +
                 I(Senior ^ 2) + I(Age ^ 2) + I(Educ ^ 2) + I(Exper ^ 2)
             ))
summary(sfit)

# okay, here's an interesting result. Age becomes useful when Exper is quadratic
# lets check if everything is okay with Educ to power 1
fit.polys <-
  lm(Bsal ~ Age + Senior + Exper + I(Exper ^ 2) + Educ + I(Educ ^ 2), data =
       df)
summary(fit.polys)

#Educ is only significant if it doesn't have a 1st order term

# lets try automatic selection with logs too
sfit <- step(
  fit.main,
  scope = list(
    lower = . ~ 1,
    upper = . ~ Senior + Age + Educ + Exper +
      I(Senior ^ 2) + I(Age ^ 2) + I(Educ ^ 2) + I(Exper ^ 2) +
      log(Senior) + log(Age) + log(Educ)
  )
)
summary(sfit)
# Exper logs seem to break because of 0 values
# lets try just Exper
fit.exper <- lm(Bsal ~ Exper, data = df)
summary(fit.exper)
#yep, that doesn't work

#lets try sqrt instead of log for Exper
# lets try automatic selection with logs too
sfit <- step(
  fit.main,
  scope = list(
    lower = . ~ 1,
    upper = . ~ Senior + Age + Educ + Exper +
      I(Senior ^ 2) + I(Age ^ 2) + I(Educ ^ 2) + I(Exper ^ 2) +
      log(Senior) + log(Age) + log(Educ) + sqrt(Exper)
  )
)
summary(sfit)
#GReat! This is the best model yet. The transform matches the histogram
# suggests. WHy didn't I try it sooner? Just say that I saw the histos and thought I'd check everything at once
hist(sqrt(df$Exper)) # looks more normal

#THis model is great, but what about interactions? Outliers or interactions?
fit.1 <-
  lm(formula = Bsal ~ Senior + Age + Educ + Exper + sqrt(Exper),
     data = df.nosex)
summary(fit.1)
opar <- par(mfrow = c(2, 2))
plot(fit.1)
par(opar)
#we have some weird thing with pt 79 (nvm, I think that is just a legend)
#what about other models?
opar <- par(mfrow = c(2, 2))
plot(fit.main)
par(opar)
#hat values
plot(hatvalues(fit.main))
plot(hatvalues(fit.1))
identify(hatvalues(fit.1))
#16 18 75 25 33 51 58 74 17 20 36 63
# Weirdly enough, 7 isn't there
# 16 is the weirdest point, lets look at that one
# see outlier investigation file

# does the response need a transform?
library(MASS)
boxcox(fit.1)
opar <- par(mfrow = c(2, 2))
plot(fit.1)
par(opar)
# do the tranform
fit.1.trans <-
  lm(formula = log(Bsal) ~ Senior + Age + Educ + Exper + sqrt(Exper),
     data = df.nosex)
summary(fit.1.trans)
opar <- par(mfrow = c(2, 2))
plot(fit.1.trans)
par(opar)
#even better
fit.1.trans2 <-
  lm(formula = 1 / Bsal ~ Senior + Age + Educ + Exper + sqrt(Exper),
     data = df.nosex)
summary(fit.1.trans2)
opar <- par(mfrow = c(2, 2))
plot(fit.1.trans2)
par(opar)
# best so far
plot(hatvalues(fit.1.trans2), main = "Hat Values of \n 1/Bsal ~ Senior + Age + Educ + Exper + sqrt(Exper)", ylab =
       "hat values")
identify(hatvalues(fit.1.trans2))
# its still 16 is the weirdest by hatvalue
plot(rstudent(fit.1.trans2), main = "Studentized Residuals of \n 1/Bsal ~ Senior + Age + Educ + Exper + sqrt(Exper)" , ylab =
       "studentized residuals")
identify(rstudent(fit.1.trans2))
# 79 is weird
# what is it about 79?
df[79, ]
#see outlier script
plot(cooks.distance(fit.1.trans2))
