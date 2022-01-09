fit.sex.1 <- lm(formula = 1 / Bsal ~ Sex + Senior + Age + Educ + Exper +
                  sqrt(Exper),
                data = df)

summary(fit.sex.1)
# sex is significant predictor, males is negative correlation but to inverse of salary

# things I'd like to try
# + 1 to Exper and then log trans{tick}
# plot all variables to Bsal withsex as color



df.plus1 <- df
df.plus1$Exper = df.plus1$Exper + 1
fit.sex.1 <-
  lm(
    formula = 1 / Bsal ~ Sex + Senior + Age + Educ + Exper + log(Exper),
    data = df.plus1$Exper
  )
summary(fit.sex.1)
#this made no difference.... go back to sqrt

#let s visualise how gender plays into all the other relationships
library(ggplot2)
opar <- par(mfrow = c(2, 2))
ggplot(df, aes(x = Senior, y = Bsal, colour = Sex)) + geom_point()
ggplot(df, aes(x = Age, y = Bsal, colour = Sex)) + geom_point()
ggplot(df, aes(x = Educ, y = Bsal, colour = Sex)) + geom_point()
ggplot(df, aes(x = Exper, y = Bsal, colour = Sex)) + geom_point()
opar
# okay, so visually it looks like men consistently get paid more

fit.sex.int <-
  lm(formula = 1 / Bsal ~ (Senior + Age + Educ + Exper + sqrt(Exper)) * Sex,
     data = df)
summary(fit.sex.int)

sfit.int <- step(fit.sex.int, scope = list(
  lower = . ~ 1,
  upper = . ~ (Senior + Age + Educ + Exper + sqrt(Exper)) * Sex
))
summary(sfit.int)
fit.best.sex <-
  lm(formula = 1 / Bsal ~ Senior + Educ + Exper + sqrt(Exper) + Sex,
     data = df)
1 / confint(fit.best.sex)


# separate dots into different age groups
df.young <- subset(df, Age <= 400)
df.old <- subset(df, Age > 400)

#old
fit.old <-
  lm(formula = 1 / Bsal ~ Age + Senior + Educ + Exper + sqrt(Exper) + Sex,
     data = df.old)
summary(fit.old)

#young
fit.young <-
  lm(formula = 1 / Bsal ~ Age + Senior + Educ + Exper + sqrt(Exper) + Sex,
     data = df.young)
summary(fit.young)

# age:sex interactions?
# old
sfit.int.old <- step(fit.old, scope = list(
  lower = . ~ 1,
  upper = . ~ (Senior + Age + Educ + Exper + sqrt(Exper)) * Sex
))
summary(sfit.int.old)

# age:sex interactions?
# young
sfit.int.young <- step(fit.young, scope = list(
  lower = . ~ 1,
  upper = . ~ (Senior + Age + Educ + Exper + sqrt(Exper)) * Sex
))
summary(sfit.int.young)

#what if we remove those two unusual women
fit.no16_79 <-
  lm(formula = 1 / Bsal ~ Senior + Educ + Exper + sqrt(Exper) + Sex,
     data = df[-16, -79, ])
summary(fit.no16_79)
#sex is still significant

# what about just sex
fit.sex <- lm(Bsal ~ Sex, data = df)
summary(fit.sex)



# see how gender relates age (seems there are lots of young men)

df.males <- subset(df, Sex == "Male")
df.young.males <- subset(df.males, Age < 450)
