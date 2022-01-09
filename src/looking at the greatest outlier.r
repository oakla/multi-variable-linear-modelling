opar = par(mfrow = c(3, 2))
for (i in 2:6) {
  hist(df.nosex[, i], main = colnames(df.nosex)[i])
  abline(v = df.nosex[16, i], col = "red")
}
par(opar)

df[16, ]
# 16 is high on everything except for Bsal, and
# ta-da, 16 is a woman

# is 16 extra weird for women?
df.women <- subset(df, Sex == "Female")[, -3]
opar = par(mfrow = c(3, 2))
for (i in 2:6) {
  hist(df.women[, i], main = colnames(df.women)[i])
  abline(v = df.women[2, i], col = "red")
}
par(opar)
# even amoung women she's not paid much
df.women

# lets look at 79
opar = par(mfrow = c(3, 2))
for (i in 2:6) {
  hist(df.nosex[, i], main = colnames(df.nosex)[i])
  abline(v = df.nosex[79, i], col = "red")
}
par(opar)

df[79, ]
# 79 is a young women who has been with the company for a long time but is not paid much

# is 79 extra weird for women?
df.women <- subset(df, Sex == "Female")[, -3]
opar = par(mfrow = c(3, 2))
for (i in 2:6) {
  hist(df.women[, i], main = colnames(df.women)[i])
  abline(v = df.nosex[79, i], col = "red")
}
par(opar)
subset(df, Educ > 14)
# not that weird, just highly education and paid very little