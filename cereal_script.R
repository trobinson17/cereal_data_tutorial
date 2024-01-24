df$name
hist(df$calories)
df
t.test(calories ~ type, df)
calories_mfr <- aov(calories ~ mfr, df)
TukeyHSD(calories_mfr)
mean(df$calories[df$mfr == "N"])
mean(df$calories[df$mfr == "G"])
