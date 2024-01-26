#IVS are first; plots for type and mfr
#first is type
freq_type <- table(df_cereal_25$type)
barplot(freq_type)

#Next is our mfr
freq_mfr <- table(df_cereal_25$mfr)
barplot(freq_mfr)

#Now let's select two DVs we are interested in: calories and rating
#I should first make some histograms
#Calories first
hist(df_cereal_25$calories)
#Ratings second
hist(df_cereal_25$rating)

#Next, I will conduct two kinds of mean differences analysis (ANOVA and t-test)
#First a t-test- differences between calories by type (hot or cold)
calories_type <- t.test(calories ~ type, df_cereal_25)
t.test(calories ~ type, df_cereal_25)
#On avg., cold sereals have ~7 more calories per serving


#Move onto ANOVA (several levels of our IV, mfr)
ANOVA_mfr_cal <- aov(calories ~ mfr, df_cereal_25)
aov(calories ~ mfr, df_cereal_25)
summary(ANOVA_mfr_cal)
TukeyHSD(ANOVA_mfr_cal)

#Zone in on sig. difference from TukeyHSD
mean(df_cereal_25$calories[df_cereal_25$mfr == "N"])
mean(df_cereal_25$calories[df_cereal_25$mfr == "G"])

#Next a t-test for rating
ratings_type <- t.test(rating ~ type, df_cereal_25)
t.test(rating ~ type, df_cereal_25)
#It seems hot cereals were rated more favorably than cold cereals by a factor
#of about 1.35

#Means were provided, view in console 
summary(aov(rating ~ mfr, df_cereal_25))
TukeyHSD(aov(rating ~ mfr, df_cereal_25))

#Zoom in on sig. difference from TukeyHSD
mean(df_cereal_25$rating[df_cereal_25$mfr == "N"])
mean(df_cereal_25$rating[df_cereal_25$mfr == "G"])
#N greater than G
mean(df_cereal_25$rating[df_cereal_25$mfr == "N"])
mean(df_cereal_25$rating[df_cereal_25$mfr == "K"])
#N greater than K
mean(df_cereal_25$rating[df_cereal_25$mfr == "N"])
mean(df_cereal_25$rating[df_cereal_25$mfr == "P"])
#N greater than P
mean(df_cereal_25$rating[df_cereal_25$mfr == "N"])
mean(df_cereal_25$rating[df_cereal_25$mfr == "Q"])
#N greater than Q
mean(df_cereal_25$rating[df_cereal_25$mfr == "N"])
mean(df_cereal_25$rating[df_cereal_25$mfr == "R"])
#N greater than R
#Internet is not working right now, but it seems that mfr N
#takes home the bread for rating
