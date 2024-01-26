#IVS are first; plots for type and mfr
#first is type
Type <- table(df_cereal_25$type)
barplot(Type)

#Next is our mfr
Manufacturer <- table(df_cereal_25$mfr)
barplot(Manufacturer)

#DVs are second: plots for calories and rating
#Calories first
Calories <- df_cereal_25$calories
hist(Calories)
#Ratings second
Ratings <- df_cereal_25$rating
hist(Ratings)

#Next, tests for mean differences (ANOVA[mfr] and t-test[type])
#First a t-test- calloric differences between type (hot or cold)
t.test(calories ~ type, df_cereal_25)
#On avg., cold sereals have ~7 more calories per serving


#Move onto ANOVA (3+ levels of our IV, mfr)
summary(aov(calories ~ mfr, df_cereal_25))
TukeyHSD(aov(calories ~ mfr, df_cereal_25))

#Zone in on marginally sig. difference from TukeyHSD
mean(df_cereal_25$calories[df_cereal_25$mfr == "N"])
mean(df_cereal_25$calories[df_cereal_25$mfr == "G"])

#Next a t-test for rating
t.test(rating ~ type, df_cereal_25)
library(ggplot2)
Graph2_base <- ggplot(df_cereal_25, aes(y = rating,
                                   x = type))
Graph2_final <- Graph2+
  stat_summary(fun.y = mean,
                geom = "bar",
                color = "black",
                fill = "gray")+
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .15)+
  ylim(0,70)+
  geom_segment(x = 1, y = 65, xend = 2, yend = 65)+
  geom_text(x = 1.5, y = 65, label = "*", size = 12)+
  theme_classic()+
  labs(x = "Type of Cereal", y = "Consumer Rating")
#It seems hot cereals were rated more favorably than cold cereals by a factor
#of about 1.35

#Means were provided, view in console 
summary(aov(rating ~ mfr, df_cereal_25))
library(ggplot2)
Graph3_base <- ggplot(df_cereal_25, aes(y = rating,
                                   x = mfr))
Graph3_final <- Graph3+
  stat_summary(fun.y = mean,
               geom = "bar",
               color = "black",
               fill = "gray")+
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .15)+
  ylim(0,100)+
  theme_classic()+
  labs(x = "Manufacturer Initial", y = "Consumer Rating")
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
