library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)
library(emmeans)
library(janitor)

#Our hypothesis can therefore be: mean frogspawn hatching time will vary with temperature level. We can predict that given our temperature range, at the highest temperature (25°C) hatching time will be reduced.
frogs <- read_csv ("Data/frogs_messy_data.csv")
head(frogs)
colnames(frogs)

frogs <- janitor::clean_names(frogs)

frogs <- frogs %>%
  rename("13°C" = temperature13,
         "18°C" = temperature18,
         "25°C" = temperature25,
         frog_id = "frogspawn_sample_id") %>%
  pivot_longer("13°C" : "25°C", values_to="days") %>%
  drop_na(days)

frogs <- frogs %>%
  rename(Temperature = "name")

lsmodel_frogs1 <- lm(days ~ Temperature, data = frogs)

summary(lsmodel_frogs1)

anova(lsmodel_frogs1)

broom::tidy(lsmodel_frogs1, conf.int = T)

frogs %>%
  ggplot(aes(x=Temperature,
             y=days))+
  geom_point()

coef(lsmodel_frogs1)

df <- c(1:30)

critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

performance::check_model(lsmodel_frogs1,
                         check = c("qq", "outliers", "homogeneity"))

frogs_colours <- c("peachpuff1", "mediumpurple1", "seagreen1")

frogs %>% 
  ggplot(aes(fill=Temperature))+
  geom_density(aes(x=days, y=..density..),
               alpha=0.6,
               colour="goldenrod2")+
  labs(x = "Hatching Time (Days)",
       y = "Density")+
  scale_fill_manual(values=frogs_colours)+
  theme_minimal()
