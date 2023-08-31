fantano <- readr::read_csv("fantano.csv")
score <- fantano |>
  select(fantano_rating, user_score, critic_score,
         number_user_reviews, number_critic_reviews,genres) |>
  filter(user_score != "NR") |>
  filter(fantano_rating != "NA")|>
  mutate(user_score = as.numeric(user_score))|>
  mutate(hip_hop = ifelse(str_detect(genres, "Hip Hop"),1,0)) |>
  mutate(trap = ifelse(str_detect(genres, "Trap"),1,0))

Hip_Hop <- score |>
  filter(hip_hop == 1)
  

a <- lm(fantano_rating ~ user_score, data = Hip_Hop)
a1 <- ggplot(data = Hip_Hop) +
  geom_point(aes(x = user_score, y = fantano_rating), color = "darkgreen",
             alpha = 0.8, size = 2)+
  geom_abline(aes(intercept = c$coefficients[[1]],
                  slope = c$coefficients[[2]]), color = "blue")+
  geom_abline(aes(intercept = 0,
                  slope = 1), color = "red")+
  theme(legend.position = "none")+
  labs(x="Score from Users", y = "Fantano Rating",
       title = "Relation of Users Score and Fantano Rating(Hop-Hop)")


b <- lm(fantano_rating ~ critic_score, data = Hip_Hop)
b1<- ggplot(data = Hip_Hop) +
  geom_point(aes(x = critic_score, y = fantano_rating), color = "darkgreen",
             alpha = 0.8, size = 2)+
  geom_abline(aes(intercept = c$coefficients[[1]],
                  slope = c$coefficients[[2]]), color = "blue")+
  geom_abline(aes(intercept = 0,
                  slope = 1), color = "red")+
  theme(legend.position = "none")+
  labs(x="Score from Critics", y = "Fantano Rating",
       title = "Relation of Critics Score and Fantano Rating(Hop-Hop)")

Trap <- score|>
  filter(trap == 1)

c <- lm(fantano_rating ~ user_score, data = Trap)
c1<- ggplot(data = Trap) +
  geom_point(aes(x = user_score, y = fantano_rating), color = "darkgreen",
             alpha = 0.8, size = 2)+
  geom_abline(aes(intercept = d$coefficients[[1]],
                  slope = d$coefficients[[2]]), color = "blue")+
  geom_abline(aes(intercept = 0,
                  slope = 1), color = "red")+
  theme(legend.position = "none")+
  labs(x="Score from Users", y = "Fantano Rating",
       title = "Relation of Users Score and Fantano Rating(Trap)")



d <- lm(fantano_rating ~ critic_score, data = Trap)
d1 <- ggplot(data = Trap) +
  geom_point(aes(x = critic_score, y = fantano_rating), color = "darkgreen",
             alpha = 0.8, size = 2)+
  geom_abline(aes(intercept = d$coefficients[[1]],
                  slope = d$coefficients[[2]]), color = "blue")+
  geom_abline(aes(intercept = 0,
                  slope = 1), color = "red")+
  labs(x="Score from Critics", y = "Fantano Rating",
       title = "Relation of Critics Score and Fantano Rating(Trap)")

library(ggpubr)
figure <- ggarrange(a1,b1,c1,d1,
                    ncol = 2, nrow = 2)
figure
  
####
a <- lm(fantano_rating ~ user_score + critic_score + number_user_reviews +
          number_critic_reviews, data = score)
summary(a)
####
b <- lm(fantano_rating ~ user_score, data = score)
plot(b,c(1:3))
summary(b)
anova(b)
ggplot(data = score) +
  geom_point(aes(x = user_score, y = fantano_rating), color = "darkgreen",
             alpha = 0.8, size = 2)+
  geom_abline(aes(intercept = b$coefficients[[1]],
                  slope = b$coefficients[[2]]), color = "blue")+
  labs(x="Score from Users", y = "Fantano Ranting",
       title = "Relation between Users Score and Fantano Ranting")


d1 <- residuals(lm(fantano_rating ~ number_user_reviews, data = score)) 
m1 <- residuals(lm(user_score~ number_user_reviews, data = score))

d2 <- residuals(lm(fantano_rating ~ user_score, data = score)) 
m2 <- residuals(lm(number_user_reviews~ user_score, data = score))

par(mfrow=c(1,2))
plot(m1,d1,xlab="user_score residual",ylab="fantano_rating residual",
     main="Added variable plot for user_score")
plot(m2,d2,xlab="number_user_reviews residual",ylab="fantano_rating residual",
     main="Added variable plot for critic_score")

model_full <- lm(fantano_rating ~ user_score+number_user_reviews, data = score )
avPlots(model_full)
#################
d1 <- residuals(lm(fantano_rating ~ number_critic_reviews, data = score)) 
m1 <- residuals(lm(critic_score~ number_critic_reviews, data = score))

d2 <- residuals(lm(fantano_rating ~ critic_score, data = score)) 
m2 <- residuals(lm(number_critic_reviews~ critic_score, data = score))

par(mfrow=c(1,2))
plot(m1,d1,xlab="critic_score residual",ylab="fantano_rating residual",
     main="Added variable plot for critic_score")
plot(m2,d2,xlab="number_critic_reviews residual",ylab="fantano_rating residual",
     main="Added variable plot for critic_score")

model_full <- lm(fantano_rating ~ critic_score+number_critic_reviews, data = score )
avPlots(model_full)