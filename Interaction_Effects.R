library(ggplot2);
library(gridExtra);
library(dplyr);

source('generate_data.R');

### Creating ice cream data
# set.seed(2);
ice.cream <- generate_ice_cream_data(50);

### Without Interaction Term
lm.all <- lm(Happiness ~ Ice_Cream_Scoops, data = ice.cream)
summary(lm.all)

p.1 <- ggplot(ice.cream, aes(x = Ice_Cream_Scoops, y = Happiness)) +
  geom_point(size = 2, color = "#F8766D") +
  xlab("Ice Cream Scoops") +
  ylab("Happiness") +
  theme_bw() +
  labs(title = "Without Interaction Term")
p.1 + geom_abline(intercept = lm.all$coefficients[1], slope = lm.all$coefficients[2], color = "#F8766D", size = 1.5)


### Two Separate fits
lm.no <- lm(Happiness ~ Ice_Cream_Scoops, data = ice.cream |> filter(Lactose_Intolerant));
lm.yes <- lm(Happiness ~ Ice_Cream_Scoops, data = ice.cream |> filter(!Lactose_Intolerant));

p.2 <- ggplot(ice.cream, aes(x = Ice_Cream_Scoops, y = Happiness, col = Lactose_Intolerant)) +
  geom_point(size = 2) +
  xlab("Ice Cream Scoops") +
  ylab("Happiness") +
  labs(color = "Lactose Intolerant", title = "Separate Fits for Lactose Tolerance") +
  theme_bw()

p.2
p.2 + geom_abline(intercept = lm.no$coefficients[1], slope = lm.no$coefficients[2], color = "#00BFC4", size = 1.5) +
      geom_abline(intercept = lm.yes$coefficients[1], slope = lm.yes$coefficients[2], color = "#F8766D", size = 1.5)


### With Interaction Term
lm.interaction <- lm(Happiness ~ Ice_Cream_Scoops + Lactose_Intolerant + Ice_Cream_Scoops*Lactose_Intolerant, data = ice.cream)
summary(lm.interaction)

p.3 <- ggplot(ice.cream, aes(x = Ice_Cream_Scoops, y = Happiness, col = Lactose_Intolerant)) +
  geom_point(size = 2) +
  xlab("Ice Cream Scoops") +
  ylab("Happiness") +
  labs(color = "Lactose Intolerant", title = "With Interaction Term") +
  theme_bw()
p.3 + geom_abline(intercept = lm.interaction$coefficients[1], slope = lm.interaction$coefficients[2], color = "#F8766D", size = 1.5)


### Two Separate fits GENDER

lm.m <- lm(Happiness ~ Ice_Cream_Scoops, data = ice.cream |> filter(Gender == "M"));
lm.f <- lm(Happiness ~ Ice_Cream_Scoops, data = ice.cream |> filter(Gender == "F"));

p.2 <- ggplot(ice.cream, aes(x = Ice_Cream_Scoops, y = Happiness, col = Gender)) +
  geom_point(size = 2) +
  xlab("Ice Cream Scoops") +
  ylab("Happiness") +
  labs(color = "Gender", title = "Separate Fits for Gender") +
  theme_bw()

p.2 + geom_abline(intercept = lm.m$coefficients[1], slope = lm.m$coefficients[2], color = "#00BFC4", size = 1.5) +
  geom_abline(intercept = lm.f$coefficients[1], slope = lm.f$coefficients[2], color = "#F8766D", size = 1.5)

lm_interaction_gender <- lm(Happiness ~ Ice_Cream_Scoops + Gender + Ice_Cream_Scoops*Gender, data = ice.cream)
summary(lm_interaction_gender)




