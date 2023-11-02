library(ggplot2);
library(gridExtra);
library(dplyr);

source('generate_data.R');

### Creating ice cream data
set.seed(2);
ice.cream <- generate_ice_cream_data(50);

### Without Interaction Term
lm.all <- lm(Happiness ~ Ice_Cream_Scoops, data = ice.cream)

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
p.2 + geom_abline(intercept = lm.no$coefficients[1], slope = lm.no$coefficients[2], color = "#F8766D", size = 1.5) +
      geom_abline(intercept = lm.yes$coefficients[1], slope = lm.yes$coefficients[2], color = "#00BFC4", size = 1.5)


### With Interaction Term
lm.interaction <- lm(Happiness ~ Ice_Cream_Scoops + Lactose_Intolerant + Ice_Cream_Scoops*Lactose_Intolerant, data = ice.cream)

p.3 <- ggplot(ice.cream, aes(x = Ice_Cream_Scoops, y = Happiness, col = Lactose_Intolerant)) +
  geom_point(size = 2) +
  xlab("Ice Cream Scoops") +
  ylab("Happiness") +
  labs(color = "Lactose Intolerant", title = "With Interaction Term") +
  theme_bw()
p.3 + geom_abline(intercept = lm.interaction$coefficients[1], slope = lm.interaction$coefficients[2], color = "#F8766D", size = 1.5)


### RSS Comparison - Table
rsq <- data.frame("Model" = c("Without Interaction", "Not Lactose Intolerant", "Lactose Intolerant","With Interaction"),
                  "R-Squared" = round(c(summary(lm.all)$r.squared, summary(lm.no)$r.squared, summary(lm.yes)$r.squared, summary(lm.interaction)$r.squared), 2))

grid.table(rsq)






