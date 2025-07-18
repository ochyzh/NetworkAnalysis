shuttles<-read.table("space_shuttles.txt")

shuttles$failure <- ifelse(shuttles$failure == 0, 0, 1)
m1 <- glm(failure ~ temp, data = shuttles, family = binomial)

library(ggplot2)
g1 <- ggplot(data = shuttles, aes(x = temp, y = failure)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = binomial),
              fill = "grey", color = "black", linetype = 2)

summary(m1)

temp <- seq(from = min(shuttles$temp), to = max(shuttles$temp),
             by = 1)

pred_probs <- plogis(coef(m1)[1] + coef(m1)[2] * temp)

df_to_graph <- as.data.frame(cbind(temp = temp, 
                                   prob = pred_probs))

temp_df <- as.data.frame(temp)

library(magrittr)
library(dplyr)

predictions <- as.data.frame(predict.glm(m1, newdata = temp_df,
                                     se.fit = TRUE)) %>% 
  mutate(
    upper_bound = plogis(fit + 2*se.fit),
    lower_bound = plogis(fit - 2 * se.fit)
  ) %>% 
  cbind(temp_df)

ggplot(predictions, aes(x = temp, y = plogis(fit))) +
  geom_line() +
  geom_ribbon(aes(x = temp, 
                  ymin = lower_bound, 
                  ymax = upper_bound),
              fill = "blue3",
              alpha = 0.2) +
  theme_bw()
