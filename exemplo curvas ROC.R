set.seed(123)
class1 <- rnorm(100, mean = 0, sd = 1)
class2 <- rnorm(100, mean = 2, sd = 1)
data1 <- data.frame(values = c(class1, class2),
                    classes = factor(rep(c("class1", "class2"), each = 100)))
model1 <- roc(classes ~ values, data = data1)

set.seed(456)
class3 <- rnorm(100, mean = 0, sd = 1)
class4 <- rnorm(100, mean = 1.5, sd = 0.5)
data2 <- data.frame(values = c(class3, class4),
                    classes = factor(rep(c("class1", "class2"), each = 100)))
model2 <- roc(classes ~ values, data = data2)

set.seed(789)
class5 <- rnorm(100, mean = 0, sd = 1)
class6 <- rnorm(100, mean = 1, sd = 1)
data3 <- data.frame(values = c(class5, class6),
                    classes = factor(rep(c("class1", "class2"), each = 100)))
model3 <- roc(classes ~ values, data = data3)

set.seed(111)
class7 <- rnorm(100, mean = 0.5, sd = 1)
class8 <- rnorm(100, mean = 1.5, sd = 0.5)
data4 <- data.frame(values = c(class7, class8),
                    classes = factor(rep(c("class1", "class2"), each = 100)))
model4 <- roc(classes ~ values, data = data4)

library(ggplot2)

ggplot() +
 
  stat_smooth(data = data.frame(x = c(0, 1 - model1$specificities), y = c(0, model1$sensitivities)),
              aes(x = x, y = y), color = "blue", se = FALSE, method = "loess") +
  stat_smooth(data = data.frame(x = c(0, 1 - model2$specificities), y = c(0, model2$sensitivities)),
              aes(x = x, y = y), color = "red", se = FALSE, method = "loess") +
  stat_smooth(data = data.frame(x = c(0, 1 - model3$specificities), y = c(0, model3$sensitivities)),
              aes(x = x, y = y), color = "green", se = FALSE, method = "loess") +
  stat_smooth(data = data.frame(x = c(0, 1 - model4$specificities), y = c(0, model4$sensitivities)),
              aes(x = x, y = y), color = "purple", se = FALSE, method = "loess") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(x = "1 - Especificidade", y = "Sensitividade", title = "Exemplo Curvas ROC") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_gray() +
  theme(panel.background = element_rect(fill = "gray90"))
