# CHOOSING FUNCTIONAL FORM
# DATA SOURCE: FRED https://fredaccount.stlouisfed.org/public/dashboard/9575

data <- matrix(c(1.826,	105.754,
                 2.040,	105.459,
                 2.471,	102.163,
                 1.635,	101.773,
                 2.054,	102.070,
                 2.748,	100.637,
                 2.816,	100.000,
                 2.694,	101.602,
                 2.485,	101.333,
                 1.554,	106.006), 
               ncol = 2,
               byrow = TRUE)

data <- data.frame(Price = data[,1],
                   Demand = data[,2])

print(data)

library(ggplot2)
library(ggthemes)

# PLOT 1
ggplot(data = data,
       aes(x = Demand,
           y = Price)) +
  geom_point(size = 2,
             color = "#999999",
             alpha = 0.5) + 
  geom_smooth(method = lm,
              formula = y ~ x,
              se = FALSE) +
  scale_y_continuous(name = "Dollars per gallon") +
  scale_x_continuous(name = "Consumption, Index 2012") +
  theme_wsj() +
  coord_cartesian(clip = "off") +
  theme(panel.border = element_rect(colour = "black", 
                                    fill = NA, 
                                    size = 0.4),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# PLOT 2
ggplot(data = data,
       aes(x = Demand,
           y = Price)) +
  geom_point(size = 2,
             color = "#999999",
             alpha = 0.5) + 
  geom_smooth(method = loess,
              formula = y ~ x,
              se = FALSE) +
  scale_y_continuous(name = "Dollars per gallon") +
  scale_x_continuous(name = "Consumption, Index 2012") +
  theme_wsj() +
  coord_cartesian(clip = "off") +
  theme(panel.border = element_rect(colour = "black", 
                                    fill = NA, 
                                    size = 0.4),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# PLOT 3
ggplot(data = data,
       aes(x = Demand,
           y = Price)) +
  geom_point(size = 2,
             color = "#999999",
             alpha = 0.5) + 
  geom_smooth(method = loess,                          #LOESS FIT: NONPARAMETRIC FIT
              formula = y ~ x,
              se = FALSE,
              aes(colour = "A")) +
  geom_smooth(method = lm,
              formula = y ~ x,                         #LINEAR FIT
              se = FALSE,
              aes(colour = "B")) +
  geom_smooth(method = lm,
              formula = y ~ x + I(x^2),                #QUADRATIC FIT
              se = FALSE,
              aes(colour = "C")) +
  geom_smooth(method = lm,
              formula = y ~ x + I(x^2) + I(x^3),       #CUBIC FIT
              se = FALSE,
              aes(colour = "D")) +
  geom_smooth(method = lm,
              formula = y ~ x + I(x^2) + sin(x),       #CUBIC-SINE FIT
              se = FALSE,
              aes(colour = "E")) +
  scale_y_continuous(name = "Dollars per gallon") +
  scale_x_continuous(name = "Consumption, Index 2012") +
  theme_wsj() +
  coord_cartesian(clip = "off") +
  scale_colour_manual(name = "", 
                      labels = c("Smoothing", 
                                 "Linear",
                                 "Quadratic", 
                                 "Cubic",
                                 "Cubic-Sine"),
                      values = c("blue",
                                 "purple",
                                 "#FC4E07", 
                                 "#E7B800", 
                                 "#33CC99")) +
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        panel.border = element_rect(colour = "black", 
                                    fill = NA, 
                                    size = 0.4),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

