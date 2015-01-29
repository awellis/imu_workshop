
library("ggplot2")
p0 <- ggplot(data.frame(x = c(0, 1)), aes(x)) +
    theme_classic()

p1 <- p0 + stat_function(fun = dbeta, 
                      args = list(shape1 = 1, 
                                  shape2 = 1),
                     color = "blue")
p1

p1 + stat_function(fun = dbeta, 
                   args = list(shape1 = 10, shape2 = 2), 
                   color = 'firebrick')

p2 <- p0 + stat_function(fun = dbeta, 
                   args = list(shape1 = 5, shape2 = 5), 
                   color = 'blue')
p2

p2 + stat_function(fun = dbeta, 
                   args = list(shape1 = 14, shape2 = 6), 
                   color = 'firebrick')
