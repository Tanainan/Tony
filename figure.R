library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)


dt <- data.frame(x = c(2, 8),
                 y = c(8, 2),
                 name = c("A","B"))

#only 2 choices
a <- ggplot(dt, aes(x,y)) + 
  geom_point(size = 1) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), vjust = -2, size = 3) + 
  labs(x = "Attribute 2", y = "Attribute 1", title = "Two Options in Two-attributes Context") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))

  
#similarity effect
dt1 <- data.frame(x = c(2, 8, 7.7),
                 y = c(8, 2, 2.3),
                 name = c("A","B","B"))
b <- ggplot(dt1, aes(x,y)) + 
  geom_point(size = 1) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), vjust = -2, size = 3) + 
  labs(x = "Attribute 2", y = "Attribute 1", title = "Similarity Effect") +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 


#perceptual focus effect
dt2 <- data.frame(x = c(2, 2.5, 5, 7.5, 8),
                  y = c(2, 2, 8, 2, 2),
                  name = c("D", "E", "A", "C", "B"))
d <- ggplot(dt2, aes(x,y)) + 
  geom_point(size = 1) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), vjust = -2, size = 3) + 
  labs(x = "Attribute 2", y = "Attribute 1", title = "Perceptual Focus Effect") +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 



#outlier effect
dt3 <- data.frame(x = c(2,6.8,7,7.2,7.4,7.6,7.8,8),
                  y = c(8,3.2,3,2.8,2.6,2.4,2.2,2),
                  name = c("A","","","","Bs","","",""))
e <- ggplot(dt3, aes(x,y)) + 
  geom_point(size = 1) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), vjust = -2, size = 3) + 
  labs(x = "Attribute 2", y = "Attribute 1", title = "With More Than Two Similar Options") +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 


# 2 choices with lines and shades
c <- ggplot(dt, aes(x,y)) + 
  geom_point(size = 1) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), vjust = -2, size = 3) + 
  labs(x = "Attribute 2", y = "Attribute 1", title = "Attraction Effect") +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  geom_abline(intercept = 10, slope = -1, size = 0.3) + 
  geom_segment(y = 0, yend = 8, x = 2, xend = 2, size = 0.3) +
  geom_segment(y = 2, yend = 2, x = 0, xend = 8, size = 0.3) + 
  geom_segment(y = 8, yend = 8, x = 0, xend = 2, size = 0.3) + 
  geom_segment(y = 0, yend = 2, x = 8, xend = 8, size = 0.3) +
  #geom_text(x=8.5, y=0.5, label="Y", size = 6)
  geom_rect(xmin = 2, xmax = 8, ymin = 0, ymax = 2, fill = "gray", alpha = 0.2) +
  geom_text(x=5, y=1, label="X", size = 3) 

grid.arrange(b,c,d,e, ncol = 2)
