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
  geom_text(aes(label = name), vjust = -2, size = 5) + 
  labs(x = "Attribute 2", y = "Attribute 1", title = "Two Options in Two-attributes Context") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))

  
#similarity effect
dt1 <- data.frame(x = c(2, 8, 7.7),
                 y = c(8, 2, 2.3),
                 name = c("A","B","B'"))
b <- ggplot(dt1, aes(x,y)) + 
  geom_point(size = 1) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), hjust = - 0.6, size = 5, vjust = -1) + 
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
  geom_text(aes(label = name), vjust = -2, size = 5) + 
  labs(x = "Attribute 2", y = "Attribute 1", title = "Perceptual Focus Effect") +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 



#outlier effect
dt3 <- data.frame(x = c(2,6.8,7,7.2,7.4,7.6,7.8,8),
                  y = c(8,3.2,3,2.8,2.6,2.4,2.2,2),
                  name = c("A","","","B'","","","","B"))
e <- ggplot(dt3, aes(x,y)) + 
  geom_point(size = 1) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), hjust = - 0.6, size = 5, vjust = -1) + 
  labs(x = "Attribute 2", y = "Attribute 1", title = "Current Study") +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) 


# 2 choices with lines and shades
c <- ggplot(dt, aes(x,y)) + 
  geom_point(size = 1) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), vjust = -2, size = 5) + 
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

grid.arrange(b,e, ncol = 1)



# shift weighting

ggplot(dt3, aes(x,y)) + 
  geom_point(size = 1) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), hjust = - 0.6, size = 5, vjust = -1) + 
  labs(x = "Attribute 2", y = "Attribute 1", title = "Dimensional Weight Model in Current Study") +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  geom_abline(intercept = 10, slope = -1, size = 0.1) +
  geom_segment(y = 0, yend = 10, x = 0, xend = 10, size = 0.02) +
  geom_segment(y = 4.8, yend = 5, x = 4.8, xend = 4.6, size = 0.02) +
  geom_segment(y = 5, yend = 5.2, x = 4.6, xend = 4.8, size = 0.02) +
  geom_segment(y = 9.7, yend = 10, x = 10, xend = 10, size = 0.02) +
  geom_segment(y = 10, yend = 10, x = 9.7, xend = 10, size = 0.02) +
  geom_segment(y = 0, yend = 9, x = 0, xend = 10.5, size = 0.05) +
  geom_segment(y = 10.5, yend = 0, x = 1, xend = 9, size = 0.05) +
  geom_segment(y = 4.45, yend = 4.7, x = 5.2, xend = 5, size = 0.05) +
  geom_segment(y = 4.7, yend = 4.9, x = 5, xend = 5.25, size = 0.05) +
  geom_segment(y = 8.7, yend = 9, x = 10.45, xend = 10.45, size = 0.08) +
  geom_segment(y = 9, yend = 9, x = 10.2, xend = 10.5, size = 0.05) +
  geom_segment(y = 8.6, yend = 8.2, x = 8.8, xend = 9.3, size = 1) +
  geom_segment(y = 8.2, yend = 8.4, x = 9.3, xend = 9.3, size = 1) +
  geom_segment(y = 8.2, yend = 8.2, x = 9.1, xend = 9.3, size = 1) 
  

# figure 3
dt4 <- data.frame(x = c(2,6.8,7,7.2,7.4,7.6,7.8,8),
                  y = c(8,3.2,3,2.8,2.6,2.4,2.2,2),
                  z = c(1,1,2,2,2,2,2,1),
                  name = c("A","B'","","","","","","B"))
ggplot(dt4, aes(x,y, color = as.factor(z))) + 
  geom_point(size = 1) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), hjust = - 0.6, size = 5, vjust = -1) + 
  labs(x = "Attribute 2", y = "Attribute 1") +
  geom_abline(intercept = 10, slope = -1, size = 0.1) +
  theme(plot.title = element_text(size = 12, hjust = 0.5)) +
  scale_color_manual(values = c("#000000", "#CCCCCC")) +
  theme(legend.position = "none")
  
  
  
