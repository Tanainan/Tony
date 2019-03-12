library(ggplot2)


dt <- data.frame(x = c(2, 8),
                 y = c(8, 2),
                 name = c("A","B"))

#only 2 choices
ggplot(dt, aes(x,y)) + 
  geom_point(size = 3) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), vjust = -2) + 
  labs(x = "Attribute 2", y = "Attribute 1") 

  
#similarity effect
dt1 <- data.frame(x = c(2, 8, 7.6),
                 y = c(8, 2, 1.7),
                 name = c("A","B","B'"))
ggplot(dt1, aes(x,y)) + 
  geom_point(size = 3) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), vjust = -2) + 
  labs(x = "Attribute 2", y = "Attribute 1") 


#perceptual focus effect
dt1 <- data.frame(x = c(2, 2.5, 5, 7.5, 8),
                  y = c(2, 2, 8, 2, 2),
                  name = c("D", "E", "A", "C", "B"))
ggplot(dt1, aes(x,y)) + 
  geom_point(size = 3) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), vjust = -2) + 
  labs(x = "Attribute 2", y = "Attribute 1") 



#outlier effect
dt1 <- data.frame(x = c(2, 8, 7.6, 8),
                  y = c(8, 2, 1.7, 1.7),
                  name = c("A","B","B'","B'"))
ggplot(dt1, aes(x,y)) + 
  geom_point(size = 3) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), vjust = -2) + 
  labs(x = "Attribute 2", y = "Attribute 1") 


# 2 choices with lines and shades
ggplot(dt, aes(x,y)) + 
  geom_point(size = 3) +
  theme_bw() + 
  scale_x_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  scale_y_continuous(limits = c(0.5,10), breaks = c(2,4,6,8,10)) +
  geom_text(aes(label = name), vjust = -2) + 
  labs(x = "Attribute 2", y = "Attribute 1") +
  geom_abline(intercept = 10, slope = -1) + 
  geom_segment(y = 0, yend = 8, x = 2, xend = 2) +
  geom_segment(y = 2, yend = 2, x = 0, xend = 8) + 
  geom_segment(y = 8, yend = 8, x = 0, xend = 2) + 
  geom_segment(y = 0, yend = 2, x = 8, xend = 8) +
  #geom_text(x=8.5, y=0.5, label="Y", size = 6)
  geom_rect(xmin = 2, xmax = 8, ymin = 0, ymax = 2, fill = "gray", alpha = 0.2) +
  geom_text(x=5, y=1, label="X", size = 6) 

