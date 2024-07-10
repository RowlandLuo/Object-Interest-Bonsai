rm(list = ls())
library(tidyverse)
ObjectInterest1 <- read_csv("240907_r1451_3_baseline_object-interest.csv")
head(ObjectInterest1)
#To delete the column with ROI 3 and ROI4
ObjectInterest1 <- ObjectInterest1 %>%
  select(!15:24)
#To delete the first 10 seconds of the video
ObjectInterest1 <- ObjectInterest1 %>%
  filter(Value.Item1.Item2>260)
#Rename the centroid coordinates
ObjectInterest1 <- ObjectInterest1 %>%
  rename(pixelx = Value.Item1.Item3.X, pixely = Value.Item1.Item3.Y, TimePoint = Value.Item1.Item2)
ObjectInterest1 <- ObjectInterest1 %>%
  select(!1)
ObjectInterest1 <- ObjectInterest1 %>%
  select(!14)
#Now we map the trajectory of the animal in the open field.
x <- c(ObjectInterest1$pixelx)
y <- c(ObjectInterest1$pixely)
df <- data.frame(x = x, y = y)
p1 <- ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.3)
p1
#Add ROI one onto the scatterplot
library(ggforce)
p1circle130 <- p1 + geom_circle(aes(x0 = 309, y0 = 705, r = 130), color = "blue")
p1_130 <- p1circle130 + geom_circle(aes(x0 = 793, y0 = 236, r = 130), color = "red")
p1_130

radius <- 130
center_x1 <- 309
center_y1 <- 705
df$distance <- sqrt((df$x - center_x1)^2 + (df$y - center_y1)^2)
points_in_circle1 <- sum(df$distance <= radius)
print(points_in_circle1)

center_x2 <- 793
center_y2 <- 236
df$distance1 <- sqrt((df$x - center_x2)^2 + (df$y - center_y2)^2)
points_in_circle2 <- sum(df$distance1 <= radius)
print(points_in_circle2)

