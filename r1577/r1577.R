rm(list = ls())
library(tidyverse)
ObjectInterest1 <- read_csv("240726_r1577_0_baseline_object-interest.csv")
head(ObjectInterest1)
#To delete the column with ROI 3 and ROI4
ObjectInterest1 <- ObjectInterest1 %>%
  select(!15:24)
#To delete the first 10 seconds of the video
ObjectInterest1 <- ObjectInterest1 %>%
  filter(Value.Item1.Item1.Item2>31)
#Rename the centroid coordinates
ObjectInterest1 <- ObjectInterest1 %>%
  rename(pixelx = Value.Item1.Item1.Item3.X, pixely = Value.Item1.Item1.Item3.Y, TimePoint = Value.Item1.Item1.Item2)
ObjectInterest1 <- ObjectInterest1 %>%
  select(!1)
ObjectInterest1 <- ObjectInterest1 %>%
  select(!15)
ObjectInterest1 <- ObjectInterest1 %>%
  filter(!Value.Item2 == TRUE)
#Now we map the trajectory of the animal in the open field.
x <- c(ObjectInterest1$pixelx)
y <- c(ObjectInterest1$pixely)
df <- data.frame(x = x, y = y)
p1 <- ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.3)
p1
df <- df%>%na.omit
#Add ROI one onto the scatterplot
library(ggforce)
p1circle130 <- p1 + geom_circle(aes(x0 = 125, y0 = 398, r = 75), color = "blue")
p1_130 <- p1circle130 + geom_circle(aes(x0 = 391, y0 = 164, r = 75), color = "red")
p1_130
radius <- 75
center_x1 <- 125
center_y1 <- 398
distance <- sqrt((df$x - center_x1)^2 + (df$y - center_y1)^2)
point_in_circle1 <- sum(distance <= radius)
point_in_circle1
center_x2 <- 391
center_y2 <- 164
df$distance1 <- sqrt((df$x - center_x2)^2 + (df$y - center_y2)^2)
points_in_circle2 <- sum(df$distance1 <= radius)
print(points_in_circle2)

#Let's try the above code on the second video file.
ObjectInterest2 <- read_csv("240726_r1577_1_baseline_object-interest.csv")
head(ObjectInterest2)
#To delete the column with ROI 3 and ROI4
ObjectInterest2 <- ObjectInterest2 %>%
  select(!15:24)
#To delete the first 10 seconds of the video
ObjectInterest2 <- ObjectInterest2 %>%
  filter(Value.Item1.Item1.Item2>45)
#Rename the centroid coordinates
ObjectInterest2 <- ObjectInterest2 %>%
  rename(pixelx = Value.Item1.Item1.Item3.X, pixely = Value.Item1.Item1.Item3.Y, TimePoint = Value.Item1.Item1.Item2)
ObjectInterest2 <- ObjectInterest2 %>%
  select(!1)
ObjectInterest2 <- ObjectInterest2 %>%
  select(!15)
ObjectInterest2 <- ObjectInterest2 %>%
  filter(!Value.Item2 == TRUE)
#Now we map the trajectory of the animal in the open field.
x <- c(ObjectInterest2$pixelx)
y <- c(ObjectInterest2$pixely)
df <- data.frame(x = x, y = y)
p2 <- ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.3)
p2
df <- df%>%na.omit
#Add ROI one onto the scatterplot
library(ggforce)
p2circle130 <- p2 + geom_circle(aes(x0 = 125, y0 = 388, r = 75), color = "blue")
p2_130 <- p2circle130 + geom_circle(aes(x0 = 386, y0 = 159, r = 75), color = "red")
p2_130
radius <- 75
center_x1 <- 125
center_y1 <- 388
distance <- sqrt((df$x - center_x1)^2 + (df$y - center_y1)^2)
point_in_circle1 <- sum(distance <= radius)
point_in_circle1
center_x2 <- 386
center_y2 <- 159
df$distance1 <- sqrt((df$x - center_x2)^2 + (df$y - center_y2)^2)
points_in_circle2 <- sum(df$distance1 <= radius)
print(points_in_circle2)


#Now let's look at the next video
ObjectInterest3 <- read_csv("240726_r1577_2_baseline_object-interest.csv")
head(ObjectInterest3)
#To delete the column with ROI 3 and ROI4
ObjectInterest3 <- ObjectInterest3 %>%
  select(!15:24)
#To delete the first 10 seconds of the video
ObjectInterest3 <- ObjectInterest3 %>%
  filter(Value.Item1.Item1.Item2>40)
#Rename the centroid coordinates
ObjectInterest3 <- ObjectInterest3 %>%
  rename(pixelx = Value.Item1.Item1.Item3.X, pixely = Value.Item1.Item1.Item3.Y, TimePoint = Value.Item1.Item1.Item2)
ObjectInterest3 <- ObjectInterest3 %>%
  select(!1)
ObjectInterest3 <- ObjectInterest3 %>%
  select(!15)
ObjectInterest3 <- ObjectInterest3 %>%
  filter(!Value.Item2 == TRUE)
#Now we map the trajectory of the animal in the open field.
x <- c(ObjectInterest3$pixelx)
y <- c(ObjectInterest3$pixely)
df <- data.frame(x = x, y = y)
p3 <- ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.3)
p3
df <- df%>%na.omit
#Add ROI one onto the scatterplot
library(ggforce)
p3circle130 <- p3 + geom_circle(aes(x0 = 125, y0 = 393, r = 75), color = "blue")
p3_130 <- p3circle130 + geom_circle(aes(x0 = 376, y0 = 159, r = 75), color = "red")
p3_130
radius <- 75
center_x1 <- 125
center_y1 <- 393
distance <- sqrt((df$x - center_x1)^2 + (df$y - center_y1)^2)
point_in_circle1 <- sum(distance <= radius)
point_in_circle1
center_x2 <- 376
center_y2 <- 164
df$distance1 <- sqrt((df$x - center_x2)^2 + (df$y - center_y2)^2)
points_in_circle2 <- sum(df$distance1 <= radius)
print(points_in_circle2)