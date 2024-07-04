library(tidyverse)
ObjectInterest1 <- read_csv("240704_r1452_0_baseline_object-interest.csv")
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
#Add ROI one onto the scatterplot
install.packages("ggforce")
library(ggforce)
p1circle100 <- p1 + geom_circle(aes(x0 = 309, y0 = 705, r = 100), color = "blue")
p1circle80 <- p1 + geom_circle(aes(x0 = 309, y0 = 705, r = 80), color = "blue")
p1circle130 <- p1 + geom_circle(aes(x0 = 309, y0 = 705, r = 130), color = "blue")
p1_100 <- p1circle100 + geom_circle(aes(x0 = 793, y0 = 256, r = 100), color = "red")
p1_80 <- p1circle80 + geom_circle(aes(x0 = 793, y0 = 256, r = 80), color = "red")
p1_130 <- p1circle130 + geom_circle(aes(x0 = 793, y0 = 256, r = 130), color = "red")
p1_100
p1_130
library(gridExtra)
grid.arrange(p1_100, p1_130)
#Let's try the above code on another video file.
ObjectInterest2 <- read_csv("240704_r1452_1_baseline_object-interest.csv")
ObjectInterest2 <- ObjectInterest2 %>%
  select(!15:24)
ObjectInterest2 <- ObjectInterest2 %>%
  filter(Value.Item1.Item2>260)
ObjectInterest2 <- ObjectInterest2 %>%
  rename(pixelx = Value.Item1.Item3.X, pixely = Value.Item1.Item3.Y, TimePoint = Value.Item1.Item2)
ObjectInterest2 <- ObjectInterest2 %>%
  select(!1)
ObjectInterest2 <- ObjectInterest2 %>%
  select(!14)
x1 <- c(ObjectInterest2$pixelx)
y1 <- c(ObjectInterest2$pixely)
df1 <- data.frame(x = x1, y = y1)
p2 <- ggplot(df, aes(x = x1, y = y1)) +
  geom_point(alpha = 0.3)
p2
p2circle130 <- p2 + geom_circle(aes(x0 = 309, y0 = 705, r = 130), color = "blue")
p2_130 <- p2circle130 + geom_circle(aes(x0 = 793, y0 = 256, r = 130), color = "red")
p2_130
grid.arrange(p1_130, p2_130)
#Let's add another video to try it out.
ObjectInterest3 <- read_csv("240704_r1452_2_baseline_object-interest.csv")
ObjectInterest3 <- ObjectInterest3 %>%
  select(!15:24)
ObjectInterest3 <- ObjectInterest3 %>%
  filter(Value.Item1.Item2>260)
ObjectInterest3 <- ObjectInterest3 %>%
  rename(pixelx = Value.Item1.Item3.X, pixely = Value.Item1.Item3.Y, TimePoint = Value.Item1.Item2)
ObjectInterest3 <- ObjectInterest3 %>%
  select(!1)
ObjectInterest3 <- ObjectInterest3 %>%
  select(!14)
x2 <- c(ObjectInterest3$pixelx)
y2 <- c(ObjectInterest3$pixely)
df2 <- data.frame(x = x2, y = y2)
p3 <- ggplot(df2, aes(x = x2, y = y2)) +
  geom_point(alpha = 0.3)
p3
p3circle130 <- p3 + geom_circle(aes(x0 = 309, y0 = 705, r = 130), color = "blue")
p3_130 <- p3circle130 + geom_circle(aes(x0 = 793, y0 = 256, r = 130), color = "red")
p3_130