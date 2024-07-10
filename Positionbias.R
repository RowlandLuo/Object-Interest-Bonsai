library(tidyverse)
PositionBias <- read_csv("Bonsai-IsaRow - Object-Interest.csv")
PositionBias <- PositionBias %>%
  select(19:20)
boxplot(Frames ~ FrameNumber, data = PositionBias, main = "Boxplot of Interest at 2 Positions", xlab = "Positions")
t.test(Frames ~ FrameNumber, data = PositionBias)
#p value is 0.93, which means that there doesn't seem to be a position bias among the four adult rats.


