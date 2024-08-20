library(tidyverse)
PositionBias <- read_csv("Bonsai-IsaRow - Object-Interest.csv")
PositionBias <- PositionBias %>%
  select(19:20)
boxplot(Frames ~ FrameNumber, data = PositionBias, main = "Boxplot of Interest at 2 Positions", xlab = "Positions")
t.test(Frames ~ FrameNumber, data = PositionBias)
#p value is 0.93, which means that there doesn't seem to be a position bias among the four adult rats.

ObjectInterest <- read_csv("Bonsai-IsaRow - Object-Interest.csv")
ObjectInterest <- ObjectInterest %>%
  select(21:22) %>%
  na.omit
ObjectInterest <- ObjectInterest %>%
  rename("Discrimination" = "Discrimination Scores")
a1 <- aov(Discrimination ~ Objects, data = ObjectInterest)
summary(a1)
TukeyHSD(a1)
Summary <- ObjectInterest %>%
  group_by(Objects) %>%
  summarise(
    mean = mean(Discrimination),
    sd = sd(Discrimination),
    n = n(),
    se = sd/sqrt(n)
  )
p1 <- ggplot(Summary, aes(x = Objects, y = mean)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(title = "Object Interest Scores",
       x = "Objects", y = "Mean Discrimination Scores") +
  theme_minimal()
p1
