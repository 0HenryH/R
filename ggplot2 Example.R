# A scatter plot has been made for you
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# Replace ___ with the correct column
windows()
ggplot(mtcars, aes(x = wt, y = mpg, color = mtcars$disp)) +
  geom_point()

# Replace ___ with the correct column
windows()
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point()

# Explore the diamonds data frame with str()
str(diamonds)

# Add geom_point() with +
windows()
ggplot(diamonds, aes(x = carat, y = price)) + geom_point()


# Add geom_point() and geom_smooth() with +
windows()
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + geom_smooth()