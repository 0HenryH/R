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
      #add the title and set it in the middle
       +labs(title = "Vancouver")+theme(plot.title=element_text(hjust=0.5))


# Add geom_point() and geom_smooth() with +
windows()
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + geom_smooth() 
#can be only smooth funcion
#alpha = 0.4 in aes color = ~ set the transparenty and color

# +geom_smooth(aes(group = 1), method = "lm", se = FALSE, linetype = 2)
#use the linear model, overall fit, no shadow, and virtual line

ggplot(diamonds,aes(x = ,y = ,col = ,shape = ,size = ,fill = ,label=)) #label + geom_text() instead of point(),

#to adjust the settings on axis x
scale_x_continuous("name",limits = c(2,8),breaks = seq(2,8,3),expand = c(0,1))

+ labs(x="sepal Length",y = , col = ,title = )                 
#if overplotted, we can adjust the size and the shape to show more details

ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) +
  geom_jitter() +
  facet_grid(. ~ Measure)
#facet grid