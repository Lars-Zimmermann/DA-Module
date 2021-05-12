# Data Visualization ####

participants_data <- read.csv("participants_data.csv")


#create a barplot 
participants_barplot <- table(participants_data$gender)

barplot(participants_barplot)



participants_barplot <- table(participants_data$continent_of_origin)

barplot(participants_barplot)



# ggplot2
library(ggplot2)

# names and email
# Create a scatterplot of days to email response (y) 
# as a function of the letters in your first name (x)
names(participants_data)
ggplot(data = participants_data,
       aes(x = letters_in_first_name,
           y = days_to_email_response)) +
  geom_point()




#color and size
# Create a scatterplot of days to email response (y) 
# as a function of the letters in your first name (x) 
# with colors representing binary data 
# related to academic parents (color) 
# and working hours per day as bubble sizes (size).
ggplot(data = participants_data,
       aes(x = letters_in_first_name,
           y = days_to_email_response,
           color = academic_parents,
           size = working_hours_per_day)) +
  geom_point()


ggplot(data = participants_data,
       aes(x = years_of_study,
           y = number_of_publications,
           color = gender,
           size = age)) +
  geom_point()



# Data Visualization 2 ####
library(ggplot2)
library(dplyr)
#iris data
  # Create a scatterplot of iris petal length (y) 
  # as a function of sepal length (x) 
  # with colors representing iris species (color) 
  # and petal width as bubble sizes (size).

ggplot(data = iris,
       aes(x = Sepal.Length, 
           y = Petal.Length,
           color = Species,
           size = Petal.Width)) +
  geom_point()


#diamonds price
  # Create a plot with the diamonds data 
  # of the carat (x) and the price (y)

plot1 <- ggplot(data = diamonds,
                aes(x = carat,
                    y = price,
                    alpha = 0.2)) +
          geom_point()

  # Create a plot with the diamonds data 
  # of the log of carat (x) 
  # and the log of price (y)

ggplot(data = diamonds,
       aes(x = log(carat),
           y = log(price),
           alpha = 0.2)) +
  geom_point()


#color and shapes
  # Create a smaller diamonds data set (top 100 rows), 
  # create a scatterplot with carat on the x-axis 
  # and price on the y-xis and 
  # with the color of the diamond as the color of the points. 
dsmall <- top_n(diamonds, n = 100)

ggplot(data = dsmall, aes(x = carat, 
                          y = price, 
                          color = color)) + 
  geom_point()

 

  # Create a smaller diamonds data set (top 40 rows), 
  # create a scatterplot with carat on the x-axis 
  # and price on the y-xis and 
  # with the cut of the diamond as the shapes for the points. 
dsmall <- top_n(diamonds, n = 40)

ggplot( data = dsmall, 
        aes(x = carat, 
            y = price, 
            shape = cut)) + 
  geom_point()


#set parameters 
  # Create a plot of the diamonds data 
  # with carat on the x-axis, price on the y-axis. 
  # Use the inhibit function to set the alpha to 0.1 
  # and color to blue.
ggplot(data = diamonds, 
       aes(x = carat, 
           y = price, 
           alpha = I(0.1), 
           color = I("blue"))) + 
  geom_point()


#geom options
  # Create a smaller data set of diamonds with 50 rows. 
  # Create a scatterplot and smoothed conditional 
  # means overlay with carat on the x-axis 
  # and price on the y-axis.
dsmall <- top_n(diamonds, 
                n = 50)

ggplot(data = dsmall, 
       aes(x = carat, 
           y = price))+
  geom_point()+
  geom_smooth()

#smooth function
  # Create a smaller data set of diamonds with 50 rows. 
  # Create a scatterplot and smoothed conditional 
  # means overlay with carat on the x-axis 
  # and price on the y-axis.
  # Use 'glm' as the option for the smoothing
dsmall <- top_n(diamonds, 
                n = 50)

ggplot(data = dsmall, 
       aes(x = carat, 
           y = price))+ 
  geom_point()+ 
  geom_smooth(method = 'glm')


#boxplots
  # Change the boxplot so that the x-axis is cut and
  #  the y-axis is price divided by carat
ggplot(data = diamonds, 
       aes(x = cut, 
           y = price/carat)) + 
  geom_boxplot()

#jitter points
  # Change the jittered boxplot so that the x-axis is cut and
  #  the y-axis is price divided by carat
ggplot(data = diamonds, 
       aes(x = cut, 
           y = price/carat)) + 
  geom_boxplot()+ 
  geom_jitter()



#adding alpha 
  # Change the alpha to 0.4 to make 
  # the scatter less transparent
ggplot(data = diamonds, 
       aes(x = color, 
           y = price/carat, 
           alpha = I(0.4))) + 
  geom_boxplot()+ 
  geom_jitter()


#geom histogram
  # Change the density plot so that the x-axis is carat 
  # and the color is the diamond color
ggplot(data = diamonds, 
       aes(x = carat,
           color = color)) +
  geom_density()

  
  # Change the density plot so that the x-axis is carat 
  # the color is the diamond color
  # and the alpha is set to 0.3 using the inhibit function
ggplot(data = diamonds, 
       aes(x = carat, 
           color = color, 
           alpha = I(0.3))) +
  geom_density()




#ggplot subset
  # Create a plot of the mpg data with 
  # manufacturer as the color and a linear model 'lm'
  # as the smooth method
ggplot(data = mpg, 
       aes(x = displ, 
           y = hwy,  
           color = manufacturer)) + 
  geom_point() +
  geom_smooth(method = "lm")




#slow ggplotting
  # Change the title and labels as you see fit
ggplot(mtcars, 
       aes(mpg, 
           y = hp, 
           col = gear)) +
  geom_point() +
  ggtitle("MTCars") +
  labs(x = "mpg", 
       y = "hp", 
       col = "gear")

#example for slow ggplotting
  # Change the title and labels as you see fit  
ggplot(data = mtcars) +
  aes(x = mpg) +
  labs(x = "mpg") +
  aes(y = hp) +
  labs(y = "hp") +
  geom_point() +
  aes(col = gear) +
  labs(col = "gear") +
  labs(title = "MTCars")



#geom_tile
participants_data <- read.csv("participants_data.csv")
  
  # subset the data to numeric only with select_if
part_data <- select_if(participants_data, 
                       is.numeric)
  # use 'cor' to perform pearson correlation
  # use 'round' to reduce correlation 
  # results to 1 decimal
cormat <- round(cor(part_data), 
                digits = 1)
  # use 'as.data.frame.table' to build a table
  # with correlation values
melted_cormat <- as.data.frame.table(cormat, 
                                     responseName = "value")
  # plot the result with 'geom-tile'
ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile()


#export figures ####
png(file = "cortile.png", width = 7, height = 6, units = "in", res = 300)

ggplot(data = melted_cormat, 
       aes(x = Var1, 
           y = Var2, 
           fill = value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(
                        angle = 45, 
                        hjust = 1))

dev.off()



#Test your new skills

#1) Create a scatter plot, barchart and boxplot (as above)

#2) Vary the sample and run the same analysis and plots

#3) Save your most interesting figure and share it with us
