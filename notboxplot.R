# author: Denis O'Hora

# inspired by notBoxPlot by raacampbell
# https://github.com/raacampbell/notBoxPlot

# see also https://helenajambor.wordpress.com/2018/08/28/pick-n-mix-plots/

# Purpose
# An alternative to a box plot, where the focus is on showing raw
# data. Plots columns of y as different groups located at points
# along the x axis defined by the optional vector x. Points are
# layed over a 1.96 SEM (95# confidence interval) in red and a 1 SD
# in blue. The user has the option of plotting the SEM and SD as a
# line rather than area. Raw data are jittered along x for clarity. This
# function is suited to displaying data which are normally distributed.
# Since, for instance, the SEM is meaningless if the data are bimodally
# distributed. 

notboxplot <- function(data, x, y, 
                       width = .15){
  require(ggplot2)
  data$points_x <- as.numeric(data[,x])
  data$points_y <- data[,y]
  
  # used ave instead of ddply (tricky to deal with x and y)
  data_summary <- data.frame(unique(data[,x]),
                        unique(ave(data[,y], data[,x])),
                        unique(ave(data[,y], data[,x], 
                                   FUN = function(x) sd(x) / sqrt(length(x)) * 1.96)),
                        unique(ave(data[,y], data[,x], 
                                   FUN = function(x) sd(x) )) 
                        )
  
  colnames(data_summary) = c( x, "mean", "CI", "sd")

  data_summary$mean_x <- as.numeric(data_summary[,x]) - width
  data_summary$mean_xend <- as.numeric(data_summary$Species) + width
  data_summary$mean_y <- data_summary$mean
  data_summary$mean_yend <- data_summary$mean
  
  data_summary$rect_xmin = as.numeric(data_summary$Species) - width
  data_summary$rect_xmax = as.numeric(data_summary$Species) + width
  
  data_summary$CI_ymin <- data_summary$mean - data_summary$CI
  data_summary$CI_ymax <- data_summary$mean + data_summary$CI

  data_summary$sd_ymin <- data_summary$mean - data_summary$sd
  data_summary$sd_ymax <- data_summary$mean + data_summary$sd
  
  # set up plot 
  hold_plot <- ggplot(data_summary) + theme_bw() +
    # mean
    geom_segment(aes_(x = ~mean_x,
                  xend = ~mean_xend,
                  y = ~mean_y,
                  yend = ~mean_yend,
                  colour = as.name(x) )
                 ) +
    # CI
    geom_rect(aes_(xmin = ~rect_xmin,
                  xmax = ~rect_xmax,
                  ymin = ~CI_ymin,
                  ymax = ~CI_ymax,
                  fill = as.name(x) ),
                  alpha = .5) +
    # SD
    geom_rect(aes_(xmin = ~rect_xmin,
                  xmax = ~rect_xmax,
                  ymin = ~sd_ymin,
                  ymax = ~sd_ymax,
                  fill = as.name(x) ),
                  alpha = .3 ) +

    geom_point(data = data,
               aes_(x = ~points_x, fill = as.name(x), y = ~points_y),
               shape = 21, alpha = 0.5, position = position_jitter(width = 0.1))
    
    # tidy the labels
    hold_plot <- hold_plot + labs(x = x, y = y) 
  
    return(hold_plot)
}

notboxplot(iris, x = "Species", y = "Sepal.Length")
nbp <- notboxplot(iris, x = "Species", y = "Sepal.Width")
nbp
