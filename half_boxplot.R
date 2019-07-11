# R script by Frank Soboczenski, PhD 
# email: frank.soboczenski@kcl.ac.uk
# twitter: h21k

# Small script that checks if the required packages are installed if not it will install them !

requiredPackages = c('ggplot2','ggthemes')
for(a in requiredPackages){
  if(!require(a, character.only = TRUE)) install.packages(a)
  library(a,character.only = TRUE)
}

#loading the libraries
library(ggplot2);library(ggthemes)

#data source
data(iris)

#plotting the plot :) 
ggplot(iris) +
  theme_stata() +
  theme(line = element_blank()) + 
  stat_boxplot(aes(x = Species, y = Sepal.Length), geom='errorbar', linetype=1, width=0.2) + 
  geom_boxplot(aes(x = Species, y = Sepal.Length, fill=Species),
               alpha = 1, size = 0.75, width = 0.25, outlier.shape = 3) + 
  annotate("rect", xmin = 1, xmax = 1.5, ymin = 0, ymax = 8, alpha = 1, fill = 'white') +
  annotate("rect", xmin = 2, xmax = 2.5, ymin = 0, ymax = 8, alpha = 1, fill = 'white') +
  annotate("rect", xmin = 3, xmax = 3.5, ymin = 0, ymax = 8, alpha = 1, fill = 'white') +
  geom_point(aes(x = as.numeric(Species) + 0.1, colour = Species, y = Sepal.Length),
             alpha = 0.5, position = position_jitter(width = 0.1))

#Notes

#outlier.shape = 3 --> display outliers as crosses
#