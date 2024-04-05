############################ Functions ############################ 
## This a file which will store all the functions which are      ##
## redundant in our different data files.               n        ##
###################################################################

########################### Libraries ############################

library(plotly)
library(RColorBrewer)
library(dplyr)
library(tibble)
library(tidyverse)



########################## Load the data ##########################

import_df <- function(file_name){
  
  df <- read.csv(file_name)
  
  return(df)
}

########################## Plot by gender #########################


plot_by_gender <- function(data, variable) {
  
  # calculate mean by gender
  p <- aggregate(reformulate("Gender", variable), data, mean)
  
  # create histogram
  hist <- plot_ly(data, x = ~get(variable), color = ~Gender, type = "histogram",
                  histnorm = 'probability density', nbinsx = 20, 
                  marker = list(opacity = 0.6,
                                line = list(width = 1, color = '#000000')),
                  colors = c('#508B8D', '#F3D6CB'),
                  legendgroup = ~Gender,
                  nbinsx = 20) %>%
    layout(title = paste0("Distribution of ", variable, " by Gender"),
           xaxis = list(title = variable),
           yaxis = list(title = "Probability Density"),
           legend = list(orientation = "h",
                         yanchor = "bottom",
                         xanchor = "right",
                         y = 1.03,
                         x = .97),
           barmode = "overlay")
  
  # create bar plot
  bar <- plot_ly(p, x = ~Gender, y = ~get(variable), type = "bar",
                 text = ~paste(variable, ": ", round(get(variable), 0)),
                 textposition = 'auto',
                 marker = list(color = c("#F3D6CB", "#508B8D"), opacity = 0.6, width = 0.8),
                 hovertemplate = paste0("Average ", variable, " Among %{x} = %{y} <extra></extra>")
                 ) %>%
          layout(title = paste0("Average ", variable),
                xaxis = list(title = "Gender"),
                yaxis = list(title = variable))
  
  return(list(hist = hist, bar = bar))
}

########################## Plot by Profession #########################


plot_by_profession <- function(data, variable, Profession1, Profession2) {
  
  datafiltered <- data %>% filter(Profession %in% c(Profession1, Profession2))
  
  # calculate mean by Profession :
  p <- aggregate(datafiltered[paste0(variable)], datafiltered["Profession"], mean)
  
  # create histogram
  hist <- plot_ly(datafiltered,
                  x = ~get(variable),
                  color = ~Profession,
                  type = "histogram",
                  colors = c("#F3D6CB", "#508B8D"),
                  marker = list(opacity = 0.6,
                                line = list(width = 1, color = '#000000')),
                  nbinsx = 20) %>%

    layout(title = paste0("Distribution of ", variable, " by profession"),
           xaxis = list(title = variable),
           yaxis = list(title = "Probability Density"),
           legend = list(orientation = "h",
                         yanchor = "bottom",
                         xanchor = "right",
                         y = 1.03,
                         x = .97),
           barmode = "overlay")
  
  # create bar plot
  bar <- plot_ly(p, x = ~Profession, y = as.formula(paste0("~",variable)), type = "bar", 
                 text = ~paste(variable, ": ", round(get(variable))), 
                 marker = list(color = c("#F3D6CB", "#508B8D"), opacity = 0.6, width = 0.8),
                 hovertemplate = paste0("Average ", variable, " among %{x} = %{y} <extra></extra> people")) %>%
    layout(title = paste0("Average ", variable),
           xaxis = list(title = "Profession"),
           yaxis = list(title = variable))
  
  return(list(hist = hist, bar = bar))
}


############################### Plot 3D ##############################


plot_3d_Clusters <- function(data, label,order){
  
  # Determine the number of clusters
  num_clusters <- length(unique(data[, label]))
  

  
  # Create a list of colors using the RColorBrewer package
  colors <- brewer.pal(n = num_clusters, name = "Set1")
  
  # Create the plot
  plot_3d <- plot_ly(data,
                     x = ~ Income,
                     y = ~ SpendingScore,
                     z = ~ Age,
                     type = "scatter3d",
                     mode = "markers",
                     color = ~ as.factor(data[, label]),
                     colors = colors,
                     marker = list(size = 3, symbol = "circle")
  )
  
  return(plot_3d)
}



######################### Gender Repartition #########################

Function_gender_repartition <- function(data,labels_name){
  
  # First, group the data frame by 'Cluster' and count the frequency of each 'Gender' value.
  data_cg <-  table(factor(data[[labels_name]],
                           levels = unique(data[[labels_name]])), data$Gender)
  data_cg <- as.data.frame(data_cg)
  
  # Next, unstack the table to obtain a separate count for each gender in each cluster.
  data_cg <- pivot_wider(data_cg, names_from = Var2, values_from = Freq)
  
  
  # Calculate the ratio of each gender in each cluster.
  data_cg$RatioMen <- (data_cg$Male / rowSums(data_cg[,2:3])) * 100
  data_cg$RatioWomen <- (data_cg$Female / rowSums(data_cg[,2:3])) * 100

  
  fig_gender_repartition <- plot_ly(data_cg, 
                                    x = ~RatioMen,
                                    y = ~Var1,
                                    name = 'Men',
                                    type = "bar",
                                    marker = list(opacity = 0.5,
                                                  line = list(width = 1,
                                                              color = '#000000')),
                                    marker = list(color = '#508B8D'),
                                    barmode = 'stack',
                                    barwidth = 0.4,
                                    text = paste(round(data_cg$RatioMen, 2), "%"),
                                    orientation = "h"
  ) %>%
    add_trace(x = ~RatioWomen, 
              name = 'Women',
              marker = list(color = '#F3D6CB'),
              barwidth = 0.4,
              text = paste(round(data_cg$RatioWomen, 2), "%"),
              orientation = "h") 
  
  
  
  fig_gender_repartition <- fig_gender_repartition %>% 
    layout(title = list(text = "Gender distribution by cluster",
                        font = list(size = 15,
                                    color = "black",
                                    family = "serif"),
                        x = 0.98,
                        y = 0.98),
           xaxis = list(title = "Repartition (%)"), # Modify the x-axis title
           yaxis = list(title = "Cluster"), # Set the y-axis title
           barmode = 'stack',
           # Modify the tickformat for the x-axis to display percentage sign.
           xaxis = list(tickformat = "%")
    )
  
  
  return(fig_gender_repartition)
}

####################### Comparative Clustering ########################



Function_comparative_cluster <- function(df, label, Cluster1, Cluster2) {
  
  # Subset the data frame and calculate the mean of each variable by cluster
  comparative_df <- df %>%
    select(!!sym(label), Age, Income, SpendingScore, FamilySize) %>%
    group_by(!!sym(label)) %>%
    summarize(across(Age:FamilySize, mean)) %>%
    mutate(Income = Income / 1000) %>%
    pivot_longer(cols = -!!sym(label),
                 names_to = "variable",
                 values_to = "value") %>%
    spread(key = !!sym(label), value = value)  %>%
    select(!!sym("variable"), !!sym(Cluster1), !!sym(Cluster2))
    
  
  
  # Create a grouped bar chart using plot_ly
  fig_comparative <- plot_ly(comparative_df,
                             x = ~variable,
                             type = "bar",
                             y = as.formula(paste0("~", comparative_df[2])),
                             barmode = "overlay",
                             bargap = 0.2,
                             barwidth = 0.4,
                             hovertemplate = paste('%{x}: %{y:.2f}<extra></extra>'),
                             name = names(comparative_df[2]),
                             marker = list(opacity = 0.5,
                                           line = list(width = 1, 
                                                       color = '#000000')),
                             marker = list(color = '#508B8D')) %>%
    add_trace(y = as.formula(paste0("~", comparative_df[3])),
              name = names(comparative_df[3]),
              marker = list(color = '#F3D6CB'))
  
  # Set x and y axis labels using layout
  fig_comparative <- fig_comparative %>% layout(xaxis = list(title = "Variable"),
                                                yaxis = list(title = "Value"))
  
  return(fig_comparative)
}
