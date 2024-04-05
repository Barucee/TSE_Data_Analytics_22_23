########################### Rshiny App ########################### 
## This a file constructed thank to RShiny package in order to  ##
## share to the shareholders our different visualizations       ##
##################################################################

########################### Libraries ############################

source("Functions.R")
library(shiny)
library(parallel)
library(dplyr)
library(plotly)
library(RColorBrewer)



######################### Load the data ##########################

path <- '../Data/dfClusters.csv'

cl <- makeCluster(detectCores())

df <- parLapply(cl, path, import_df)

stopCluster(cl)
df <- bind_rows(df)

order <- c("Target - Old",
           "Target - Young",
           "Most valuable - Young, Rich",
           "Most valuable - Old, Rich",
           "Valuable - Old, 'Poor' ",
           "Valuable - Young, 'Poor'",
           "Less valuable - Old",
           "Less valuable - Young",
           "Least valuable")

#################### Creation of the ShinyApp ####################

# Define UI :
ui <- shinyUI(
  navbarPage("App Shop Cluster",
             
             tabPanel(
               "Introduction ",
               fluidRow(
                 column(12, h2("Introduction")),
                 column(12, p("Here is your app which will allow you to compare and understand the data of your shop.")),
                 column(12, p("You will find different slide which will allow you to understand different characteristics of your
                              customers. Furthermore, you will have the output of our clustering ML model in  order to know at which
                              group they belong.")),
                 column(12, HTML("<ul> 
                     <li> Descriptive statistics by gender : Display distribution of your choosen variable segmented by the gender.
                     Moreover, you will be able to watch the average of your variable choosen for both gender </li> 
                     <li> Descriptive statistics by profession : Display distribution of your choosen variable segmented by the 
                     profession choosen. Besides, you will be able to watch the average of your variable choosen for the profession
                     selected.</li> 
                     <li> 3D Plot : Here, is an interactive 3D plot displaying the cluster depending on 3 variables : Age,
                     Spending Score and Income</li> 
                     <li> Cluster gender Repartition : You will be able to visualize the repartion of the gender for each 
                     cluster </li> 
                     <li> Cluster mean of variable : This slide will allow you to have the mean of each variable for the clusters
                     selected.</li> 
                     <li> Conclusion : Finally, we will give you some advice for your advertisement campaign. </li> 
                     </ul>")),
                 column(12, p("Best regards,")),
                 column(12, p("Ravey & Clouet Data Consulting Team"))
               )
             ),
               
             tabPanel(
               "Descriptive statistics by gender",
               sidebarLayout(
                 sidebarPanel(
                   selectizeInput(
                     "Variable1", label="Variable",
                     choices= names(df[,3:8]),
                     multiple=F,
                     selected= NULL,
                     options = list(create = TRUE,
                                    placeholder = 'Choose the variable')
                   ),
                   selectizeInput(
                     'Typeofgraph', label="Type of graph",
                     choices= c('Histogram per gender','Bar average per gender'),
                     multiple=F,
                     selected= NULL,
                     options = list(create = TRUE,
                                    placeholder = 'Choose the graph')
                   )
                 ),
                 mainPanel(plotlyOutput("GenderPlot"))
               )
             ),
             
             tabPanel(
               "Descriptive statistics by profession",
               sidebarLayout(
                 sidebarPanel(
                   selectizeInput(
                     "Variable2", label="Variable",
                     choices= names(df[,3:8]),
                     multiple=F,
                     selected= NULL,
                     options = list(create = TRUE,
                                    placeholder = 'Choose the variable')
                   ),
                   selectizeInput(
                     "Profession1", label="Profession 1",
                     choices= unique(df$Profession),
                     multiple=F,
                     selected= "Healthcare",
                     options = list(create = TRUE,
                                    placeholder = 'Choose the profession 1')
                   ),
                   selectizeInput(
                     "Profession2", label="Profession 2",
                     choices= unique(df$Profession),
                     multiple=F,
                     selected= "Lawyer",
                     options = list(create = TRUE,
                                    placeholder = 'Choose the profession 2')
                   ),
                   selectizeInput(
                     'TypeofgraphProfession', label="Type of graph",
                     choices= c("Histogram per profession",'Bar average per profession'),
                     multiple=F,
                     selected= NULL,
                     options = list(create = TRUE,
                                    placeholder = 'Choose the graph')
                   )
                 ),
                 mainPanel(plotlyOutput("Genderprofession"))
               )
             ),
             
             tabPanel(
               "3D plot",
               mainPanel(plotlyOutput("threeDplot")),
             ),  
             
             tabPanel(
               "Cluster gender repartition",
               mainPanel(plotlyOutput("ClusterGender"))
             ),
             
             tabPanel(
               "Cluster mean of variable",
               sidebarLayout(
                 sidebarPanel(
                   selectizeInput(
                     "Cluster1", label="Cluster 1",
                     choices= order,
                     multiple=F,
                     selected= "Target - Young",
                     options = list(create = TRUE,
                                    placeholder = 'Choose the cluster')
                   ),
                   selectizeInput(
                     "Cluster2", label="Cluster 2",
                     choices= order,
                     multiple=F,
                     selected= "Least valuable",
                     options = list(create = TRUE,
                                    placeholder = 'Choose the cluster')
                   )
                 ),
                 mainPanel(plotlyOutput("ClusterMeanVariable"))
               )
             ),
             tabPanel(
               "Conclusion",
               fluidRow(
                 column(12, h2("Conclusion")),
                 column(12, p("As you have been able to see, we have given names to cluster to allow you to quickly 
                              pinpoint which kind of cluster it is. We will give you some advice of advertisement")),
                 column(12, HTML("<ul> 
                     <li> Least valuable, Less valuable - Young, Less valuable - old are clusters which have a low spending score.
                     Moreover, comparatively to the other cluster their income is low, particularly the least valuable income. It
                     is not necessary to advertise them a lot if they do not have the income, they could simply take your mail or message
                     as spam and do not come anymore. However, it can be interesting to give voucher per mail or advertise family product to 
                     the 'Less valuable - Young' which have bigger family. Playing with the quantity sold to them, and not on the direct margin
                     can be a good move.</li> 
                     <li> Valuable and most valuable are cluster which are doubled. Each of these clusters exist with young and old people.
                     Each of them has already high spending score, so advertisement to have more buying is not the aim. However,
                     it can be interesting to target the type of people they are, if you do sms/mail campaign. Indeed, Valuable - Old and
                     Most valuable - Young have an higher number of family number. It is may be not a good idea to advertise them family
                     promotion, because they would buy anyway. Nevertheless, it can be interesting to propose them family product depending
                     on which product you sell to keep them aware that you sell these products.</li> 
                     <li> Target - Old and Target young are the most interesting. They have a big income but a low spending score. Following,
                     the other analytic survey we gave you 2 weeks ago in Industrial Organization of your store, you suffer a lot from competition
                     of another store. Here, you do not have to advertise promotion/give voucher or other because they have money to spend.
                     The goal of your sms/mail campaign will be to emphasize the quality of your product compared to your competitor
                     and the brand name of your shop.</li> 

                     </ul>")),
                 column(12, p("If you have any feedback or suggestions for improvement, please feel free to contact us.")),
                 column(12, p("Best regards,")),
                 column(12, p("Ravey & Clouet Data Consulting Team")))
             )
             )
  )
             



# Define server logic to plot
server <- function(input, output) {


  output$GenderPlot <- renderPlotly({
    
    # Get selected variable and type of graph
    variable <- input$Variable1
    graphType <- input$Typeofgraph
    
    # Check if both input variables are selected
    if (is.null(variable) || is.null(graphType)) {
      return(NULL)
    }
    
    # Call the function plot_by_gender with the selected variable
    plot_gender <- plot_by_gender(df, variable)
    
    # Return the selected type of graph
    if (graphType == "Histogram per gender") {
      return(plot_gender$hist)
    } else {
      return(plot_gender$bar)
    }
  })
  
  output$Genderprofession <- renderPlotly({
    
    # Get selected variable, professions, and type of graph
    variable <- input$Variable2
    profession1 <- input$Profession1
    profession2 <- input$Profession2
    graphType <- input$TypeofgraphProfession
    
    # Check if all input variables are selected
    if (is.null(variable) || is.null(profession1) || is.null(profession2) || is.null(graphType)) {
      return(NULL)
    }
    
    # Call the function plot_by_profession with the selected variables
    plots_profession <- plot_by_profession(df, variable, profession1, profession2)
    
    # Return the selected type of graph
    if (graphType == "Histogram per profession") {
      return(plots_profession$hist)
    } else {
      return(plots_profession$bar)
    }
  })
  
  output$threeDplot <- renderPlotly({
    plot_3d_Clusters(df, "Cluster_Kmeans_label_name")
  })
  
  output$ClusterGender <- renderPlotly({
    Function_gender_repartition(df,"Cluster_Kmeans_label_name")
  })
  
  output$ClusterMeanVariable <- renderPlotly({
    
    Cluster1 <- input$Cluster1
    Cluster2 <- input$Cluster2
    
    if (is.null(Cluster1) || is.null(Cluster2)) {
      return(NULL)
    }

    Function_comparative_cluster(df,"Cluster_Kmeans_label_name",Cluster1,Cluster2)
    
  
  })
}



shinyApp(ui, server)
