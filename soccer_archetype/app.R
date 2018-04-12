#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(fmsb)
library(cluster)
library(FactoMineR)
library(factoextra)
library(tibble)
library(NbClust)
library(corrplot)
library(DT)
library(RCurl)

path <- getURL("https://raw.githubusercontent.com/bryce-murphy/soccer_analysis/master/asa_soccer.csv")

soccer <- read.csv(text = path, stringsAsFactors = F, header = T)


futbol <- 
  soccer %>%
  select(-First, - Last) %>%
  mutate(Goals_per90 = Goals/Min * 90,
         Shooting_Percentage = SoT/Goals) %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  arrange(desc(Season))

futbol$Player_Year <- paste(futbol$Player, futbol$Season, sep = "_")


# DEFINE UI FOR APP THAT CREATES RADAR PLOT -------------------------------------
ui <- fluidPage(
  
  # Application title
  titlePanel("MLS Athlete Comparisons"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "filter",              # Select variables to filter on for analysis
                  label = "Table Variables:",
                  choices = c("Min", "Goals", "Shots", "SoT", "Dist", "Solo", "Goals_per90", "Shooting_Percentage", "xG", 
                              "xPlace", "G.xG", "KeyP", "Assts", "xA", "A.xA", "xG.xA", "xGperShot", "xAperPass", 
                              "GmxGperShot", "AmxAperPass"),
                  multiple = T,
                  selectize = T,
                  selected = c("Team", "Min", "Goals", "Shots", "SoT", "Assts", "KeyP")
      ),
      checkboxGroupInput(inputId = "years",
                         label = "Select Table Seasons:",
                         choices = c("2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011"),
                         selected = c("2018")
      ),
      numericInput(inputId = "minutes",
                   label = "Minimum Minutes:",
                   value = 0,
                   step = 100),
      numericInput(inputId = "goals",
                   label = "Minimum Goals:",
                   value = 0,
                   step = 5),
      selectInput(inputId = "cluster",
                  label = "Cluster Variables:",
                  choices = c("Min", "Goals", "Shots", "SoT", "Dist", "Solo", "Goals_per90", "Shooting_Percentage", "xG", 
                              "xPlace", "G.xG", "KeyP", "Assts", "xA", "A.xA", "xG.xA", "xGperShot", "xAperPass", 
                              "GmxGperShot", "AmxAperPass"),
                  multiple = T,
                  selectize = T,
                  selected = c("Goals", "Shots", "SoT", "KeyP", "Assts")
      ),
      sliderInput(inputId = "k",
                  label = "Number of Clusters:",
                  min = 2,
                  max = 10,
                  value = 4),
      selectInput(inputId = "players",
                  label = "Select Player(s):",
                  choices = futbol$Player_Year,
                  multiple = T,
                  selectize = T,
                  selected = c("C.J. Sapong_2018", "Christian Ramirez_2018"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      # Create tabs for table, clustering, and PCA
      tabsetPanel(type = "tabs",
                  tabPanel("Table", 
                           p(""),
                           strong("THE CSV FILE SUPPORTING THIS APP WAS RETRIEVED
                                  FROM AMERICANSOCCERANALYSIS.COM. I WANT TO THANK
                                  THEM FOR MAKING THEIR DATA PUBLICLY AVAILABLE!!!"),
                           p(""),
                           DT::dataTableOutput("table")),
                  tabPanel("Cluster",
                           tableOutput("medoids"),
                           DT::dataTableOutput("player_clust"),
                           plotOutput("cluster")),
                  tabPanel("Player Comparison",
                           plotOutput("barplot")),
                  tabPanel("Explanation",
                           plotOutput("PCA"),
                           plotOutput("silhouette"))
      )
    )
  )
)

# DEFINE SERVER LOGIC TO CREATE DT AND CLUSTER --------------------------------------
server <- function(input, output) {
  
  output$table <- DT::renderDataTable({
    
    futbol_filter <- 
      futbol %>%
      select(Player, Season, Team, Min, input$filter) %>%
      filter(Season %in% input$years, 
             Goals >= input$goals,
             Min >= input$minutes) %>%
      arrange(desc(Season))
    
    DT::datatable(futbol_filter, rownames = F,
                  options = list(autoWidth = T))
    
    
  })
  
  output$cluster <- renderPlot({
    
    futbol_analysis <-
      futbol %>%
      select(Season, Player_Year, input$cluster) %>%
      filter(Season %in% input$years,
             Goals >= input$goals,
             complete.cases(.))
    
    futbol_scaled <- scale(futbol_analysis[-c(1:2)])
    
    pam.futbol <- pam(futbol_scaled, input$k)  # SLIDER INPUT TO CHOOSE NUMBER OF CLUSTERS
    
    # CLUSTER GRAPH 
    
    fviz_cluster(pam.futbol, 
                 palette = "Set2", # color palette
                 ellipse.type = "t", # Concentration ellipse
                 repel = TRUE, # Avoid label overplotting (slow)
                 geom = "point",
                 ggtheme = theme_minimal()
    )
    
  })
  
  output$medoids <- renderTable({
    
    futbol_analysis <-
      futbol %>%
      select(Season, Player_Year, input$cluster) %>%
      filter(Season %in% input$years, 
             Goals >= input$goals)
    
    futbol_scaled <- scale(futbol_analysis[, -c(1:2)])
    
    pam.futbol <- pam(futbol_scaled, input$k)  # SLIDER INPUT TO CHOOSE NUMBER OF CLUSTERS
    
    
    
    futbol.df <- as_data_frame(pam.futbol$medoids)
    
    futbol.df$Archetype <- futbol_analysis$Player_Year[pam.futbol$id.med]
    
    futbol.df
    
  })
  
  output$player_clust <- DT::renderDataTable({
    
    
    futbol_analysis <-
      futbol %>%
      select(Season, Player_Year, input$cluster) %>%
      filter(Season %in% input$years,
             Goals >= input$goals)
    
    futbol_scaled <- scale(futbol_analysis[-c(1:2)])
    
    pam.futbol <- pam(futbol_scaled, input$k)  # SLIDER INPUT TO CHOOSE NUMBER OF CLUSTERS
    
    futbol_analysis$Archetype <- pam.futbol$clustering
    
    futbol_table <-
      futbol_analysis %>%
      select(Season, Player_Year, Archetype, input$cluster) %>%
      filter(Season %in% input$years,
             Goals >= input$goals)
    
    
    DT::datatable(futbol_table, rownames = F, filter = "top",
                  options = list(autoWidth = T, pageLength = 4))
    
    
    
  })
  
  output$barplot <- renderPlot({
    
    futbol_analysis <-
      futbol %>%
      select(Season, Player_Year, input$cluster) %>%
      filter(Season %in% input$years,
             Goals >= input$goals,
             Player_Year %in% input$players) %>%
      gather(Statistics, Value, -Season, -Player_Year)
    
    ggplot(futbol_analysis, aes(Statistics, Value, fill = Value)) +
      geom_bar(position = "dodge", stat = "summary") +
      facet_wrap( ~ Player_Year, ncol = 1) +
      scale_fill_gradient(low = "red", high = "green") +
      coord_flip()
    
    
    
  })
  
  output$PCA <- renderPlot({
    
    analysis <-
      futbol %>%
      select(Season, input$cluster) %>%
      filter(Season %in% input$years, 
             Goals >= input$goals)
    
    soccer_pca <- PCA(analysis[, -1])
    
    fviz_pca_var(soccer_pca, col.var="contrib") +
      scale_color_gradient2(low = "white", mid = "blue", 
                            high = "red") +
      theme_bw()
    
  })
  
  
  output$silhouette <- renderPlot({
    
    analysis <-
      futbol %>%
      select(Season, input$cluster) %>%
      filter(Season %in% input$years, 
             Goals >= input$goals)
    
    analysis_scaled <- scale(analysis[, -c(1:2)])
    
    fviz_nbclust(analysis_scaled, pam, method = "silhouette") +
      theme_classic()
    
  })
  
  
  
  
}
# RUN THE APP ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)


