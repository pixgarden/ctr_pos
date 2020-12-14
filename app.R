library(shiny)
library(DT)
library(plotly)

# Define UI ----
ui <- fluidPage(
  #upload and settings
  titlePanel("Compare search queries CTR VS Average Position"),
  mainPanel(
    fileInput(
      "file1",
      "1. 'Queries.csv' File",
      multiple = FALSE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    textInput("brand", "2. brand (regex)", "brand name|brandname"),
    textInput("filter", "3. filter (regex)", ".*"),
    checkboxInput("mean", "Mean CTR by Avg. Pos.", FALSE),
    checkboxInput("median", "Median CTR by Avg. Pos.", FALSE)
    #sliderInput(inputId = "impression", label = "Impression", min = 0, max = max(df$Impressions), value = c(0,  max(df$Impressions)), step = 5)
    
  ),
  #howto section
  h2("How to use this tool?"),
  
  p(
    "Upload your Queries.csv file from Google Search Console, fill in you brand and to compare each search queries CTR VS avg. Position."
  ),
  p(
    "Checkout search queries on the top left, they have good positions and bad CTR."
  ),
  p(
    "You might want rework a few metadata to increase CTR and secure more SEO traffic. The darker a dot is, the more impressions it represents.
    Ideally we should to remove data from non-targeted countries and remove Google Image search data."
  ),
  
  hr(),
  
  tabsetPanel(
    tabPanel("ALL", fluid = TRUE,
             mainPanel(plotlyOutput("contents2"))),
    
    tabPanel("Explorer", fluid = TRUE,
             mainPanel(DTOutput("contents")))
    
  )
  )

# Define server logic to read selected file ----
server <- function(input, output) {
  #graph
  output$contents2 <- renderPlotly({
    #need files to upload
    req(input$file1)
    
    
    
    tryCatch({
      df <- read.csv(input$file1$datapath,
                     header = TRUE)
      #sep = ",",
      #quote = input$quote)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    #rename colonne because sometimes colname doesn't match in other language
    colnames(df) <-
      c("Query", "Clicks", "Impressions", "CTR", "Position")
    df <- droplevels(df)
    
    # transform CTR to a number
    
    df$CTR <-
      as.numeric(gsub(pattern = "%", replacement = "", df$CTR))
    df <- na.omit(df)
    
    
    # add branding tag according to setting
    
    df <- df %>%
      #mutate(branding =  stringr::str_detect(Query, "green"))
      mutate(branding =  stringr::str_detect(Query, input$brand)) %>%
      filter(grepl(input$filter, Query))
    
    
    # average and mean computation per positions
    
    
    df_mean <- df
    
    df_mean$Position <- round(df$Position)
    
    
    # grabing max impression to be sure dot are being display with 1 opacity
    max <- max(df_mean$Impressions)
    
    df_median <- df_mean
    
    df_mean <- df_mean %>%
      group_by(Position) %>%
      summarise(CTR = mean(CTR), Impressions = max)
    
    df_median <- df_median %>%
      group_by(Position) %>%
      summarise(CTR = median(CTR), Impressions = max)
    
    df_mean$Query <- paste("Mean Pos.", df_mean$Position)
    df_median$Query <- paste("Median Pos.", df_mean$Position)
    
    #install.packages("scales")
    library(scales)
    
    p <- ggplot(data = df) +
      aes(
        x = CTR ,
        y = Position,
        label = Query,
        alpha = Impressions,
        color = branding
      ) +
      geom_point() +
      theme_minimal() +
      scale_x_continuous(label = label_number(suffix = "%")) +
      scale_y_reverse(lim = c(15, 1)) +
      scale_alpha(range = c(0.3, 1)) +
      scale_color_manual(values = c("#09043a", "#ff0000")) +
      labs(title = "POS 1", x = "SERPs CTR", y = "Avg. Google Position") +
      theme(legend.position = "bottom") +
      scale_alpha(range = c(0.05, 1), trans = "log")
    
    if (input$mean) {
      p <- p + geom_point(data = df_mean,
                          color = 'gold',
                          size = 3)
    }
    
    if (input$median) {
      p <- p + geom_point(data = df_median,
                          color = 'green',
                          size = 3)
    }
    #render plotly using ggplot graph
    print(ggplotly(p))
  })
  
  output$contents <- renderDT({
    req(input$file1)
    
    tryCatch({
      df <- read.csv(input$file1$datapath,
                     header = T,
                     sep = ",")
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    
    return(df)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
