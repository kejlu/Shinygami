library(shinydashboard)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Shinygami"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Apple Worldwide Revenue (S)", tabName = "apple_revs", icon = icon("th")),
    menuItem("Apple Product Revenue (S)", tabName = "prod_revs", icon = icon("th")),
    menuItem("Apple Product Units Sold (S)", tabName = "prod_units", icon = icon("th")),
    menuItem("Apple Worldwide Revenue (b)", tabName = "apple_revb", icon = icon("bar-chart-o")),
    menuItem("Apple Product Revenue (b)", tabName = "prod_revb", icon = icon("bar-chart-o")),
    menuItem("Apple Product Units Sold (b)", tabName = "prod_unitb", icon = icon("bar-chart-o")),
    menuItem("Apple Worldwide Revenue (w)", tabName = "apple_revv", icon = icon("th"))
    
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard", h2('Lol uhhhhhh...'),
              p('Hello, this is a very basic demo (~20% of what I have in mind). Like an apple tree, this app will grow with time! (and after MLH Prime)')
      ), 
      tabItem(tabName = "apple_revs",
              fluidRow((plotlyOutput("plot1", height = 500, width = 700)))
      ), 
      tabItem(tabName = "prod_revs",
              fluidRow((plotlyOutput("plot2", height = 500, width = 700)))
      ),
      tabItem(tabName = "prod_units",
              fluidRow((plotlyOutput("plot3", height = 500, width = 700)))
      ),
      tabItem(tabName = "apple_revb",
              fluidRow((plotlyOutput("plot4", height = 500, width = 700)))
      ), 
      tabItem(tabName = "prod_revb",
              fluidRow((plotlyOutput("plot5", height = 500, width = 700)))
      ),
      tabItem(tabName = "prod_unitb",
              fluidRow((plotlyOutput("plot6", height = 500, width = 700)))
      ),
      tabItem(tabName = "apple_revv",
              fluidRow((plotOutput("plot7", height = 500, width = 700)))
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  countries_rev2 <- read.csv("apple2_revenue.csv", header = FALSE, stringsAsFactors = FALSE)
  colnames(countries_rev2) <- c('Q_Y', 'Y_Q', 'Year', 'Quarter', 'Region', 'Rev')
  
  prod_units2 <- read.csv("apple2_prod_units.csv", header = FALSE, stringsAsFactors = FALSE)
  colnames(prod_units2) <- c('Q_Y', 'Y_Q', 'Year', 'Quarter', 'Product', 'Units')
  
  prod_rev2 <- read.csv("apple2_prod_revenue.csv", header = FALSE, stringsAsFactors = FALSE)
  colnames(prod_rev2) <- c('Q_Y', 'Y_Q', 'Year', 'Quarter', 'Product', 'Rev')
  
#   plot1 <- eventReactive(input$plotButton1, {
#     input$Dataset1
#   })
  
  
  output$plot1 <- renderPlotly({
    p_region_rev <- ggplot(data = countries_rev2, aes(x = Y_Q, y = Rev, color = Region, group = Region, frame = Year)) + 
      geom_point(size = 4) + 
      geom_line() + theme(axis.text.x = element_text(angle = 30, hjust = 1)) + theme_bw() +
      xlab('Quarters') + ylab('Revenue (USD in millions)') + ggtitle('Apple Worldwide Revenue 2010 - 2016')
    #scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1)) +
    scale_y_continuous(limits = c(0, 35000), breaks = round(seq(min(countries_rev2$Rev), max(countries_rev2$Rev), by = 2000),1))
    ggplotly(p_region_rev)
  })
  
  output$plot2 <- renderPlotly({
    p_prod_rev <- ggplot(data = prod_rev2, aes(x = Y_Q, y = Rev, color = Product, group = Product )) +
      geom_point(size = 4) + geom_line() +theme_bw() +
      xlab('Quarters') + ylab('Revenue (USD in millions)') + ggtitle('Apple Product Revenue 2010 - 2016')
    ggplotly(p_prod_rev)
  })
  
  
  output$plot3 <- renderPlotly({
    p_prod_units <- ggplot(data = prod_units2, aes(x = Y_Q, y = Units, color = Product, group = Product )) +
      geom_point(size = 4) + geom_line() + theme_bw() +
      xlab('') + ylab('Units Sold (thousands)') + ggtitle('Apple Product Units Sold 2010 - 2016')
    ggplotly(p_prod_units)
  })
  
  output$plot4 <- renderPlotly({
    b_region_rev <- ggplot(data = countries_rev2, aes(x = Y_Q, y = Rev)) + #geom_point(size = 4) +
      xlab('Quarters') + ylab('Revenue (USD in millions)') + ggtitle('Apple Worldwide Revenue 2010 - 2016') +
      geom_bar(aes(fill = countries_rev2$Region), stat = 'identity', position = 'dodge') + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplotly(b_region_rev)
  })
  
  output$plot5 <- renderPlotly({
    b_prod_rev <- ggplot(data = prod_rev2, aes(x = Y_Q, y = Rev)) + #geom_point(size = 4) +
      geom_bar(aes(fill = prod_rev2$Product), stat = 'identity', position = 'dodge') + theme_bw() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab('Quarters') + ylab('Revenue (USD in millions)') + ggtitle('Apple Product Revenue 2010 - 2016')
    ggplotly(b_prod_rev)
  })
  
  
  output$plot6 <- renderPlotly({
    b_prod_units <- ggplot(data = prod_units2, aes(x = Y_Q, y = Units)) + #geom_point(size = 4) +
      geom_bar(aes(fill = prod_units2$Product), stat = 'identity', position = 'dodge') + theme_bw() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab('') + ylab('Units Sold (thousands)') + ggtitle('Apple Product Units Sold 2010 - 2016')
    ggplotly(b_prod_units)
  })
  
  output$plot7 <- renderPlot({
    ggplot_waterfall(
      dtData = countries_rev,
      'Y_Q', 'Americas') + xlab('Quarters') + ylab('Revenue (USD in millions)') + theme_bw() + ggtitle('Americas')
    
  })
  
}

shinyApp(ui, server)