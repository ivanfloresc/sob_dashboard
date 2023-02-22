library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(shinycssloaders)
library(SwimmeR)
library(arrow)
library(formattable)

# Read feather data
data <- read_feather("all_data.feather")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Summary of Business"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Table", tabName = "table", icon=icon("table")),
            selectInput("crop", "Choose a crop:", choices = c(unique(data$commodity_name))),
            selectInput("state", "Select a state:", choices=c("All", unique(data$state_ab)), selected = "All"),
            selectInput("county", "Select a county:", choices=c("All"), selected = "All"),
            downloadButton("downloadTable", label="Download table"),
            menuItem("Plot", tabName = "plot", icon=icon("chart-line")),
            downloadButton("downloadPlot", label="Download plot")
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "plot",
                    fluidRow(
                        box(plotOutput("plot1", height = 400),width = 12)
                    )
            ),
            tabItem(tabName = "table",
                    fluidRow(
                        box(dataTableOutput("table"),width = 12)
                    )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    filtered_data <- reactive({
        data %>%
            filter(commodity_name == input$crop) %>%
            filter(if (input$state != "All") state_ab == input$state else TRUE) %>%
            filter(if (input$county != "All") county_name == input$county else TRUE) %>%
            group_by(year) %>%
            summarise(
                sum_acres = sum(net_report_lvl_amnt / 1000000),
                premium = sum(premium / 1000000),
                farmer_subsidy = sum(subsidy / 1000000),
                indemnity = sum(indemnity / 1000000)
            ) %>%
            mutate(
                loss_ratio = indemnity / premium,
                gain_loss = premium - indemnity,
                prem_gain_rate = gain_loss / premium,
                farmer_prem_paid = premium - farmer_subsidy,
                farmer_net_paid = indemnity - farmer_prem_paid
            )
    })
    
    # Update subgroup input based on group selection
    observeEvent(input$state, {
        countygroup <- data %>%
            filter(state_ab == input$state) %>%
            pull(county_name) %>%
            unique()
        updateSelectInput(session, "county", choices = c("All", countygroup), selected = "All")
    })
    
    # Render table
    output$table <- renderDT({
        datatable(
            filtered_data(),
            colnames = c(
                "Year", "Acres", "Total Premium", "Farmer Subsidy",
                "Indemnity Payments", "Loss Ratio", "$Gain(loss)", "%Prem Gain Rate",
                "Farmer Prem Paid-$", "Farmer Net-$"
            ),
            caption = paste0(
                "Table, Federal Crop Insurance, State: ",
                input$state, ", County: ", input$county, ", ",
                str_to_title(input$crop), " Only, $ Millions (except rates)"
            ),
            class = "display caption",
            options = list(scrollX = TRUE, scrollY=TRUE, pageLength=24), rownames=FALSE) %>%  
            formatPercentage('prem_gain_rate',2) %>%
            formatRound(columns=c('sum_acres','premium','farmer_subsidy','indemnity','loss_ratio','gain_loss','farmer_prem_paid','farmer_net_paid'), digits=3)
        
    })
    
    # Render plot
    output$plot1 <- renderPlot({
        filtered_data() %>% 
            select(year,farmer_prem_paid,farmer_subsidy,indemnity) %>%  pivot_longer(!year, names_to = "variable",values_to = "value") %>% 
            mutate(variable_cat = ifelse(variable=="farmer_prem_paid", "Premium", ifelse(variable=="farmer_subsidy", "Subsidy", "Indemnity"))) %>% 
            ggplot(aes(x = year, y = value, color=variable_cat)) + 
            geom_line(linewidth=1) + 
            theme_bw(base_size=20) +
            labs(x="Year" , y="Million $", title=paste0("Crop Program Data, Crop: ", str_to_title(input$crop)), subtitle=paste0("State: ",input$state , ", County: ",input$county)) +
            guides(color=guide_legend(title="Variable"))
        
        #ggplot(filtered_data(), aes(x = year, y = loss_ratio)) + 
        #  geom_line()
    })
    
    
    
    # Download plot
    output$downloadPlot <- downloadHandler(
        filename = "plot.png",
        content = function(file) {
            ggsave(file, plot =filtered_data() %>% 
                       select(year,farmer_prem_paid,farmer_subsidy,indemnity) %>%  pivot_longer(!year, names_to = "variable",values_to = "value") %>% 
                       mutate(variable_cat = ifelse(variable=="farmer_prem_paid", "Premium", ifelse(variable=="farmer_subsidy", "Subsidy", "Indemnity"))) %>% 
                       ggplot(aes(x = year, y = value, color=variable_cat)) + 
                       geom_line(linewidth=1) + 
                       theme_bw(base_size=20) +
                       labs(x="Year" , y="Million $", title=paste0("Crop Program Data, Crop: ", str_to_title(input$crop)), subtitle=paste0("State: ",input$state , ", County: ",input$county)) +
                       guides(color=guide_legend(title="Variable")), device = "png")
        }
    )
    
    # Download table
    # Download filtered data
    output$downloadTable <- downloadHandler(
        
        filename = function() {
            paste("filtered_data", ".csv", sep = "")
        },
        content = function(file) {
            
            write.csv(filtered_data(), file, row.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)


