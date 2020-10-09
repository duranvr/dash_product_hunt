library(googlesheets4)
library(googledrive)
library(dplyr)
library(lubridate)

#### Def reactive functions ####

dataInput_task4 <- reactive({
    drive_upload(intput$task_submit_4,
                 drive_path)
    sheet_append(submit_history_sheet,
                 data.frame(
                     team_id = team_id_render,
                     timestamp = format(now(), format = "%Y-%m-%dT%H:%M:%S"),
                     task_id = 4,
                     pt_on_time = ifelse(now() < task_deadlines$deadline[4], 50, 0),
                     stringsAsFactors = FALSE
                 ))
})

#### Load data first time ####

team_id_render <- 1

item_usage_sheet <- "https://docs.google.com/spreadsheets/d/1hQaJ_8RFGPcUvzBI9qipm40oGNog4cF4pGlVp0VZPAY/edit?ts=5f7bc93d#gid=0"
purchases_sheet <- "https://docs.google.com/spreadsheets/d/1DV9xSHGVde6_zl1Ax9MDgwjtffwvNDdUNPHE2fT36HM/edit?ts=5f7bc948#gid=0"
submit_history_sheet <- "https://docs.google.com/spreadsheets/d/1hQaJ_8RFGPcUvzBI9qipm40oGNog4cF4pGlVp0VZPAY/edit?ts=5f7bc93d#gid=0"

purchases <- read_sheet(purchases_sheet,
                        sheet = "Market") %>% 
    filter(team_id == team_id_render)

item_usage <- read_sheet(item_usage_sheet,
                         sheet = "uso itens") %>% 
    filter(team_id == team_id_render)

submit_history <- read_sheet(submit_history_sheet,
                             sheet = "Histórico Entrega") %>% 
    filter(team_id == team_id_render)

task_deadlines <- data.frame(
    task = c(1, 2, 3, 4),
    deadline = c(
        "2020-10-19",
        "2020-10-25",
        "2020-10-26",
        "2020-11-01"
    )
)


date_render <- "2020-10-13"


drive_path <- paste0("ENTREGAS TIMES/Product Game - Time ", team_id_render, "/")


task_vec <- submit_history %>% 
    filter(team_id == 6) %>% 
    pull(task_id)

last_task_completed <- if(length(task_vec)){
    max(task_vec)
} else {
    0
}


#### App ####

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exibindo para grupo 1 - final dia 1"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            if(1 > last_task_completed){
                fileInput("task_submit_1", "Envie sua tarefa 1 aqui",
                          multiple = FALSE,
                          accept = c(".pdf"))
            },
            fileInput("task_submit_2", "Envie sua tarefa 2 aqui",
                      multiple = FALSE,
                      accept = c(".pdf")),
            fileInput("task_submit_3", "Envie sua tarefa 3 aqui",
                      multiple = FALSE,
                      accept = c(".pdf")),
            fileInput("task_submit_4", "Envie sua tarefa 4 aqui",
                      multiple = FALSE,
                      accept = c(".pdf")),
            
            sliderInput("tasks_done",
                        "Tarefas Completas:",
                        min = 1,
                        max = 4,
                        value = last_task_completed)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           
            
            textOutput("remaining_points"),
            tableOutput("available_items"),
            
            tableOutput("submit_table"),
            tableOutput("purchase_table"),
            tableOutput("item_usage_table"),
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # dataInput_task4()
    observe({
        if(!4 %in% 4){
            req(input$task_submit_4)
            
            drive_upload(input$task_submit_4$datapath,
                         paste0(drive_path, "task4.pdf"))
            sheet_append(submit_history_sheet,
                         sheet = "Histórico Entrega",
                         data.frame(
                             team_id = team_id_render,
                             timestamp = format(now(), format = "%Y-%m-%dT%H:%M:%S"),
                             task_id = 4,
                             pt_on_time = ifelse(now() < task_deadlines$deadline[4], 50, 0),
                             stringsAsFactors = FALSE
                         ))
            submit_history <- read_sheet("https://docs.google.com/spreadsheets/d/1hQaJ_8RFGPcUvzBI9qipm40oGNog4cF4pGlVp0VZPAY/edit?ts=5f7bc93d#gid=0",
                                         sheet = "Histórico Entrega")
            
        }

    })

    output$submit_table <- renderTable({
        submit_history %>% 
            filter(team_id == team_id_render)
    })
    output$purchase_table <- renderTable({
        purchases
    })
    output$item_usage_table <- renderTable({
        item_usage
    })
    
    output$remaining_points <- renderText({
        tot_purchases <- purchases %>% 
            pull(pt_cost_item) %>% sum()
        tot_points <- submit_history %>% 
            mutate(tot_points = pt_on_time + pt_task_quality) %>% 
            pull(tot_points) %>% sum()
        paste0("Product Coins: ", tot_points + tot_purchases)
    })
    
    output$available_items <- renderTable({
        full_join(purchases %>% 
            group_by(item_id) %>% 
            summarize(bought = n()),
        item_usage %>% 
            group_by(item_id) %>% 
            summarize(used = n())
        ) %>% 
            
            mutate(bought = ifelse(is.na(bought), 0, bought),
                   used = ifelse(is.na(used), 0, used),
                   available_items = bought - used) %>% 
            select(item_id, available_items)
        
        
    })
    # output$task4 <- renderText({
    #     print(input$task_submit_4)
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
