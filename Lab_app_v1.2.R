#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# pacman::p_load(shiny, ggplot2, dplyr, tidyr, cowplot, shinythemes, shinydashboard, tidytext, data.table)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(shinythemes)
library(shinydashboard)
library(tidytext)
library(data.table)

options(shiny.maxRequestSize=500*1024^2)

## Create user interface variable
ui = dashboardPage(
    
    dashboardHeader(title = "Dandekar Lab App"),
    
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Plotting", tabName = "P", icon = icon('bar-chart-o'))
        )
    ),
    
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "P",
                    # Application title
                    titlePanel("Dandekar Lab Data Visualization")
                    )
        ),
                    
                    # Sidebar with select inputs for plotting. Most of these have a reactive component on the server side that updates based on user input.
                    fluidRow(
                        
                        box(
                            h3('Formatting Guidelines:'),
                        width = 2
                        ),
                        
                        tags$image(src = 'Slide1.jpg', height = 400)
                    ),
        
                    br(),
        
                    fluidRow(
                        
                        box(
                            fileInput('Data', 'Choose CSV file in plotting format (GeneID, ProteinID, metabolite, samples, etc. must all be in their own column)',
                                      accept = c('text/csv', '.txt', '.csv')),
                            selectInput('IDcol', 'Select your ID column', choices = NULL),
                            width = 3),
                        
                        box(    
                            selectInput('metaquestion', 'Do you have metadata?', c('No', 'Yes')),
                            
                            fileInput('metadata', 'Choose metadata file',
                                      accept = 'text/csv'), 
                            width = 3),
                        
                        box(    
                            selectInput('annoquestion', 'Do you have an annotation?', c('No', 'Yes')),
                            
                            fileInput('annotation', 'Choose annotation file',
                                      accept = 'text/csv'),
                            width = 3),
                        
                        box(
                            selectInput('PlotType', 'Select plot type:', c('Boxplot', 'Barplot')),
                            
                            selectInput('feat', 'What do you want to plot?', 
                                        choices = NULL), 
                            
                            selectizeInput('filter', 'Which variable (gene, protein, metabolite, counts, etc.)',
                                           choices = NULL, multiple = T),
                            
                            width = 3)
                        
                        ),
        
                    fluidRow(
                        
                        box(
                            selectInput('x', 'Select plot x-axis:', choices = NULL),
                            
                            selectizeInput('xsub', 'Subset x-axis (optional):', choices = NULL),
                            
                            width = 3),
                        
                        box(
                            selectInput('y', 'Select plot y-axis:', choices = NULL),

                            selectizeInput('ysub', 'Subset y-axis (optional):', choices = NULL),

                            width = 3),
                        
                        box(
                            selectInput('facet', 'Facet by:', choices = NULL),
                            
                            selectizeInput('fill', 'Fill by:', choices = NULL),
                            width = 3),

                        box(
                            selectInput('datsub', 'Subset Dataset (optional):', choices = NULL),

                            selectizeInput('datsubfeat', 'Which feature(s)?', choices = NULL, multiple = T),

                            width = 3)
                        
                        ),
        
                    fluidRow(
                        
                            downloadButton("downloadData", "Download This Plot"),
                        
                        box(
                            numericInput('SaveWidth', 'Change size of saved plot', value = 8, width = '100px'),
                            width = 2)
                        
                    ),
                    
                    fluidRow(
                        
                        box(
                        plotOutput('Bplot', width = 'auto'),
                        width = 10)
                        
                    ),
        
            )
    )




# Define server logic 
server <- function(input, output, session) {
    
    ## This defines the dataset to be worked on in the server based on user selection
    dataset <- reactive({

        req(input$Data)

        fread(input$Data$datapath)
    })
    
    observe({
        updateSelectInput(session, 'IDcol', 'Select your ID column', 
                          choices = names(dataset()))
    })
    
    metadata <- reactive({

        req(input$metadata)

        fread(input$metadata$datapath)
    })
    
    annotation <- reactive({
        
        req(input$annotation)
        
        fread(input$annotation$datapath)
    })
    
    dataset01 = reactive({
        
        req(c(input$IDcol))
        
        if(input$annoquestion == 'Yes') {
            
            dataset() %>%  
            pivot_longer(!.data[[input$IDcol]], names_to = 'Sample') %>%
            mutate(Group = gsub('..$', '', Sample)) %>% 
            left_join(annotation())
            
        } else {
            
            dataset() %>%   
                pivot_longer(!.data[[input$IDcol]], names_to = 'Sample') %>%
                mutate(Group = gsub('..$', '', Sample))
            
        }
    })
    
    dataset02 = reactive({
        
        req(c(input$Data, input$metadata))
        
        if(input$metaquestion == 'Yes') {
            
            dataset01() %>% 
                left_join(metadata())
        } else {
            
            dataset01()
        }
        
        
    })
    
    ## Each of these 'observe' functions with 'updateselect(ize)Input' is required to make input options based on previous user input. As you may notice, they are matched to ui inputs.
    observe({
        updateSelectInput(session, 'feat', 'What do you want to plot?', 
                          choices = names(dataset02()))
    })
    
    observe({
        updateSelectizeInput(session, 'filter', 'Which feature(s)? If multiple, you should facet by variable selected above.', choices = unique(dataset02()[[input$feat]]), server = T)
    })
    
    ## Filtering the data based on user inputs
           dataset2 = reactive({
                    
                    req(c(input$feat, input$filter))
                    
                    dataset02() %>% 
                        filter(.data[[input$feat]] %in% input$filter) 
                    
                })

        
    ## Each of these 'observe' functions with 'updateselect(ize)Input' is required to make input options based on previous user input. As you may notice, they are matched to ui inputs.    
    
    observe({
        updateSelectizeInput(session, 'x', 'Select plot x-axis:', choices = names(dataset02()), server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'xsub', 'Subset x-axis (optional):', choices = c('None', unique(dataset02()[[input$x]])), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'y', 'Select plot y-axis:', choices = names(dataset02()), server = T)
    })

    observe({
        updateSelectizeInput(session, 'ysub', 'Subset y-axis (optional):', choices = c('None', unique(dataset02()[[input$y]])), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'facet', 'Facet by:', choices = c('None', names(dataset02())), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'fill', 'Fill by:', choices = c(names(dataset02())), server = T)
    })

    observe({
        updateSelectizeInput(session, 'datsub', 'Subset Dataset (optional):', choices = c('None', names(dataset02())), selected = 'None', server = T)
    })

    observe({
        updateSelectizeInput(session, 'datsubfeat', 'Which feature(s)?', choices = c('None', unique(dataset02()[[input$datsub]])), selected = 'None', server = T)
        
    })
    
    ## Filtering the data based on user inputs
    dataset3 = reactive({
        
        req(c(input$datsub, input$datsubfeat))
        
        dataset2() %>% 
            filter(.data[[input$datsub]] %in% input$datsubfeat)
        
    })

    
    ## Define plotting variable in server to be displayed in ui based on inputs
    plotInput = reactive({
        
        if(input$datsub != 'None') {
            
            dat = dataset3()
        
        } else {
            
            dat = dataset2()
        }

        # if(input$xsub != 'None') {
        #     dat = dat %>% filter(.data[[input$x]] %in% input$xsub)
        # }
        # 
        # if(input$ysub != 'None') {
        #     dat = dat %>% filter(.data[[input$y]] %in% input$ysub)
        # }
        # 
        # if(input$datsub != 'None') {
        #     dat = dat %>% filter(.data[[input$datsub]] %in% input$datsubfeat)
        # }
        ## Conditional statement for input boxplot
        if(input$PlotType == 'Boxplot') {
            if(input$facet == 'None') {
                p = ggplot(dat, aes(reorder(.data[[input$x]], .data[[input$y]]),
                                    .data[[input$y]], fill = .data[[input$fill]]))+
                    geom_boxplot()+
                    scale_x_reordered()+
                    theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
                    guides(fill = guide_legend(title = input$fill))+
                    ggtitle(input$filter)+
                    labs(x = input$x, y = input$y)

            } else {

                p = ggplot(dat, aes(reorder_within(.data[[input$x]], .data[[input$y]], .data[[input$facet]]),
                                         .data[[input$y]], fill = .data[[input$fill]]))+
                    geom_boxplot()+
                    scale_x_reordered()+
                    theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
                    guides(fill = guide_legend(title = input$fill))+
                    labs(x = input$x, y = input$y)+
                    facet_wrap(~.data[[input$facet]], scales = "free", ncol = 2)
            }

        } else {
            ## conditional input for input barplot
            if(input$PlotType == 'Barplot') {
                if(input$facet == 'None') {
                    p = ggplot(dat, aes(reorder(.data[[input$x]], .data[[input$y]]),
                                        .data[[input$y]], fill = .data[[input$fill]]))+
                        geom_bar(stat = 'identity')+
                        scale_x_reordered()+
                        theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
                        guides(fill = guide_legend(title = input$fill))+
                        labs(x = input$x, y = input$y)

                } else {

                    p = ggplot(dat, aes(reorder_within(.data[[input$x]], .data[[input$y]], .data[[input$facet]]),
                                        .data[[input$y]], fill = .data[[input$fill]]))+
                        geom_bar(stat = 'identity')+
                        scale_x_reordered()+
                        theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
                        guides(fill = guide_legend(title = input$fill))+
                        labs(x = input$x, y = input$y)+
                        facet_wrap(~.data[[input$facet]], scales = "free_x")
                }

            }

        }


    })
    ## This is required to set the plot to a variable of 'output'
    output$Bplot <- renderPlot({
        print(plotInput())
    })
    ## This creates a button to downnload the current plot
    output$downloadData <- downloadHandler(
        filename = function() { paste(gsub('[[:punct:]]', '_', input$filter), '.png', sep='_') },
        content = function(file) {
            save_plot(file, plotInput(), base_height = input$SaveWidth)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
