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
library(DT)
library(shinyjs)

options(shiny.maxRequestSize=500*1024^2)

## Create user interface variable
ui = dashboardPage(
    
    dashboardHeader(title = "Dandekar Lab App"),
    
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem('Data manipulator', tabName = 'DM'),
            menuItem("Boxplot/Barplot", tabName = "P", icon = icon('bar-chart-o'))
        )
    ),
    
    ## Body content
    dashboardBody(
        tabItems(
            
            tabItem('DM',
                    titlePanel('Use this to manipulate your data if needed'),
                    
                    fluidRow(
                        
                        box(
                            fileInput('rawData', 'Choose CSV file to manipulate',
                                      accept = c('text/csv', '.txt', '.csv')),
                            
                            downloadButton("downloadData", "Download Modified Data"),
                            
                            width = 3),
                        
                        box(
                            selectInput('remCol', 'Choose column(s) to remove', choices = NULL, multiple = T),
                            
                            selectInput( 'remRow', 'Choose row(s) to remove', choices = NULL, multiple = T),
                            
                            actionButton('remAction', 'Remove'),
                            width = 3),
                        
                        box(
                            selectInput('nameCol', 'Choose row to set as column names', choices = NULL, multiple = F),
                            
                            actionButton('nameAction', 'Rename'),
                            width = 3),
                        
                        box(
                            selectInput('oldName', 'Select column to rename', choices = NULL, multiple = F),
                            
                            textInput('newName', 'Type new name'),
                            
                            actionButton('newNameAction', 'Rename'),
                            width = 3)
                    ),
                    
                    br(),
                    
                    fluidRow(
                        
                        DT::DTOutput('rawTable')
                        
                    )
                    
            ),
            # First tab content
            tabItem(tabName = "P",
                    # Application title
                    titlePanel("Boxplot/Barplot"),
                    
                    # Sidebar with select inputs for plotting. Most of these have a reactive component on the server side that updates based on user input.
                    fluidRow(
                        
                        box(
                            h3('Formatting Guidelines:'),
                            width = 2
                        ),
                        
                        tags$image(src = 'Slide1.JPG', height = 400)
                    ),
                    
                    br(),
                    
                    fluidRow(
                        
                        DT::DTOutput('plotData')
                        
                    ),
                    
                    br(),
                    
                    fluidRow(
                        
                        box(
                            fileInput('Data', 'Choose CSV file in plotting format (Guidelines Above). Your data is displayed above.',
                                      accept = c('text/csv', '.txt', '.csv')),
                            selectInput('IDcol', 'Select your ID column', choices = NULL),
                            width = 3),
                        
                        box(
                            fileInput('annotation', 'Choose annotation file. Numeric IDs are required for joining with plotting data.',
                                      accept = 'text/csv'),
                            
                            selectInput('AnnoID', 'Select ID column matching that of your data', choices = NULL),
                            
                            selectInput('annoquestion', 'Did you load annotation', choices = c('No', 'Yes')),
                            
                            width = 3),
                        
                        box(
                            fileInput('metadata', 'Choose metadata file',
                                      accept = 'text/csv'), 
                            
                            selectInput('Sampcol', 'Select your Sample column name', choices = NULL),
                            
                            actionButton('metaquestion', 'Join with data'),
                            
                            width = 3),
                        
                        box(
                            selectInput('PlotType', 'Select plot type:', c('Boxplot', 'Barplot')),
                            
                            selectInput('feat', 'What do you want to plot?', 
                                        choices = NULL), 
                            
                            actionButton('Plot', 'Click to plot.'),
                            
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
                        
                        downloadButton("downloadPlot", "Download This Plot"),
                        
                        downloadButton("downloadPlotData", "Download Plotting Data"),
                        
                        box(
                            numericInput('SaveWidth', 'Change size of saved plot', value = 8, width = '100px'),
                            width = 2)
                        
                    ),
                    
                    fluidRow(
                        
                        box(
                            plotOutput('Bplot', width = 'auto'),
                            width = 10)
                        
                    )
            )
        ),
        
    )
)




# Define server logic 
server <- function(input, output, session) {
    
    rawData = reactive({
        
        req(input$rawData)
        
        fread(input$rawData$datapath) %>% 
            mutate(across(everything(), ~replace(., . == '', 'NA')))
        
    })
    
    # df = rawData()
    
    modData = reactiveValues()
    
    observe({
        
        modData$x = rawData()
        
    })
    
    
    output$rawTable = DT::renderDT({
        
        datatable(modData$x, selection = list(target = 'column'))
    })
    
    observe({
        updateSelectizeInput(session, 'remCol', 'Choose column(s) to remove',
                             choices = c(names(modData$x)), server = T)
    })
    
    
    observe({
        updateSelectInput(session, 'remRow', 'Choose row(s) to remove',
                          choices = c(rownames(modData$x)))
    })
    
    observe({
        updateSelectInput(session, 'nameCol', 'Choose row to set as column names', 
                          choices = c(rownames(modData$x)))
        
    })
    
    observe({
        updateSelectInput(session, 'oldName', 'Select column to rename', 
                          choices = c(names(modData$x)))
        
    })
    
    observeEvent(input$remAction, {
        
        req(c(input$remCol, input$remRow))
        
        modData$x = modData$x %>% 
            select(!all_of(input$remCol)) %>% 
            filter(!row_number() %in% input$remRow)
    })
    
    observeEvent(input$nameAction, {
        
        req(c(input$nameCol))
        
        names = modData$x %>% 
            slice(as.numeric(input$nameCol)) %>% 
            unlist(., use.names = F) %>% 
            make.unique()
        
        modData$x = modData$x %>% 
            filter(!row_number() %in% as.numeric(input$nameCol))
        
        names(modData$x) = names
    })
    
    observeEvent(input$newNameAction, {
        
        req(c(input$newName, input$oldName))
        
        modData$x = modData$x %>% 
            rename_with(~gsub(input$oldName, input$newName, .x))
        
    })
    
    output$downloadData <- downloadHandler(
        filename = function() { paste(input$rawData, '_modified.csv', sep='') },
        content = function(file) {
            fwrite(modData$x, file)
        }
    )
    
    ## This defines the dataset to be worked on in the server based on user selection
    
    
    
    dataset = reactive({
        
        req(input$Data)
        
        fread(input$Data$datapath)
    })
    
    datasetMod = reactiveValues()
    
    observe({
        
        datasetMod$x = dataset()
        
    })
    
    
    output$plotData = DT::renderDT({
        
        datatable(datasetMod$x)
    })
    
    ## Each of these 'observe' functions with 'updateselect(ize)Input' is required to make input options based on previous user input. As you may notice, they are matched to ui inputs.
    observe({
        
        req(input$annoquestion)
        
        if(input$annoquestion == 'Yes') {
            
            
            observe({
                updateSelectInput(session, 'feat', 'What do you want to plot?', 
                                  choices = names(annotation()))
            })
            
        } else {
            
            observe({
                updateSelectInput(session, 'feat', 'What do you want to plot?', 
                                  choices = names(datasetMod$x))
            })
            
        }
    })
    
    observe({
        
        req(input$annoquestion)
        
        if(input$annoquestion == 'Yes') {
    
    observe({
        updateSelectizeInput(session, 'filter', 'Which feature(s)? If multiple, you should facet by variable selected above.', choices = unique(annotation()[[input$feat]]), server = T)
    })
            
        } else {
            
            observe({
                updateSelectizeInput(session, 'filter', 'Which feature(s)? If multiple, you should facet by variable selected above.', choices = unique(dataset()[[input$feat]]), server = T)
            })
            
        }
    })
    
    ## Filtering the data based on user inputs
    observe({
        
        req(c(input$feat, input$filter))
        
        if(input$annoquestion == 'Yes') {
        
        datasetMod$x = annotation() %>% 
                       filter(.data[[input$feat]] %in% input$filter) %>% 
                       right_join(datasetMod$x)
        } else {
            
            datasetMod$x = dataset() %>% 
                filter(.data[[input$feat]] %in% input$filter) 
            
        }
        
    })
    
    ##
    
    observe({
        updateSelectInput(session, 'IDcol', 'Select your ID column', 
                          choices = names(dataset()))
    })
    
    Names = reactive({
        input$IDcol
    })
    
    NamesMod = reactiveValues()
    
    observe({
        
        NamesMod$x = Names()
        
    })
    
    observeEvent(input$Plot, {
        
        req(input$IDcol)
        
        datasetMod$x = datasetMod$x %>%   
            pivot_longer(!.data[[input$IDcol]], names_to = 'Sample', values_to = 'Expression Level') %>%
            mutate(Group = gsub('..$', '', Sample), ID_Column = as.numeric(.data[[input$IDcol]])) %>% 
            select(!.data[[input$IDcol]]) %>% 
            rename_with(~ gsub('ID_Column', input$IDcol, .x)) %>% 
            relocate(.data[[input$IDcol]])
    })
    
    annotation <- reactive({
        
        req(input$annotation, input$IDcol)
        
        fread(input$annotation$datapath) 
        
    })
    
    observe({
        updateSelectInput(session, 'AnnoID', 'Select ID column matching that of your data', 
                          choices = names(annotation()))
    })
    
    # observe({
    #     
    #     
    # })
    
    # observeEvent(input$Plot, {
    
    observe({
        
        req(c(input$annoquestion))
        
        if(input$annoquestion == 'Yes') {
        
        anno = annotation() %>% 
            mutate(ID_Column = as.numeric(.data[[input$AnnoID]])) %>% 
            select(!.data[[input$AnnoID]]) %>% 
            rename_with(~ gsub('ID_Column', input$IDcol, .x))%>% 
            relocate(.data[[input$AnnoID]])
        
        NamesMod$x = c(input$IDcol, names(annotation()))
        
        datasetMod$x = datasetMod$x %>%  
            left_join(anno)
        
        } else {
            
            datasetMod$x = datasetMod$x
        }
        
    })
    
    metadata <- reactive({
        
        req(input$metadata)
        
        fread(input$metadata$datapath)
    })
    
    observe({
        updateSelectInput(session, 'Sampcol', 'Select your Sample column name', 
                          choices = names(metadata()))
    })
    
    
    
    observeEvent(input$metaquestion, { 
        
        hide('metaquestion')
    })
    
    
    observeEvent(input$metaquestion, {
        
        req(c(input$metaquestion, input$Sampcol))
        
        datasetMod$x = datasetMod$x %>%  
            left_join(metadata(), by = input$Sampcol)
        
    })
    
    ## Each of these 'observe' functions with 'updateselect(ize)Input' is required to make input options based on previous user input. As you may notice, they are matched to ui inputs.    
    
    observe({
        updateSelectizeInput(session, 'x', 'Select plot x-axis:', choices = NamesMod$x, server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'xsub', 'Subset x-axis (optional):', choices = c('None', unique(datasetMod$x[[input$x]])), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'y', 'Select plot y-axis:', choices = names(datasetMod$x), server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'ysub', 'Subset y-axis (optional):', choices = c('None', unique(datasetMod$x[[input$y]])), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'facet', 'Facet by:', choices = c('None', names(datasetMod$x)), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'fill', 'Fill by:', choices = c(names(datasetMod$x)), server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'datsub', 'Subset Dataset (optional):', choices = c('None', names(datasetMod$x)), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'datsubfeat', 'Which feature(s)?', choices = c('None', unique(datasetMod$x[[input$datsub]])), selected = 'None', server = T)
        
    })
    
    ## Filtering the data based on user inputs
    dataset3 = reactive({
        
        req(c(input$datsub, input$datsubfeat))
        
        dataset2() %>% 
            filter(.data[[input$datsub]] %in% input$datsubfeat)
        
    })
    
    
    ## Define plotting variable in server to be displayed in ui based on inputs
    plotInput = observeEvent(input$Plot, {
            
            if(input$datsub != 'None') {
                
                dat = dataset3()
                
            } else {
                
                dat = datasetMod$x
            }
            
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
    output$downloadPlot <- downloadHandler(
        filename = function() { paste(gsub('[[:punct:]]', '_', input$filter), '.png', sep='_') },
        content = function(file) {
            save_plot(file, plotInput(), base_height = input$SaveWidth)
        }
    )
    
    output$downloadPlotData <- downloadHandler(
        filename = function() { paste(input$Data, '_joined_with_metadata_or_annotation.csv', sep='') },
        content = function(file) {
            fwrite(datasetMod$x, file)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)