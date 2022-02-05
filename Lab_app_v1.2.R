#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, ggplot2, dplyr, cowplot, shinythemes, shinydashboard, tidytext, data.table)

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
                    titlePanel("Dandekar Lab Data Visualization"),
                    
                    # Sidebar with select inputs for plotting. Most of these have a reactive component on the server side that updates based on user input.
                    inputPanel(
                        
                        box(
                            fileInput('Data', 'Choose CSV file in plotting format (GeneID, ProteinID, metabolite, samples, expression level, etc. must all be in their own column)',
                                      accept = 'text/csv'),
                            
                            selectInput('PlotType', 'Select plot type:', c('Boxplot', 'Barplot')),
                            width = 20),
                        
                        box(
                            selectInput('feat', 'What do you want to plot?', choices = NULL), width = 12),
                        
                        box(
                            selectizeInput('filter', 'Which variable (gene, protein, metabolite, counts, etc.)', choices = NULL, multiple = T), width = 20),
                        
                        box(
                            selectInput('x', 'Select plot x-axis:', choices = NULL),
                            
                            selectizeInput('xsub', 'Subset x-axis (optional):', choices = NULL),
                            
                            width = 12),
                        
                        box(
                            selectInput('y', 'Select plot y-axis:', choices = NULL),
                            
                            selectizeInput('ysub', 'Subset y-axis (optional):', choices = NULL),
                            
                            width = 12),
                        
                        box(
                            selectInput('datsub', 'Subset Dataset (optional):', choices = NULL),
                            
                            selectizeInput('datsubfeat', 'Which feature(s)?', choices = NULL),
                            
                            width = 20),
                        
                        box(
                            selectInput('facet', 'Facet by:', choices = NULL),
                            
                            selectizeInput('fill', 'Fill by:', choices = NULL),
                            width = 12)
                    ),
                    
                    mainPanel(
                        plotOutput('Bplot')),
                    
                    downloadButton("downloadData", "Download This Plot"),
                    
                    box(
                        numericInput('SaveWidth', 'Change size of saved plot', value = 8, width = '100px'),
                        width = 1)
                    
            )
        )
    )
)




# Define server logic 
server <- function(input, output, session) {
    ## This defines the dataset to be worked on in the server based on user selection
    dataset <- reactive({
        
        req(input$Data)
        
        fread(input$Data$datapath) 
    })
    ## Each of these 'observe' functions with 'updateselect(ize)Input' is required to make input options based on previous user input. As you may notice, they are matched to ui inputs.
    observe({
        updateSelectInput(session, 'feat', 'What category do you want to plot?', choices = names(dataset()))
    })
    
    observe({
        updateSelectizeInput(session, 'filter', 'Which feature(s)? If multiple, you should facet by variable selected above.', choices = unique(dataset()[[input$feat]]), server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'x', 'Select plot x-axis:', choices = names(dataset()), server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'xsub', 'Subset x-axis (optional):', choices = c('None', unique(dataset()[[input$x]])), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'y', 'Select plot y-axis:', choices = names(dataset()), server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'ysub', 'Subset y-axis (optional):', choices = c('None', unique(dataset()[[input$y]])), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'datsub', 'Subset Dataset (optional):', choices = c('None', names(dataset())), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'datsubfeat', 'Which feature(s)?', choices = c('None', unique(dataset()[[input$datsub]])), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'facet', 'Facet by:', choices = c('None', names(dataset())), selected = 'None', server = T)
    })
    
    observe({
        updateSelectizeInput(session, 'fill', 'Fill by:', choices = c(names(dataset())), server = T)
    })
    ## Define plotting variable in server to be displayed in ui based on inputs
    plotInput = reactive({
        ## Filtering the data based on user inputs
        dat = dataset() %>% filter(get(input$feat) %in% input$filter)
        
        if(input$xsub != 'None') {
            dat = dat %>% filter(get(input$x) %in% input$xsub)
        }
        
        if(input$ysub != 'None') {
            dat = dat %>% filter(get(input$y) %in% input$ysub)
        }
        
        if(input$datsub != 'None') {
            dat = dat %>% filter(get(input$datsub) %in% input$datsubfeat)
        }
        ## Conditional statement for input boxplot   
        if(input$PlotType == 'Boxplot') {
            if(input$facet == 'None') {
                p = ggplot(dat, aes(reorder(get(input$x), get(input$y)), 
                                    get(input$y), fill = get(input$fill)))+
                    geom_boxplot()+
                    scale_x_reordered()+
                    theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
                    guides(fill = guide_legend(title = input$fill))+
                    labs(x = input$x, y = input$y)
                
            } else {
                
                p = ggplot(dat, aes(reorder_within(get(input$x), get(input$y), get(input$facet)), 
                                    get(input$y), fill = get(input$fill)))+
                    geom_boxplot()+
                    scale_x_reordered()+
                    theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
                    guides(fill = guide_legend(title = input$fill))+
                    labs(x = input$x, y = input$y)+
                    facet_wrap(~get(input$facet), scales = "free", ncol = 2)
            }
            
        } else {
            ## conditional input for input barplot
            if(input$PlotType == 'Barplot') {
                if(input$facet == 'None') {
                    p = ggplot(dat, aes(reorder(get(input$x), get(input$y)), 
                                        get(input$y), fill = get(input$fill)))+
                        geom_bar(stat = 'identity')+
                        scale_x_reordered()+
                        theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
                        guides(fill = guide_legend(title = input$fill))+
                        labs(x = input$x, y = input$y)
                    
                } else {
                    
                    p = ggplot(dat, aes(reorder_within(get(input$x), get(input$y), get(input$facet)), 
                                        get(input$y), fill = get(input$fill)))+
                        geom_bar(stat = 'identity')+
                        scale_x_reordered()+
                        theme(text = element_text(size = 20), axis.text.x = element_text(angle = 30, vjust = 0.5))+
                        guides(fill = guide_legend(title = input$fill))+
                        labs(x = input$x, y = input$y)+
                        facet_wrap(~get(input$facet), scales = "free_x")
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
            save_plot(file,plotInput(), base_height = input$SaveWidth)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
