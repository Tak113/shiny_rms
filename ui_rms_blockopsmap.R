tabItem_rms_bom <- 
  tabItem(tabName = "blockopsmap",
          
          fluidPage(
            
            shinyjs::useShinyjs(),
            # shinyjs::inlineCSS(appCSS),
            
            titlePanel("RMS : Process Block - Operations - Resources Map"),
            helpText("Make sure to save for the update(s) you make. Each events are stored only at user side until user saves"),
            helpText("Block Ops Map data table holds Operations and Resources combination and its MOR by process block options"),
            br(),
            
            #Architecture selection pull down
            fluidRow(
              column(4,
                     uiOutput('ui_bom_selection')
              )
            ),
            
            br(),
            
            #operation button settings
            fluidRow(
              column(12,
                     actionButton("add_button_bom", "Add", icon("plus")),
                     actionButton("edit_button_bom", "Edit", icon("edit")),
                     actionButton("copy_button_bom", "Copy", icon("copy")),
                     actionButton("delete_button_bom", "Delete", icon("trash-alt")),
                     actionButton("fetch_button_bom", "Fetch", icon("sync-alt"))
              )
            ),
            br(),
            
            #datatable
            fluidRow(
              column(12, #data table width
                     dataTableOutput("bom_table")
              )
            ),
            
            #shiny alert setting
            useShinyalert(),
            
            #save, download button
            fluidRow(
              column(12,
                     actionButton("save_button_bom", "Save", icon("save")),
                     downloadButton("download_button_bom", "Download", icon("download"))
              )
            )
            
          )
          
  )