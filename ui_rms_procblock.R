tabItem_rms_procblock <-
  tabItem(tabName = "procblock",
          
          fluidPage(
            
            shinyjs::useShinyjs(),
            # shinyjs::inlineCSS(appCSS),
            
            titlePanel("RMS : Process Block"),
            helpText("Make sure to save for the update(s) you make. Each events are stored only at user side until user saves"),
            helpText("Process Block data table holds process block for each products recipe and its explanations"),
            
            br(),
            
            #operation button settings
            fluidRow(
              column(12,
                     actionButton("add_button_pb", "Add", icon("plus")),
                     actionButton("edit_button_pb", "Edit", icon("edit")),
                     actionButton("copy_button_pb", "Copy", icon("copy")),
                     actionButton("delete_button_pb", "Delete", icon("trash-alt")),
                     actionButton("fetch_button_pb", "Fetch", icon("sync-alt"))
              )
            ),
            br(),
            
            #datatable
            fluidRow(
              column(12, #data table width
                     dataTableOutput("pb_table")
              )
            ),
            
            #shiny alert setting
            useShinyalert(),
            
            #save, download button
            fluidRow(
              column(12,
                     actionButton("save_button_pb", "Save", icon("save")),
                     downloadButton("download_button_pb", "Download", icon("download"))
              )
            )
            
          )
          
  )
