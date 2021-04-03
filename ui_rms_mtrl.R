tabItem_rms_mtrl <-
  tabItem(tabName = "material",
          
          fluidPage(
            
            shinyjs::useShinyjs(),
            # shinyjs::inlineCSS(appCSS),
            
            titlePanel("RMS : Material"),
            helpText("Make sure to save for the update(s) you make. Each events are stored only at user side until user saves"),
            helpText("Material data table holds direct and indirect material cost and assumption note."),
            br(),
            
            #operation button settings
            fluidRow(
              column(12,
                     actionButton("add_button_mtrl", "Add", icon("plus")),
                     actionButton("edit_button_mtrl", "Edit", icon("edit")),
                     actionButton("copy_button_mtrl", "Copy", icon("copy")),
                     actionButton("delete_button_mtrl", "Delete", icon("trash-alt")),
                     actionButton("fetch_button_mtrl", "Fetch", icon("sync-alt"))
              )
            ),
            br(),
            
            #datatable
            fluidRow(
              column(12, #data table width
                     dataTableOutput("mtrl_table")
              )
            ),
            
            #shiny alert setting
            useShinyalert(),
            
            #save, download button
            fluidRow(
              column(12,
                     actionButton("save_button_mtrl", "Save", icon("save")),
                     downloadButton("download_button_mtrl", "Download", icon("download"))
              )
            )
            
          )
          
  )
