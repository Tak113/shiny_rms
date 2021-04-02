tabItem_rms_abm <-
  tabItem(tabName = "arcblockmap",
          
          fluidPage(
            
            shinyjs::useShinyjs(),
            # shinyjs::inlineCSS(appCSS),
            
            titlePanel("RMS : Product - Process Block Map"),
            helpText("Make sure to save for the update(s) you make. Each events are stored only at user side until user saves (eg. connection pool)"),
            helpText("Prod Block Map data table holds process block by product recipe options"),
            br(),
            
            #Architecture selection pull down
            fluidRow(
              column(4,
                     uiOutput('ui_abm_selection')
              )
            ),
            
            br(),
            
            #operation button settings
            fluidRow(
              column(12,
                     actionButton("add_button_abm", "Add", icon("plus")),
                     actionButton("edit_button_abm", "Edit", icon("edit")),
                     actionButton("copy_button_abm", "Copy", icon("copy")),
                     actionButton("delete_button_abm", "Delete", icon("trash-alt")),
                     actionButton("fetch_button_abm", "Fetch", icon("sync-alt"))
              )
            ),
            br(),
            
            #datatable
            fluidRow(
              column(12, #data table width
                     dataTableOutput("abm_table")
              )
            ),
            
            #shiny alert setting
            useShinyalert(),
            
            #save, download button
            fluidRow(
              column(12,
                     actionButton("save_button_abm", "Save", icon("save")),
                     downloadButton("download_button_abm", "Download", icon("download"))
              )
            )
            
          )
          
  )
