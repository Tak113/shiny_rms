tabItem_rms_equip <-
  tabItem(tabName = "equipment",
          
          fluidPage(
            
            shinyjs::useShinyjs(),
            
            titlePanel("PF RMS : Equipment"),
            helpText("Make sure to save for the update(s) you make. Each events are stored only at user side until user saves (eg. connection pool)"),
            
            br(),
            
            #Architecture selection pull down
            fluidRow(
              column(4,
                     uiOutput('ui_equip_selection')
              )
            ),
            
            br(),
            
            #operation button settings
            fluidRow(
              column(12,
                     actionButton("add_button_equip", "Add", icon("plus")),
                     actionButton("edit_button_equip", "Edit", icon("edit")),
                     actionButton("copy_button_equip", "Copy", icon("copy")),
                     actionButton("delete_button_equip", "Delete", icon("trash-alt")),
                     actionButton("fetch_button_equip", "Fetch", icon("sync-alt"))
              )
            ),
            br(),
            
            #datatable
            fluidRow(#width="100%",
              column(12,
                     dataTableOutput("equip_table")
              )
            ),
            
            #shiny alert setting
            useShinyalert(),
            
            #save, download button
            fluidRow(
              column(12,
                     actionButton("save_button_equip", "Save", icon("save")),
                     downloadButton("download_button_equip", "Download", icon("download"))
              )
            )
            
          )
          
  )
