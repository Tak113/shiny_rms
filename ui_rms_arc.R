tabItem_rms_arc <-
  tabItem(tabName = "architecture",
          
          fluidPage(
            
            shinyjs::useShinyjs(),
            # shinyjs::inlineCSS(appCSS),
            
            titlePanel("RMS : Product"),
            helpText("Make sure to save for the update(s) you make. Each events are stored only at user side until user saves"),
            helpText("Product data table holds product options and note for each product scenario"),
            br(),
            
            #operation button settings
            fluidRow(
              column(12,
                     actionButton("add_button_arc", "Add", icon("plus")),
                     actionButton("edit_button_arc", "Edit", icon("edit")),
                     actionButton("copy_button_arc", "Copy", icon("copy")),
                     actionButton("delete_button_arc", "Delete", icon("trash-alt")),
                     actionButton("fetch_button_arc", "Fetch", icon("sync-alt"))
              )
            ),
            br(),
            
            #datatable
            fluidRow(
              column(12, #data table width
                     dataTableOutput("arc_table")
              )
            ),
            
            #shiny alert setting
            useShinyalert(),
            
            #save, download button
            fluidRow(
              column(12,
                     actionButton("save_button_arc", "Save", icon("save")),
                     downloadButton("download_button_arc", "Download", icon("download"))
              )
            )
            
          )
          
  )
