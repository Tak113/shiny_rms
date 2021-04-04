tabItem_app_master <- 
  tabItem(tabName = "mastertable",
    
    fluidPage(
      
          h2("RMS Output"),
          h5("Output by product recipe for high level stats and drill down."),
          
          br(),
          
          #Architecture selection pull down
          fluidRow(
            box(title = 'Input for Master Table and High Level Stats', width=12,
              column(4,
                uiOutput('ui_master_selection')
              ),
              column(8,
                sliderInput("xSPW", "Mfg Fomat Starts Per Week (k xSPW)", min=0, max=20, value=1, step=0.25)
              )
            ) #box
          ), #fluidRow,
          
          h4('High Level Stats'),
          fluidRow(
            valueBoxOutput("VBox_masterTable_CapEx", width = 4),
            valueBoxOutput("VBox_masterTable_Space", width = 4),
            valueBoxOutput("VBox_masterTable_xSPW", width = 4)
          ),
          
          br(),
          
          #Tabset
          tabsetPanel(type='tabs',
            tabPanel('Structural Table',
              
              br(),
              h4('Master Table'),
              h5('Structurally breaking down to each operations/flow. No step function is used for caped (tool count), but fractional'),
                     
              #datatable
              fluidRow(
                column(12, #data table width
                  dataTableOutput("masterTable")
                )
              ),
              
              #save, download button
              fluidRow(
                column(12,
                  downloadButton("download_button_masterTable", "Download", icon("download"))
                )
              )  
            ), #tabPanel1
            tabPanel('Drilldown',
              
              br(),
              h4('Drill Down'),
              h5('Drill down for exploretoly data analysis')
              
                    
                     
            ) #tabPanel2
          ) #tabSetPanel
    ) #fluidPage
  )
