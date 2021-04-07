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
            tabPanel('Drilldown',
              
              br(),
              h4('Drill Down'),
              h5('For exploretoly data analysis'),
              br(),
              
              fluidRow(
                column(5,
                  column(12,
                    h4('By process block'),
                    echarts4rOutput('donut_xx', height=400),
                  ),
                  br(),
                  column(12,
                    h4('By cost block'),
                    echarts4rOutput('donut_block', height=400)
                  )
                ),
                column(7,
                  h4('Pareto'),
                  h5('Top 20 pareto, by combination of cost block/process block and equipment/operation'),
                  echarts4rOutput('pareto', height=800)
                )
              )
                    
                     
            ), #tabPanel2
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
            ) #tabPanel1
          ) #tabSetPanel
    ) #fluidPage
  )
