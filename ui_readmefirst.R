tabItem_readmefirst <- 
  tabItem(tabName = "readmefirst",
          h2("Read me first"),
          fluidRow(
            column(4,
                   wellPanel(
                     tags$li(tags$a(href = "#Overview", "Overview"),style="list-style-type:square;"),
                     tags$li(tags$a(href = "#DataProductContents", "Data Product Contents"),style="list-style-type:square;"),
                     tags$li(tags$a(href = "#DevTools", "Dev Tools"),style="list-style-type:square;")
                   )
            ),
          ),
          br(),
          
          
          # Description -------------------------------------------------------------
          tags$hr(),
          h3("Overview", id="Overview"),
          tags$p(
            h5("RM (Record Management) is an integrated system designed for modeling SOR(system of record) of product manufacturing.
               in terms of operations resarch KPIs. Main output is capital investment, required space, and cost per manufacturing format for each product scenario,
               This product SoR is also used for other factory DX project. See link for additinal factory DX apps.")
          ),
          tags$p(
            h5("All inputs are assumed SEMI E10 standard definition. Financial assumptions(labor cost, direct/indirect ratio, etc.) are hard coded")
          ),
          br(),
          
          
          # Data Products ----------------------------------------------------------
          tags$hr(),
          h3("Data Product Contents", id = "DataProductContents"),
          h5("This application folds 2 main parts - Data production(Input : Record Management System) and Data analytics(Output : DP Applications)"),
          tags$ul(style="list-style-type:square",
                  tags$li(tags$b("RMS (Records Management System) : Input"),
                          tags$ul(style = "list-style-type:circle",
                                  tags$li("Product : ", tags$br(), "Product options and note for each recipe. This is a scenario for an output"),
                                  tags$li("Process Block : ", tags$br(), "High level process block to create a product"),
                                  tags$li("Arc Block Map : ", tags$br(), "Mapping table for product and process block. You will see high level process flow agasint each architecture"),
                                  tags$li("Equipment : ", tags$br(), "Equipment related database. It holds 3digit code name for each equipment, vender/model ID/equipment configurations, cost, space, and labor"),
                                  tags$li("Block Ops Map : ", tags$br(), "Mapping table for process block, operations, resources, xPH, and Material cost"),
                                  tags$li("Material : ", tags$br(), "Material related databse. It holds direct/indirect material cost")
                          )
                  ),
                  tags$li(tags$b("Business Intelligence : Output"),
                          tags$ul(style = "list-style-type:circle",
                                  tags$li("Master Table : ", tags$br(),
                                  "Structural output by product scenario. Output by operation. Ouputs are capex,
                        space, unit cost, and manufacturing fomat cost (eg. wafer cost).
                                  2 "),
                                  tags$li("Drilldown : ", tags$br(),
                                          "Drill down visualization to identify key contributions on the target output.
                                  2 ")
                  )
          ),
          br(),
          
          # System Specification ----------------------------------------------------
          tags$hr(),
          h3("Link to Dev Tools", id = "DevTools"),
          tags$ul(style = "list-style-type:square;",
                  tags$li(
                    tags$a(href="https://github.com/Tak113/shiny_rms", "Github : version control")
                  ),
                  tags$li(
                    tags$a(href="https://shinyapps.io","Shinyapps.io : PaaS(hosting server)")
                  )
          )
       
          
          
  )