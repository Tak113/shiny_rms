
# load sub files for each pages -------------------------------------------
source('ui_readmefirst.R', local = TRUE)
source('ui_rms_arc.R', local = TRUE)
source('ui_rms_procblock.R', local = TRUE)
source('ui_rms_arcblockmap.R', local = TRUE)
source('ui_rms_equip.R', local = TRUE)
source('ui_rms_blockopsmap.R', local = TRUE)
source('ui_rms_mtrl.R', local = TRUE)
source('ui_app_master.R', local = TRUE)

# dashboard component -----------------------------------------------------
header <- dashboardHeader(title = "RM", titleWidth = "250px")

sidebar <- dashboardSidebar(width = "250px", collapsed = TRUE,
                            sidebarMenu(
                              
                              menuItem("Read me first", tabName = 'readmefirst', icon = icon("home")),
                              menuItem("Record Management", icon = icon("database"),
                                       menuSubItem("Product", tabName = 'architecture', icon = icon("table")),
                                       menuSubItem("Process Block", tabName = 'procblock', icon = icon("table")),
                                       menuSubItem("Prod Block Map", tabName = 'arcblockmap', icon = icon("table")),
                                       menuSubItem("Equipment", tabName = 'equipment', icon = icon("table")),
                                       menuSubItem("Block Ops Map", tabName = 'blockopsmap', icon = icon("table")),
                                       menuSubItem("Material", tabName = 'material', icon = icon("table"))
                              ),
                              menuItem("Business Intelligence", tabName = 'mastertable', icon = icon("chart-line")),
                              menuItem("Link", icon = icon("link"),
                                       menuSubItem("Factory Dashboard", icon = icon("calculator"),href="https://datak.biz"),
                                       menuSubItem("Factory Ops", icon = icon("table"),href="https://datak.biz")
                              )
                            )
)

body <- dashboardBody(
  tabItems(
    
    tabItem_readmefirst,
    tabItem_rms_arc,
    tabItem_rms_procblock,
    tabItem_rms_abm,
    tabItem_rms_equip,
    tabItem_rms_bom,
    tabItem_rms_mtrl,
    tabItem_app_master
    
  ),
  
  #theme
  use_theme(
    create_theme(
      adminlte_global(
        content_bg = "#fafafa" #background color in body
      ),
      adminlte_sidebar(
        dark_bg = "#798e99", #background color at side bar
        dark_hover_bg = "#355667", #hover color at side bar
        dark_submenu_hover_color = "#355667" #hover text color at side bar submenu
      ),
      adminlte_color(
        blue = "#798e99", #light blue
        green = "#4f7359",
        red = "#ba6667",
        purple = "#355667", #this 'purple' is used for a skin. this is dark blue
        yellow = "#a8ab5b",
        black = "#737373"
      )
    )
  ),
  
  #mini sidebar prep
  useShinyjs(),
  tags$head({tags$style(HTML("@media (min-width: 768px) {
                                            .content-wrapper,
                                            .right-side,
                                            .main-footer {
                                                margin-left: 230px;
                                            }
                                        }
                                        @media (max-width: 767px) {
                                            .sidebar-open .content-wrapper,
                                            .sidebar-open .right-side,
                                            .sidebar-open .main-footer {
                                                -webkit-transform: translate(250px, 0);
                                                -ms-transform: translate(250px, 0);
                                                -o-transform: translate(250px, 0);
                                                transform: translate(250px, 0);
                                            }
                                        }
                                        @media (max-width: 767px) {
                                            .main-sidebar,
                                            .left-side {
                                                -webkit-transform: translate(-250px, 0);
                                                -ms-transform: translate(-250px, 0);
                                                -o-transform: translate(-250px, 0);
                                                transform: translate(-250px, 0);
                                            }
                                        }
                                        @media (min-width: 768px) {
                                            .sidebar-collapse .main-sidebar,
                                            .sidebar-collapse .left-side {
                                                -webkit-transform: translate(-250px, 0);
                                                -ms-transform: translate(-250px, 0);
                                                -o-transform: translate(-250px, 0);
                                                transform: translate(-250px, 0);
                                            }
                                        }"))})
  
)


# dashboard framing -------------------------------------------------------
ui <- dashboardPage(skin = "purple", header, sidebar, body)

