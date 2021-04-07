
# Load tables -------------------------------------------------------------

#make masterTable_df reactive on the user event
masterTable_df <- reactiveValues()

#fetch data table
arc_masterTable <- dbReadTable(con, "arc") %>% select(ArcName,uuid_arc)
pb_masterTable <- dbReadTable(con, "procblock") %>% select(-ProcBlock_Note)
abm_masterTable <- dbReadTable(con, "arcblockmap")
equip_masterTable <- dbReadTable(con, "equip") %>% select(-MfgFomat)
bom_masterTable <- dbReadTable(con, "blockopsmap")
mtrl_masterTable <- dbReadTable(con, "mtrl") %>%  select(-MtrlFomat)


# Architecture Selection --------------------------------------------------

# render selectinput in UI
output$ui_master_selection <- renderUI({
  selectInput("master_selection",
              label = "Product;",
              choices = unique(arc_masterTable$ArcName),
              selectize = TRUE
  )
})


# Master Table Creation --------------------------------------------------

#baseline table creation
masterTable_df$data <-
  full_join(abm_masterTable, bom_masterTable, by="uuid_pb") %>%
  left_join(equip_masterTable, by="uuid_equip") %>%
  left_join(arc_masterTable, by="uuid_arc") %>%
  left_join(pb_masterTable, by="uuid_pb") %>%
  left_join(mtrl_masterTable, by="uuid_mtrl") %>%
  mutate(EquipCost=EquipCost_woIQ*(1+IQ)) %>% #EquipCost
  mutate(ToolReq = ToolReq(1000,xPH,GU)) %>% #ToolReq at 1kXSPW
  mutate(CapEx = ToolReq*EquipCost) %>%  #CapEx at 1kXSPW, this will be revised latesr to actual xSPW
  group_by(ArcName) %>%
  mutate(CapExRate = CapEx/sum(CapEx,na.rm=TRUE)) %>%  #add capex contribution percent. na.rm is let sum() calculate na data (otherwise sum() does not calculate and this column is blank)
  mutate(SeqID = row_number()) %>%  #add an numeric ID column by ArcName
  mutate(EquipSpace = MORa*SpaceScaler) %>%
  mutate(SpaceReq = ToolReq*EquipSpace) %>%
  mutate(DPN = DPN(CapEx)*(1+Spare_Maint*5)) %>%   #DPN, using 1kxSPW for a calc for 5yrs straight line, but vol does not matter as long as dpn is strucal(economy of scale) base. 5 for spare is conoverting from 5ys to 1 yr
  mutate(Labor = LaborCost(ToolReq, DLperTool, DL_IDL, 1000, TTE)) %>% #Labor, using 1kPSPW for a calc, but vol does not matter
  mutate(Mtrl = MtrlCost) %>% 
  mutate_each(funs(replace(.,is.na(.),0))) %>% # replace NA to 0
  mutate(CS_OH = CS_OH(Labor, Mtrl, CapEx, OH, Spare_Maint)) %>% # facility and overhead cost
  mutate(MU = MU(Labor, Mtrl, CS_OH, CapEx, Spare_Maint, ProcMU, CapExMU)) %>%
  mutate(TtlCost_MfgFomat = Mtrl+Labor+DPN+CS_OH+MU) %>%
  mutate(Contributions = TtlCost_MfgFomat/sum(TtlCost_MfgFomat,na.rm=TRUE))



# Render Table to UI --------------------------------------------------------

#store the currently filtered table in a reactive
interim_masterTable <- reactive({

  # filter column, sorting by preference
  masterTable_df$data <- masterTable_df$data[,c("SeqID","ArcName","ProcBlock","Ops","Rcs","EquipDescription","EquipGroup","MfgFomat",
                                                "xPH","GU","EquipCost","EquipSpace","MtrlName","DLperTool","ToolReq","CapEx","CapExRate",
                                                "SpaceReq", "Labor", "Mtrl", "DPN", "CS_OH", "MU", "TtlCost_MfgFomat","Contributions")]

  table <- masterTable_df$data

  # magnify by kXSPW, based on mfg format eitehr panel or wafer, or adjust decimal/significance
  table$CapEx <- table$CapEx*input$xSPW
  table$ToolReq <- table$ToolReq*input$xSPW
  table$SpaceReq <- round(table$SpaceReq*input$xSPW,digits=0)

  #sort by architecture
  table[table$ArcName %in% input$master_selection,]


})

output$masterTable <- DT::renderDataTable({

  dt_masterTable <- datatable(
    # masterTable_df$data,
    interim_masterTable(),
    selection = 'multiple',
    escape = FALSE,
    rownames = FALSE,
    extensions = 'Buttons', #for colvis
    # filter = 'top', #this will cause couple of issues on styling : product selection CSS not work and value box just shown up 1 but designed as 3
    options = list (
      lengthChange = TRUE,
      pageLength = 20,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = "colvis",
          text = "Columm Visibility"
        )
      ),
      columnDefs = list(
        list(
          targets = c(1,6,7,9,11,12,13,16,18:22,24), #has to be a number. number is column order from left starting from 0
          visible = FALSE
        )
      )
    )
  )

  dt_masterTable %>%
    formatCurrency(c('EquipCost','CapEx'), digit=0) %>%
    formatCurrency(c('DPN','Mtrl','Labor','CS_OH','MU','TtlCost_MfgFomat'),digit = 1) %>%
    formatPercentage(c('CapExRate'), digit=1) %>%
    formatPercentage('GU', digit=0) %>%
    formatRound(c('xPH','ToolReq','DLperTool'),digit=1) %>%
    formatRound(c('EquipSpace'), digit = 0) %>%
    formatString(c('SpaceReq','EquipSpace'),suffix = " SF") %>%
    formatString(c('DLperTool'), suffix = " /Day")

})


# ValueBox ----------------------------------------------------------------

#value box for CapEx
output$VBox_masterTable_CapEx <- renderValueBox({
  VB_MT_CapEx <- paste(signif(sum(interim_masterTable()$CapEx, na.rm=TRUE)/1000000,digit =3),"M$")
  valueBox(
    VB_MT_CapEx,"CapEx", icon=icon("chart-bar"), color="blue"
  )
})

#value box for Space
output$VBox_masterTable_Space <- renderValueBox({
  VB_MT_Space <- paste(signif(sum(interim_masterTable()$SpaceReq, na.rm=TRUE)/1000,digit =3),"KSF")
  valueBox(
    VB_MT_Space,"Space", icon=icon("chart-bar"), color="blue"
  )
})

#value box for Panel Cost
output$VBox_masterTable_xSPW <- renderValueBox({
  VB_MT_Panel <- paste(signif(sum(interim_masterTable()$TtlCost_MfgFomat, na.rm=TRUE),digit =3),"$")
  valueBox(
    VB_MT_Panel,"Cost per Mfg Fomat", icon=icon("chart-bar"), color="blue"
  )
})


# Download Button ---------------------------------------------------------
output$download_button_masterTable <- downloadHandler(
  filename = function() {
    paste("PF_DB_masterTable_", Sys.Date(), ".csv", sep="") #sep="" is no space between words (in this case PFxx and date)
  },
  content = function(file) {
    write.csv(interim_masterTable(), file, row.names=F)
  }
)


# echarts -----------------------------------------------------------------

output$donut_xx <- renderEcharts4r({
  interim_masterTable() %>% 
    e_charts(ProcBlock) %>% 
    e_pie(Contributions, radius = c("30%", "55%")) %>% 
    e_tooltip(
      trigger = 'item',
      textStyle = list(
        color = '#fff'
      ),
      formatter = e_tooltip_pie_formatter("percent")
    ) %>% 
    e_theme('red-velvet')
})


output$donut_block <- renderEcharts4r({
  interim_masterTable() %>%
    gather(costblock, cost, Labor, Mtrl, DPN, CS_OH, MU) %>%
    group_by(costblock) %>%
    summarise(sum=sum(cost)) %>%
    mutate(ratio = sum/sum(sum)) %>%
    e_charts(costblock) %>%
    e_pie(ratio, radius = c("30%", "55%")) %>%
    e_tooltip(
      trigger = 'item',
      textStyle = list(
        color = '#fff'
      ),
      formatter = e_tooltip_pie_formatter("percent")
    ) %>%
    e_theme('tech-blue')
})

output$pareto <- renderEcharts4r({
  interim_masterTable() %>%
    group_by(Rcs) %>%
    summarise(sum=sum(TtlCost_MfgFomat),
              DPN=sum(DPN),
              Labor=sum(Labor),
              Mtrl=sum(Mtrl),
              MU=sum(MU),
              CS_OH=sum(CS_OH)) %>%
    slice_max(sum, n=20) %>%  #top 20
    arrange(sum) %>%
    e_charts(Rcs) %>%
    e_bar(DPN, stack = "grp") %>%
    e_bar(Labor, stack = "grp") %>%
    e_bar(Mtrl, stack = "grp") %>%
    e_bar(MU, stack = "grp") %>%
    e_bar(CS_OH, stack = "grp") %>%
    # e_labels(position = "right") %>%
    e_flip_coords() %>%
    e_x_axis(
      formatter = e_axis_formatter('currency')
    ) %>%
    # e_y_axis(
    #   axisLabel = list(interval = 0, rotate =45),
    #   margin = 100
    # ) %>%
    e_tooltip(
      axisPointer = list(
        type='cross'
      ),
      trigger = 'axis',
      backgroundColor = 'rgba(0,0,0,0.75)',
      borderColor = 'rgba(0,0,0,0.75)',
      textStyle = list(
        color = '#fff'
      )
    ) %>%
    e_theme('tech-blue')
})
