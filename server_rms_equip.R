#enable shiny alert
useShinyalert()

#make equip_df reactive on the user event
equip_df <- reactiveValues()

mf_name_selection_equip <- dbReadTable(con,'procblock') %>% select(-c(ProcBlock_Note,ProcBlock,uuid_pb))
equip_df$data <- dbReadTable(con, "equip")

# render selectinput in UI ------------------------------------------------
#in shiny app, the ui function runs before the server function. need to move to server making this as reactive expression
output$ui_equip_selection <- renderUI({
  selectInput("equip_selection",
              label = "Manufacturing Format;",
              choices = c("All", unique(mf_name_selection_equip$MfgFomat)),
              selected = "All",
              multiple = TRUE)
})



# form prep ---------------------------------------------------------------

#Form function for data entry, to be called in both 'add' and 'edit' event
#button_id is a variable and button id which will navigate to each event
entry_form_equip <- function(button_id) {

  showModal(
    modalDialog(
      title = strong("Entry Form"),
      fluidRow(
        column(6,
               textInput("Rcs", "Rcs", placeholder = ""),
               textInput("EquipDescription", "EquipDescription", placeholder = ""),
               textInput("EquipGroup", "EquipGroup", placeholder = ""),
               textInput("Vender", "Vender", placeholder = ""),
               textInput("Model", "Model", placeholder = "")
        ),
        column(6,
               selectInput("MfgFomat","MfgFomat",choices = c("",unique(as.character(mf_name_selection_equip$MfgFomat)))),
               numericInput("EquipCost_woIQ", "EquipCost_woIQ", value = 1000000, min = 0),
               numericInput("MORa", "MORa", value = 500, min = 0),
               numericInput("SpaceScaler", "SpaceScaler", value = 1.5, min = 0),
               numericInput("GU", "GU", value = 0.8, min = 0),
               numericInput("DLperTool", "DLperTool", value = 1, min = 0),
        ),
        column(6,
               textAreaInput("Equip_Note","Equip_Note", placeholder = "put assumption such as equipment configuration")
        )
      ),
      footer = tagList(
        actionButton(button_id, "Submit"),
        modalButton("Cancel")
      ),
      size = "m", #window size. m:medium
      easyClose = TRUE
    )
  )

}

#Save function for form data into data frame format from form function
formData_equip <- reactive({

  formData_equip <- data.frame(uuid_equip = UUIDgenerate(),
                         Rcs = input$Rcs,
                         EquipDescription = input$EquipDescription,
                         EquipGroup = input$EquipGroup,
                         Vender = input$Vender,
                         Model = input$Model,
                         MfgFomat = input$MfgFomat,
                         EquipCost_woIQ = input$EquipCost_woIQ,
                         MORa = input$MORa,
                         SpaceScaler = input$SpaceScaler,
                         GU = input$GU,
                         DLperTool = input$DLperTool,
                         Equip_Note = input$Equip_Note
  ) %>%
  mutate_each(funs(as.numeric), c(EquipCost_woIQ, MORa, SpaceScaler, GU, DLperTool)) %>% #match variable type to original database
  mutate_each(funs(as.character), c(uuid_equip, Rcs, EquipDescription, EquipGroup, Vender, Model, MfgFomat, Equip_Note)) #match variable type to original database

  #paste0(str(formData))
  return(formData_equip)

})


# Add ---------------------------------------------------------------------

#Add data button
observeEvent(input$add_button_equip, {
  entry_form_equip("submit_new")
})
observeEvent(input$submit_new, {
  equip_df$data <- bind_rows(equip_df$data, formData_equip()) #add new record at bottom
  shinyjs::reset("entry_form_equip")
  removeModal()
})


# Delete ------------------------------------------------------------------

observeEvent(input$delete_button_equip, {
  showModal(
    if(length(input$equip_table_rows_selected)>=1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Are you sure you would like to delete selected row(s)?"),
        footer = tagList(
          actionButton("submit_delete", "Yes"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      )
    } else if(length(input$equip_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select row(s)"),
        easyClose = TRUE
      )
    }
  )
})
observeEvent(input$submit_delete, {
  uuid_selection_equip <- interim_table_equip()[input$equip_table_rows_selected, "uuid_equip"] #identify uuid for the selected row
  uuid_index_equip <- match(uuid_selection_equip, equip_df$data$uuid_equip) #get matched row index using UUID
  equip_df$data <- equip_df$data[-uuid_index_equip,] #delete row
  removeModal()

})


# Copy --------------------------------------------------------------------

observeEvent(input$copy_button_equip, {
  if(length(input$equip_table_rows_selected)>=1) {
    row_selection <- interim_table_equip()[input$equip_table_rows_selected, "uuid_equip"] #identify uuid for the selected rows
    copy_df <- equip_df$data %>% filter(uuid_equip %in% row_selection) #filter by selected uuid rows
    copy_df$uuid_equip <- replicate(nrow(copy_df), UUIDgenerate()) #replace with new uuid for copied rows
    equip_df$data <- bind_rows(equip_df$data, copy_df) #add copy records at bottom

  } else if(length(input$equip_table_rows_selected)<1) {
    showModal(
      modalDialog(
        h3(strong("Warning")),
        paste("Please select row(s)"),
        easyClose = TRUE
      )
    )
  }
})

# Edit --------------------------------------------------------------------

#modal window. update form based on a selected row
observeEvent(input$edit_button_equip, {

  showModal(
    if(length(input$equip_table_rows_selected)>1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select only one row"),
        easyClose = TRUE
      )
    } else if(length(input$equip_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select a row"),
        easyClose = TRUE
      )
    }
  )

  if(length(input$equip_table_rows_selected)==1) {

    #load entry form
    entry_form_equip("submit_edit_equip")

    #update each value in entyr form based on selected row
    updateTextInput(session, "Rcs", value = interim_table_equip()[input$equip_table_rows_selected, "Rcs"])
    updateTextInput(session, "EquipDescription", value = interim_table_equip()[input$equip_table_rows_selected, "EquipDescription"])
    updateTextInput(session, "EquipGroup", value = interim_table_equip()[input$equip_table_rows_selected, "EquipGroup"])
    updateTextInput(session, "Vender", value =interim_table_equip()[input$equip_table_rows_selected, "Vender"])
    updateTextInput(session, "Model", value = interim_table_equip()[input$equip_table_rows_selected, "Model"])
    updateSelectInput(session, "MfgFomat", selected = interim_table_equip()[input$equip_table_rows_selected, "MfgFomat"],
                      choices = unique(as.character(mf_name_selection_equip$MfgFomat)))
    updateNumericInput(session, "EquipCost_woIQ", value = interim_table_equip()[input$equip_table_rows_selected, "EquipCost_woIQ"])
    updateNumericInput(session, "MORa", value = interim_table_equip()[input$equip_table_rows_selected, "MORa"])
    updateNumericInput(session, "SpaceScaler", value = interim_table_equip()[input$equip_table_rows_selected, "SpaceScaler"])
    updateNumericInput(session, "GU", value = interim_table_equip()[input$equip_table_rows_selected, "GU"])
    updateNumericInput(session, "DLperTool", value = interim_table_equip()[input$equip_table_rows_selected, "DLperTool"])
    updateTextAreaInput(session, "Equip_Note", value = interim_table_equip()[input$equip_table_rows_selected, "Equip_Note"])

  }
})

#update to database
observeEvent(input$submit_edit_equip, {
  uuid_selection_equip <- interim_table_equip()[input$equip_table_rows_selected, "uuid_equip"] #identify uuid for the selected row
  uuid_index_equip <- match(uuid_selection_equip, equip_df$data$uuid_equip) #get matched row index using UUID
  equip_df$data[uuid_index_equip, 1:13] <- formData_equip()[,1:13] #replace data with updated data. make sure column order matches. uuid_equip is 1st column in formData while last column in database
  shinyjs::reset("entry_form_equip")
  removeModal()
})

# Fetch -------------------------------------------------------------------

observeEvent(input$fetch_button_equip, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Latest data from each equipment data table are used to recreate full equipment catalogue. Do you still want to continue?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_refresh_equip", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
observeEvent(input$ok_refresh_equip, {
  removeModal()
  mf_name_selection_equip <- dbReadTable(con,'procblock') %>% select(-c(ProcBlock_Note,ProcBlock,uuid_pb))
  equip_df$data <- dbReadTable(con, "equip")
})


# Render Table ------------------------------------------------------------

#store the currently filtered table in a reactive. This interim is needed for select input
interim_table_equip <- reactive({

  table <- equip_df$data
  if (!"All" %in% input$equip_selection){
    return(table[table$MfgFomat %in% input$equip_selection,])
  }else{
    return(table)
  }
  table
})

output$equip_table <- DT::renderDataTable({
  
  # table <- interim_table_equip() %>% select(-uuid_equip) #remove UUID
  table <- equip_df$data %>% select(-uuid_equip) #remove UUID
  dt <- datatable(
    table,
    filter = 'top',
    selection = 'multiple',
    escape = FALSE,
    rownames = FALSE,
    extensions = 'Buttons', # for colvis
    options = list (
      pageLength = 20,
      lengthChange = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'colvis',
          text = 'Column Visibility'
        )
      ),
      columnDefs = list(
        list(
          targets = c(4,8,11), # has to be a number, number is column ordre from left from 0
          visible = FALSE
        )
      )
    )
  )
  dt %>%
    formatCurrency('EquipCost_woIQ',digit=0) %>%
    formatPercentage(c('GU'),digit=0) %>% 
    formatRound(c('SpaceScaler', 'DLperTool'), digit=1) %>%
    formatRound('MORa', digit=0) %>% 
    formatString(c('DLperTool'),suffix = " /Day") %>% 
    formatString(c('MORa'),suffix = " SF")
  
})



# Save to database --------------------------------------------------------

#confirmation window
observeEvent(input$save_button_equip, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Are you sure to update equipment datatable pulling latest equipment data table?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_save_equip", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
#save to database
observeEvent(input$ok_save_equip, {
  dbWriteTable(con, "equip", equip_df$data, overwrite = T)
  removeModal()
  shinyalert(title = "Saved to database", type = "success")
})



# Download ----------------------------------------------------------------

output$download_button_equip <- downloadHandler(
  filename = function() {
    paste("PF_DB_Equipment_", Sys.Date(), ".csv", sep="") #sep="" is no space between words (in this case PFxx and date)
  },
  content = function(file) {
    write.csv(equip_df$data, file, row.names=F)
  }
)