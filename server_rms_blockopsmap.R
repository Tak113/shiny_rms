#enable shiny alert
useShinyalert()

#make bom_df reactive on the user event
bom_df <- reactiveValues()

#create data under bom_df and load "equip" database
pb_name_selection_bom <- dbReadTable(con, "procblock") %>% select(ProcBlock, uuid_pb) #for a selection
rcs_name_selection_bom <- dbReadTable(con, "equip") %>% select(Rcs,uuid_equip) #for a selection
mtrl_name_selection_bom <- dbReadTable(con, "mtrl") %>% select(MtrlName,uuid_mtrl) #for a selection
bom_df$data <- dbReadTable(con, "blockopsmap") %>%  # for a db update
  left_join(pb_name_selection_bom, by="uuid_pb") %>%  #add ProcBlock from uuid_pb
  left_join(rcs_name_selection_bom, by="uuid_equip") %>%  #add Rcs from uuid_equip
  left_join(mtrl_name_selection_bom, by="uuid_mtrl") #add mtrl from uuid_mtrl


# render selectinput in UI ------------------------------------------------
#in shiny app, the ui function runs before the server function. need to move to server making this as reactive expression
output$ui_bom_selection <- renderUI({
  selectInput("bom_selection",
              label = "Process Block;",
              choices = c("All", unique(pb_name_selection_bom$ProcBlock)),
              selected = "All",
              multiple = TRUE)
})

# form prep ---------------------------------------------------------------


#Form function for data entry, to be called in both 'add' and 'edit' event
#button_id_bom is a variable and button id which will navigate to each event
entry_form_bom <- function(button_id_bom) {
  
  showModal(
    modalDialog(
      title = strong("Entry Form"),
      fluidRow(
        column(6,
               numericInput("Seq_Ops", "Operation Sequence", value = "", min = 0),
               selectInput("ProcBlock", "ProcBlock", choices = c("",unique(as.character(pb_name_selection_bom$ProcBlock)))), 
               textInput("Ops", "Operation", placeholder = ""), 
               selectInput("Rcs", "Rcs", choices = c("",unique(as.character(rcs_name_selection_bom$Rcs)))),
               numericInput("xPH", "xPH", value = "", min = 0), 
               selectInput("MtrlName", "MtrlName", choices = c("",unique(as.character(mtrl_name_selection_bom$MtrlName)))), 
               textAreaInput("Ops_Note", "Ops_Note", placeholder = "put operation recipe assumption such as overplate thickness and plating time") 
        )
      ),
      footer = tagList(
        actionButton(button_id_bom, "Submit"),
        modalButton("Cancel")
      ),
      size = "m", #window size. m:medium
      easyClose = TRUE
    )
  )
  
}

#Save function for form data into data frame format from form function
formData_bom <- reactive({
  
  formData_bom <- data.frame(
    uuid_bom = UUIDgenerate(),
    Seq_Ops = input$Seq_Ops,
    ProcBlock = input$ProcBlock,
    Ops = input$Ops,
    Rcs = input$Rcs,
    xPH = input$xPH,
    MtrlName = input$MtrlName,
    Ops_Note = input$Ops_Note
  ) %>% 
    mutate_each(funs(as.numeric), c(Seq_Ops, xPH)) %>% #match variable type to original database
    mutate_each(funs(as.character), c(uuid_bom, ProcBlock, Ops, Rcs, Ops_Note, MtrlName)) %>%  #match variable type to original database
    left_join(pb_name_selection_bom, by="ProcBlock") %>% #add uuid_pb from ProcBlock
    left_join(rcs_name_selection_bom, by="Rcs") %>%  #add uuid_equip from Rcs
    left_join(mtrl_name_selection_bom, by="MtrlName")  #add uuid_mtrl from MtrlName
  
  # formData_bom <- formData_bom[,c("uuid_bom","Seq_Ops","ProcBlock","Ops", "Rcs","xPH","MtrlName", "Ops_Note","uuid_pb","uuid_equip")] #reorder to follow original data frame, with ProcBlock and Rcs
  
  #paste0(str(formData_bom))
  return(formData_bom)
  
})


# Add ---------------------------------------------------------------------

#Add data button
observeEvent(input$add_button_bom, {
  entry_form_bom("submit_new_bom")
})
observeEvent(input$submit_new_bom, {
  bom_df$data <- bind_rows(bom_df$data, formData_bom()) #add new record at bottom
  shinyjs::reset("entry_form_bom")
  removeModal()
})  


# Delete ------------------------------------------------------------------

observeEvent(input$delete_button_bom, {
  showModal(
    if(length(input$bom_table_rows_selected)>=1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Are you sure you would like to delete selected row(s)?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submit_delete_bom", "Yes")
        ),
        easyClose = TRUE
      ) 
    } else if(length(input$bom_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select row(s)"),
        easyClose = TRUE
      )
    }
  )
})

observeEvent(input$submit_delete_bom, {
  uuid_selection_bom <- interim_table_bom()[input$bom_table_rows_selected, "uuid_bom"] #identify uuid for the selected row
  uuid_index_bom <- match(uuid_selection_bom, bom_df$data$uuid_bom) #get matched row index using UUID
  bom_df$data <- bom_df$data[-uuid_index_bom,] #delete row
  # cat(file=stderr(), "test", uuid_index_bom, "\n")
  removeModal()
  # DT::replaceData(proxy, interim_table_bom(), resetPaging = FALSE)
})

# Copy --------------------------------------------------------------------

observeEvent(input$copy_button_bom, {
  if(length(input$bom_table_rows_selected)>=1) {
    row_selection_bom <- interim_table_bom()[input$bom_table_rows_selected, "uuid_bom"] #identify uuid for the selected rows
    copy_df_bom <- bom_df$data %>% filter(uuid_bom %in% row_selection_bom) #filter by selected uuid rows
    copy_df_bom$uuid_bom <- replicate(nrow(copy_df_bom), UUIDgenerate()) #replace with new uuid for copied rows
    bom_df$data <- bind_rows(bom_df$data, copy_df_bom) #add copy records at bottom
  } else if(length(input$bom_table_rows_selected)<1) {
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
observeEvent(input$edit_button_bom, {
  
  showModal(
    if(length(input$bom_table_rows_selected)>1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select only one row"),
        easyClose = TRUE
      )
    } else if(length(input$bom_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select a row"),
        easyClose = TRUE
      )
    }
  )
  
  if(length(input$bom_table_rows_selected)==1) {
    
    #load entry form
    entry_form_bom("submit_edit_bom")
    
    #update each value in entry form based on selected row
    updateNumericInput(session, "Seq_Ops", value = interim_table_bom()[input$bom_table_rows_selected, "Seq_Ops"])
    updateSelectInput(session, "ProcBlock", selected = interim_table_bom()[input$bom_table_rows_selected, "ProcBlock"],
                      choices = c("",unique(as.character(pb_name_selection_bom$ProcBlock))))
    updateTextInput(session, "Ops", value = interim_table_bom()[input$bom_table_rows_selected, "Ops"])
    updateSelectInput(session, "Rcs", selected = interim_table_bom()[input$bom_table_rows_selected, "Rcs"],
                      choices = c("",unique(as.character(rcs_name_selection_bom$Rcs))))
    updateNumericInput(session, "xPH", value = interim_table_bom()[input$bom_table_rows_selected, "xPH"])
    updateSelectInput(session, "MtrlName", selected = interim_table_bom()[input$bom_table_rows_selected, "MtrlName"],
                      choices = c("",unique(as.character(mtrl_name_selection_bom$MtrlName))))
    updateTextAreaInput(session, "Ops_Note", value = interim_table_bom()[input$bom_table_rows_selected, "Ops_Note"])
    
  }
})

#update to database
observeEvent(input$submit_edit_bom, {
  uuid_selection_bom <- interim_table_bom()[input$bom_table_rows_selected, "uuid_bom"] #identify uuid for the selected row
  uuid_index_bom <- match(uuid_selection_bom, bom_df$data$uuid_bom) #get matched row index using UUID
  # cat(file=stderr(), "test", bom_df$data[uuid_index_bom,1], "\n")
  bom_df$data[uuid_index_bom, 2:11] <- formData_bom()[,2:11] #replace data with updated data. key is uuid_bom
  shinyjs::reset("entry_form_bom")
  removeModal()
})

# Fetch -------------------------------------------------------------------

observeEvent(input$fetch_button_bom, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Any unsaved event will be lost. Do you still want to refresh?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_refresh_bom", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
observeEvent(input$ok_refresh_bom, {
  removeModal()
  #create data under bom_df and load "equip" database
  pb_name_selection_bom <- dbReadTable(con, "procblock") %>% select(ProcBlock, uuid_pb) #for a selection
  rcs_name_selection_bom <- dbReadTable(con, "equip") %>% select(Rcs,uuid_equip) #for a selection
  mtrl_name_selection_bom <- dbReadTable(con, "mtrl") %>% select(MtrlName,uuid_mtrl) #for a selection
  bom_df$data <- dbReadTable(con, "blockopsmap") %>%  # for a db update
    left_join(pb_name_selection_bom, by="uuid_pb") %>%  #add ProcBlock from uuid_pb
    left_join(rcs_name_selection_bom, by="uuid_equip") %>%  #add Rcs from uuid_equip
    left_join(mtrl_name_selection_bom, by="uuid_mtrl") #add mtrl from uuid_mtrl
})


# Render Table ------------------------------------------------------------

#store the currently filtered table in a reactive. This interim is needed for select input
interim_table_bom <- reactive({
  
  bom_df$data <- bom_df$data[,c("uuid_bom","Seq_Ops","ProcBlock","Ops","Rcs","xPH","MtrlName","Ops_Note","uuid_pb","uuid_equip","uuid_mtrl")] #order change for UI
  
  table <- bom_df$data 
  if (!"All" %in% input$bom_selection){
    return(table[table$ProcBlock %in% input$bom_selection,])
  }else{
    return(table)
  }
  table
})

output$bom_table <- DT::renderDataTable({
  #this is for debugging. this won't work for datatable but work for a single data
  #cat(file=stderr(), "input$bom_selection includes", input$bom_selection, "\n")
  interim_table_bom() %>%
    select(-uuid_bom, -uuid_equip, -uuid_pb, -uuid_mtrl) #remove UUID 
  },
  rownames = FALSE,
  server = TRUE,
  options = list (
    pageLength = 20
  )
)


# Save to database --------------------------------------------------------

#confirmation window
observeEvent(input$save_button_bom, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Are you sure to save change(s) you made into database?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_save_bom", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
#save to database
observeEvent(input$ok_save_bom, {
  data_for_save_bom <- bom_df$data %>% select(-Rcs, -ProcBlock, -MtrlName)
  data_for_save_bom <- data_for_save_bom[order(data_for_save_bom$uuid_pb,data_for_save_bom$Seq_Ops),]
  dbWriteTable(con, "blockopsmap", data_for_save_bom, overwrite = T)
  removeModal()
  shinyalert(title = "Saved to database", type = "success")
})  



# Download ----------------------------------------------------------------

output$download_button_bom <- downloadHandler(
  filename = function() {
    paste("PF_DB_BlocOpskMap_", Sys.Date(), ".csv", sep="") #sep="" is no space between words (in this case PFxx and date)
  },
  content = function(file) {
    write.csv(bom_df$data, file, row.names=F)
  }
)