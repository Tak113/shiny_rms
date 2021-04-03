#enable shiny alert
useShinyalert()

#make mtrl_df reactive on the user event
mtrl_df <- reactiveValues()

#create data under mtrl_df and load "mtrl" database
mtrl_df$data <- dbReadTable(con, "mtrl")
mf_name_selection_mtrl <- dbReadTable(con,'procblock') %>% select(-c(ProcBlock_Note,ProcBlock,uuid_pb))

# form prep ---------------------------------------------------------------

#Form function for data entry, to be called in both 'add' and 'edit' event
#button_id_mtrl is a variable and button id which will navigate to each event
entry_form_mtrl <- function(button_id_mtrl) {
  
  showModal(
    modalDialog(
      title = strong("Entry Form"),
      fluidRow(
        column(6,
               textInput("MtrlName", "MtrlName", placeholder = ""),
               selectInput("MtrlFomat", "MtrlFomat", choices=c("",unique(as.character(mf_name_selection_mtrl$MfgFomat)))),
               numericInput("MtrlCost", "MtrlCost", value = 10, min = 0),
               textAreaInput("MtrlNote", "MtrlNote", placeholder = "put material assumption note here")
        )
      ),
      footer = tagList(
        actionButton(button_id_mtrl, "Submit"),
        modalButton("Cancel")
      ),
      size = "m", #window size. m:medium
      easyClose = TRUE
    )
  )
  
}

#Save function for form data into data frame format from form function
formData_mtrl <- reactive({
  
  formData_mtrl <- data.frame(uuid_mtrl = UUIDgenerate(),
                              MtrlName = input$MtrlName,
                              MtrlFomat = input$MtrlFomat,
                              MtrlCost = input$MtrlCost,
                              MtrlNote = input$MtrlNote
  ) %>% 
    mutate_each(funs(as.numeric), MtrlCost) %>%   #match variable type to original database
    mutate_each(funs(as.character), c(uuid_mtrl, MtrlName, MtrlFomat, MtrlNote))  #match variable type to original database
  
  #paste0(str(formData))
  return(formData_mtrl)
  
})


# Add ---------------------------------------------------------------------

#Add data button
observeEvent(input$add_button_mtrl, {
  entry_form_mtrl("submit_new_mtrl")
})
observeEvent(input$submit_new_mtrl, {
  mtrl_df$data <- bind_rows(mtrl_df$data, formData_mtrl()) #add new record at bottom
  shinyjs::reset("entry_form_mtrl")
  removeModal()
})  


# Delete ------------------------------------------------------------------

observeEvent(input$delete_button_mtrl, {
  showModal(
    if(length(input$mtrl_table_rows_selected)>=1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Are you sure you would like to delete selected row(s)?"),
        footer = tagList(
          actionButton("submit_delete_mtrl", "Yes"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ) 
    } else if(length(input$mtrl_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select row(s)"),
        easyClose = TRUE
      )
    }
  )
})
observeEvent(input$submit_delete_mtrl, {
  mtrl_df$data <- mtrl_df$data[-input$mtrl_table_rows_selected,]
  removeModal()
})


# Copy --------------------------------------------------------------------

observeEvent(input$copy_button_mtrl, {
  if(length(input$mtrl_table_rows_selected)>=1) {
    row_selection_mtrl <- mtrl_df$data[input$mtrl_table_rows_selected, "uuid_mtrl"] #identify uuid for the selected rows
    copy_df_mtrl <- mtrl_df$data %>% filter(uuid_mtrl %in% row_selection_mtrl) #filter by selected uuid rows
    paste(copy_df_mtrl)
    copy_df_mtrl$uuid_mtrl <- replicate(nrow(copy_df_mtrl), UUIDgenerate()) #replace with new uuid for copied rows
    mtrl_df$data <- bind_rows(mtrl_df$data, copy_df_mtrl) #add copy records at bottom
  } else if(length(input$mtrl_table_rows_selected)<1) {
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
observeEvent(input$edit_button_mtrl, {
  
  showModal(
    if(length(input$mtrl_table_rows_selected)>1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select only one row"),
        easyClose = TRUE
      )
    } else if(length(input$mtrl_table_rows_selected)<1) {
      modalDialog(
        h3(strong("Warning")),
        paste("Please select a row"),
        easyClose = TRUE
      )
    }
  )
  
  if(length(input$mtrl_table_rows_selected)==1) {
    
    #load entry form
    entry_form_mtrl("submit_edit_mtrl")
    
    #update each value in entyr form based on selected row
    updateTextInput(session, "MtrlName", value = mtrl_df$data[input$mtrl_table_rows_selected, "MtrlName"])
    updateSelectInput(session, "MtrlFomat", selected = mtrl_df$data[input$mtrl_table_rows_selected, "MtrlFomat"],
                      choices=c("",unique(as.character(mf_name_selection_mtrl$MfgFomat))))
    updateNumericInput(session, "MtrlCost", value = mtrl_df$data[input$mtrl_table_rows_selected, "MtrlCost"])
    updateTextAreaInput(session, "MtrlNote", value = mtrl_df$data[input$mtrl_table_rows_selected, "MtrlNote"])
  }
})

#update to database
observeEvent(input$submit_edit_mtrl, {
  uuid_selection_mtrl <- mtrl_df$data[input$mtrl_table_rows_selected, "uuid_mtrl"] #identify uuid for the selected row
  uuid_index_mtrl <- match(uuid_selection_mtrl, mtrl_df$data$uuid_mtrl) #get matched row index using UUID
  mtrl_df$data[uuid_index_mtrl, 2:5] <- formData_mtrl()[,2:5] #replace data with updated data. make sure column order matches. uuid_mtrl is 1st column in formData while last column in database
  shinyjs::reset("entry_form_mtrl")
  removeModal()
})

# Fetch -------------------------------------------------------------------

observeEvent(input$fetch_button_mtrl, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Any unsaved event will be lost. Do you still want to refresh?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_refresh_mtrl", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
observeEvent(input$ok_refresh_mtrl, {
  removeModal()
  mtrl_df$data <- dbReadTable(con, "mtrl")
})


# Render Table ------------------------------------------------------------

output$mtrl_table <- DT::renderDataTable({
  
  table <- mtrl_df$data %>% select(-uuid_mtrl) #remove UUID
  dt <- datatable(
    table,
    selection = 'multiple',
    escape = FALSE,
    rownames = FALSE,
    options = list (
      pageLength = 20
    )
  )
  
  dt %>% formatCurrency('MtrlCost', digit = 1)
  
})



# Save to database --------------------------------------------------------

#confirmation window
observeEvent(input$save_button_mtrl, {
  showModal(
    modalDialog(
      h3(strong("Warning")),
      paste("Are you sure to save change(s) you made into database?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_save_mtrl", "Yes")
      ),
      easyClose = TRUE
    )
  )
})
#save to database
observeEvent(input$ok_save_mtrl, {
  dbWriteTable(con, "mtrl", mtrl_df$data, overwrite = T)
  removeModal()
  shinyalert(title = "Saved to database", type = "success")
})  



# Download ----------------------------------------------------------------

output$download_button_arc <- downloadHandler(
  filename = function() {
    paste("PF_DB_Mtrl_", Sys.Date(), ".csv", sep="") #sep="" is no space between words (in this case PFxx and date)
  },
  content = function(file) {
    write.csv(mtrl_df$data, file, row.names=F)
  }
)