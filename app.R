library("shiny")
library("shinyjs") 
library("bslib") 
library("deldir")
library("jpeg")
library("tibble")
library("readr")
library("pipeR")
library("dplyr")
#library(tidyverse)

ui <- page_fillable(
  useShinyjs(),
  layout_columns(
    card(card_header("Inputs"),
         fileInput("upload_fixrep", "Upload fixation report", accept = "text/csv"),
         fileInput("upload_face", "Upload face", accept = "image/jpeg"),
         input_switch("toggle_fixations", "Toggle fixation visibility on/off"),
         textInput("aoi_name", "Type name for this AOI, then click in AOI", "a"),
         actionButton("annotate", "Annotate current face"),
         actionButton("next_face", "Start next face"),
         actionButton("debug", "Debug (for dev use only)"),
         actionButton("write", "Exit, saving annotation")
         ),
    card(card_header("Face for edit"),
         plotOutput("face_for_edit", click = "face_for_edit_click", dblclick = "face_for_edit_dblclick")
         ),
    card(card_header("Face for markup"),
         plotOutput("face_for_markup")
         )
    ,col_widths = c(2, 5, 5)
  )
)

server <- function(input, output, session) {
  
  # Globally accessible variables
  g = reactiveValues(
    aois_all = tibble(),
    aois = tibble(),
    fixrep = NA,
    fixrep_this_face = NA,
    fixrep_with_annotation = tibble(),
    vor = NA
    )
  
  observe({
    reactiveValuesToList(g)
  })
  
  # Read face jpeg function
  myjpeg = reactive({
    jpegfile <- input[['upload_face']]
    if(!is.null(jpegfile)){
      readJPEG(jpegfile$datapath, native=TRUE) 
    }
  })

  # Respond to upload fixation report button
  observeEvent(input$upload_fixrep, {
    g$fixrep = read_csv(input$upload_fixrep$datapath, show_col_types = F) 
  })
  
  # Observe without event
  observe({
    if(!is.null(input$upload_face) && !is.null(input$upload_fixrep)){
      g$fixrep_this_face = g$fixrep %>% filter(face_jpeg == input$upload_face$name)
    }
  })

  # Respond to clicks on the face
  observeEvent(input[['face_for_edit_click']], {
    # Make AOI name
    if(input$aoi_name == "a"){
      aoi_name = paste0(input$aoi_name,nrow(g$aois)+1)
      updateTextInput(session, "aoi_name", value="a")
    } else {
      aoi_name = input$aoi_name
      updateTextInput(session, "aoi_name", value="a")
    }
    this_aoi = tibble(x=input$face_for_edit_click$x, 
                      y=input$face_for_edit_click$y, 
                      jpg=input[['upload_face']]$name,
                      aoi_name=aoi_name)
    g$aois = g$aois %>% bind_rows(this_aoi)
  })
  
  # Respond to annotate
  observeEvent(input[['annotate']], {
    #browser()
    for(i in 1:nrow(g$fixrep_this_face)){
      x=g$fixrep_this_face[i, "FIX_X"] %>% pull()
      y=g$fixrep_this_face[i, "FIX_Y"] %>% pull()
      tl=tile.list(g$vor)
      tile_number = which.tile(x,y,tl)
      g$fixrep_this_face[i, "tile"] = tile_number
      g$fixrep_this_face[i, "tile_name"] = g$aois$aoi_name[tile_number]
    }
    g$fixrep_with_annotation = g$fixrep_with_annotation %>% bind_rows(g$fixrep_this_face)
    g$aois_all = g$aois_all %>% bind_rows(g$aois)
  })
  
  # Respond to start next face
  observeEvent(input[['next_face']], {
    g$aois_all = g$aois_all %>% bind_rows(g$aois)
    g$aois = tibble()
    reset("toggle_fixations")
    reset("upload_face")
    shinyjs::runjs("document.getElementById('upload_face').click();")
  })
  
  # Respond to debug
  observeEvent(input[['debug']], {
    browser()
  })
  
  # Respond to Exit saving annotation
  observeEvent(input[['write']], {
    saveRDS(g$fixrep_with_annotation, "fixrep_with_annotation.rds")
    stopApp()
  })
  
  # OUTPUT

  # Prepare the plot for the left-hand-side
  output[['face_for_edit']] <- renderPlot({
    if(!is.null(myjpeg())){
      plot(x=0, y=0, type='n', xlim = c(0, 600), ylim = c(800, 0), xaxt='n', xlab=NA, ylab=NA, axes=F, asp=1)
      axis(2, at=c(0,800), las=1)
      axis(3, at=c(0,600))
      mtext("Assuming eye-tracker has (0,0) at top-left", side=3, line=1)
      rasterImage(myjpeg(), xleft=0, ybottom=800, xright=600, ytop=0) 
      if(nrow(g$aois) > 0){
        points(x=g$aois$x, y=g$aois$y, pch=21, col="red", bg="red", cex=5)
      }}}, width=600, height=800)
  
  # Prepare the plot for the RIGHT-hand-side
  output[['face_for_markup']] <- renderPlot({
    if(!is.null(myjpeg())){
      plot(x=0, y=0, type='n', xlim = c(0, 600), ylim = c(800, 0), xaxt='n', xlab=NA, ylab=NA, axes=F, asp=1)
      axis(2, at=c(0,800), las=1)
      axis(3, at=c(0,600))
      mtext("Assuming eye-tracker has (0,0) at top-left", side=3, line=1)
      rasterImage(myjpeg(), xleft=0, ybottom=800, xright=600, ytop=0) 
      if(input$toggle_fixations==TRUE){
        points(g$fixrep_this_face$FIX_X, g$fixrep_this_face$FIX_Y, pch=21, bg="yellow", cex=5)
        text(g$fixrep_this_face$FIX_X, g$fixrep_this_face$FIX_Y)
        text(g$fixrep_this_face$FIX_X, g$fixrep_this_face$FIX_Y, 
             labels=paste0("(",g$fixrep_this_face$FIX_X, ", ", g$fixrep_this_face$FIX_Y, ")"),
             pos=1, offset=2, col="yellow", cex=2)
      }
      if(nrow(g$aois) >= 2){
        vor <-
          deldir(
            x=g$aois$x, 
            y=g$aois$y, 
            id = g$aois$aoi_name,
            rw=c(xleft = 0, xright=600, ybottom=0, ytop=800)
            )
        g$vor = vor
        plot(vor, add=TRUE, wlines="tess", showpoints=FALSE, showrect=TRUE, labelPts=TRUE, lwd=3, cex=2, lex=3, cmpnt_col=c(tri=1,tess=1,points=1,labels=2,rect=1), cmpnt_lty=c(tri=1,tess=2), axes=TRUE)
      }}}, width=600, height=800)

}

shinyApp(ui = ui, server = server)
