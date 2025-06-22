# downloadButton("download_tileassignment", "Download fixation report annotated with an AOI for each fixation")
# 
# output$download_tileassignment <- downloadHandler(
#   filename = function() {
#     paste0("fixrep_with_tile", ".csv")
#   },
#   content = function(file) {
#     vroom::vroom_write(data_tileassignment(), file)
#   }
# )