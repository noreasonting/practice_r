library(rstudioapi)
setDataPath = function(data_dir) {
  current_path <- dirname(getActiveDocumentContext()$path) 
  data_path = paste(substring(current_path, 1, nchar(current_path)-1), data_dir, sep = '')
  setwd(data_path)
  return(getwd())
}

setDataObjPath = function() {
  setDataPath("data")
}

setRawDataPath = function() {
  setDataPath("data-raw")
}