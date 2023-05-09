# Function to export a feather file using the target name and a string containing
# the desired folder path where the file we be saved. Returns the file export
# path so that tarchetypes::tar_file() can track it. Filename will contain
# the date of export.

export_feather <- function(target, folder){
  
  out_path <- paste0(
    # Folder to save file in
    folder,
    # Name of target extracted
    deparse(substitute(target)),
    "_out_",
    format(Sys.Date(), "%Y%m%d"),
    ".feather")
  
  # Export file
  write_feather(x = target,
                path = out_path)
  
  # Return path for use with tarchetypes::tar_file()
  out_path
}
