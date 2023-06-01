library(googledrive)
library(tidyverse)

# TO ACCESS A PUBLICLY AVAILABLE IMAGE FILE:
# ------------------------------------------
#
# https://raw.githubusercontent.com/murraylax/graphlibrary/main/images/SUBFOLDER/FILENAME
#
# Example:
# https://raw.githubusercontent.com/murraylax/graphlibrary/main/images/Aggregate_Supply_and_Aggregate_Demand/ASAD-AbovePotential-ADLeft.jpg

fix_folder_name <- function(folder) {
  if(str_sub(folder,-1)=="/") folder <- str_sub(folder, start=1, end=-2)
  return(folder)
}

update_all_figures <- function(savefolder, google_drive_location="https://drive.google.com/drive/u/0/folders/1v0UNDJdNHww5FOgBaxFvNuEnK0r5cWq9") {

  images.df <- tibble()
  
  allfolders <- drive_ls(google_drive_location)
  
  for(i in 1:nrow(allfolders)) {
    folder_str <- allfolders$name[i]
    folder_str <- str_squish(folder_str)
    folder_str <- str_replace_all(folder_str, " ", "_")
    google_id <- allfolders$id[i]
    
    folderpath <- sprintf("%s/%s", savefolder, folder_str)
    if(!dir.exists(folderpath)) {
      dir.create(folderpath)
    }
      
    allimages <- drive_ls(google_id)
    for(img in 1:nrow(allimages)) {
      image_str <- allimages$name[img]
      image_file_path <- sprintf("%s/%s.png", folderpath, image_str)
      image_file_path <- str_squish(image_file_path)
      image_file_path <- str_replace_all(image_file_path, " ", "_")
      
      
      image_id <- allimages$id[img]
      s <- drive_download(image_id, path=image_file_path, overwrite=TRUE)
      images.df <- bind_rows(images.df, s)
      
      jpg_file_path <- sprintf("%sjpg", str_sub(image_file_path, start=1, end=-4))
      syscom <- sprintf("convert %s -background white -flatten %s", image_file_path, jpg_file_path)
      system(syscom)
    }
  }
  
  return(images.df)
}

update_one_figure <- function(savefolder, google_drive_location, overwrite=TRUE) {
  figure_info <- drive_get(google_drive_location)
  image_str <- figure_info$name[1]
  image_file_path <- sprintf("%s/%s.png", savefolder, image_str)
  
  images.df <- drive_download(google_drive_location, path=image_file_path, overwrite=overwrite)
  
  jpg_file_path <- sprintf("%s/%s.jpg", savefolder, image_str)
  syscom <- sprintf("convert %s -background white -flatten %s", image_file_path, jpg_file_path)
  system(syscom)
  
  return(images.df)
}

# Example: NOT RUN
if(FALSE) {
  newgraph <- "https://docs.google.com/drawings/d/1Upv2eynCeoHVXoNlZfLi7qNH2PiwQbdlqjF_0vLzbMY"
  savefolder <- "./images/Aggregate_Supply_and_Aggregate_Demand/"
  update_one_figure(savefolder, newgraph)
}