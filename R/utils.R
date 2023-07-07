get_data_file <- function(filename, data_folder="data"){

    # First check locally if the file exists in data folder
    if(file.exists(file.path(data_folder, filename))){
        path <- file.path(data_folder, filename)
    }else{
        # look into installed package
        path <- system.file(data_folder, filename, package = "chinatracker")
    }

    if(!file.exists(path) | (path=="")){
        stop(paste0("File ", filename, " not found in ", data_folder, " folder or in installed package"))
    }

    return(path)
}
