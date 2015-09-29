#' Grep a chosen folder for certain types of display files
#'
#' @param folder string specifying the location of display files
#' @param type one of c("adwords", "front_end", or "floodlight")
#'
#' @return string showing full path to the chosen file
#' @export
#'

find_display_filenames = function(folder,
                                  type){
    #if folder doesn't end in slash, add a slash
    folder <- ifelse(grepl("/$", folder), folder, paste0(folder, "/"))

    # ensure user has submitted an appropriate type of file to search for
    type = match.arg(type, choices = c("adwords",
                                       "front_end",
                                       "floodlight",
                                       several.ok = FALSE))

    # choose a regex based on the input type of file to search for
    file_pattern = switch(type,
                          adwords = "campaign_performance_report",
                          front_end = "front.*end|basic",
                          floodlight = "floodlight|fl_")


    # get a character vector of files in the folder
    folder_files = list.files(folder)


    # grep for the appropriate strings indicating a file type
    filename = folder_files[grep(file_pattern,
                            folder_files,
                            ignore.case=TRUE)]

    # return the complete path to the chosen file.
    return(paste0(folder, filename))

}
