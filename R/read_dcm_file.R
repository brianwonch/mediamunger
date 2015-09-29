#' Read a DoubleClick file accounting for extra header rows and totals column
#'
#' @param filename a character string giving the path to a DoubleClick report CSV
#' @param skip number of rows to skip if known.  If set at 0, will search for "Report Fields" within first 100 rows.
#'
#' @return a data frame
#' @export
#'
read_dcm_file <- function(filename, skip = 0){
    if(class(filename) != "character"){
        stop("The filename argument to read_dcm_file() must be a character string giving a filepath to a CSV or gzip csv.")
    }

    if(!file.exists(filename)){
        stop(sprintf("The file %s does not exist.", filename))
    }

    # check the first 100 lines of a text file contain "Report Fields"
    if(skip == 0){
        report_fields_rows <- grep("Report Fields",readLines(filename,100), ignore.case=T)

        # if it matches, skip that number of rows, else don't skip lines
        if(length(report_fields_rows) > 0){
            skip <- report_fields_rows[[1]]
            message(sprintf("Found %s rows above report fields. Skipping those rows when loading...", skip))
        }
    }

    df <- read.csv(filename,
                   stringsAsFactors = FALSE,
                   # Account for a byte order mark, common in downloaded CSV files
                   fileEncoding = "UTF-8-BOM",
                   header = TRUE,
                   skip = skip)

    total_row <- grep("Total",df[,1], ignore.case=T)

    if(length(total_row) > 0){
        message(sprintf("Found total row at line %s\n.  Excluding that row...", total_row[[1]]))
        df <- df[1:(total_row-1),]
    }

    # uses cleanNames function from ixmunge
    df <- cleanNames(df)

    #  convert dates.
    #  First a utility function for dealing with dates in different formats
    clean_dcm_date <- function(x){
        if(all(grepl("\\d{4}\\-\\d{1,2}\\-\\d{1,2}",x))){
            return(as.Date(x,format="%Y-%m-%d"))
        } else if(all(grepl("\\d{1,2}\\/\\d{1,2}\\/\\d{4}",x))){
            return(as.Date(x,format="%m/%d/%Y"))
        } else if(all(grepl("\\d{1,2}\\/\\d{1,2}\\/\\d{4}",x))){
            return(as.Date(x,format="%m/%d/%y"))
        } else if(all(grepl("\\d{4}\\/\\d{1,2}\\/\\d{1,2}",x))){
            return(as.Date(x,format="%Y/%m/%d"))
        } else {
            stop("I don't recognize that date format and can't convert to date.")
        }
    }

    date_col_nums = grep("date|week", colnames(df), ignore.case=T)

    if(length(date_col_nums) > 0){
        for(i in 1:length(date_col_nums)){
            df[[date_col_nums[i] ]] <- clean_dcm_date(df[[date_col_nums[i] ]])
            }
        }

    return(df)
}