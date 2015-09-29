#' @title Load a Kenshoo report from an FTP location
#' @description Load a Kenshoo CSV report from an FTP location
#'
#' @param report_name String of the report filename.  ".csv" is appended if you exclude it.
#' @param ftp_address String with the domain name.  Default is "ftp.kenshoo.com"
#' @param is.csv Boolean if the file is csv.
#' @param username FTP site username.
#' @param password FTP site password. Can be left as NULL if this function is run in the current R session or if the Kenshoo.FTP.Creds list is saved on disk
#'
#' @param save.creds Boolean to save Kenshoo.FTP.Creds to disk for next time.
#' @param creds.file String to supply location of already-saved Kenshoo.FTP.Creds. If NULL, the function will check "~/Kenshoo.FTP.Creds"
#'
#' @return A data frame for the specified file.
#' @export
#'
load_kenshoo_ftp <- function(report_name,
                             ftp_address = "ftp.kenshoo.com",
                             is.csv = TRUE,
                             username = NULL,
                             password = NULL,
                             save.creds = FALSE,
                             creds.file = NULL)
{
    if (is.null(username) || is.null(password)) {
        # Provide default creds.file save location if not specified
        if (is.null(creds.file)) {
            creds.file = "~/Kenshoo.FTP.Creds"
        }

        if (is.null(Kenshoo$Kenshoo.FTP.Creds$username) ||
           is.null(Kenshoo$Kenshoo.FTP.Creds$password)) {

            if (file.exists(creds.file)) {
                load(creds.file, envir = Kenshoo)
            }

        # Retrieve creds if saved in creds.file or already in Kenshoo environment
            username = Kenshoo$Kenshooo.FTP.Creds[["username"]]
            password = Kenshoo$Kenshooo.FTP.Creds[["password"]]
        }

        # Creds can't be retrieved.
        stop("username and password should be specified, or provide a creds.file")
    }

    # Creds exist
    assign("Kenshoo.FTP.Creds", list("username"=username, "password"=password), envir = Kenshoo)
    message("FTP credentials stored in the 'Kenshoo' R environment")


    if (stringr::str_sub(stringr::str_trim(ftp_address), -1) != "/") {
        ftp_address = paste0(stringr::str_trim(ftp_address), "/")
    }



    filename <- paste0("ftp://", username, ":", password, "@", ftp_address, "/",
                       report_name)


    read_gz = function(ftp_path_name) {
        con <- gzcon(url(ftp_path_name))
        txt <- readLines(con)
        # grab the first readLines item (that is, the colnames) and split on comma.
        # stringr::str_split returns a list so we need to subset out the first list element.
        # We will later setNames on our dataset.  This helps avoid awkwardness with
        # a Byte Order Mark in column names (hence "BOM" fileEncoding in the
        # read.csv call.)
        nms =  str_split(txt[[1]], ",")[[1]]
        result <- read.csv(textConnection(txt), fileEncoding="UTF-8-BOM",
                           stringsAsFactors=F) %>% setNames(nms)
        return(result)
    }

    if(grepl("\\.gz", filename, ignore.case=T)){
        df = read_gz(filename)
    } else {
        df = read.csv(filename, fileEncoding="UTF-8-BOM", stringsAsFactors=F)

    }


    Kenshoo.FTP.Creds <- Kenshoo$Kenshoo.FTP.Creds
    if (save.creds) {
        save(Kenshoo.FTP.Creds, file = creds.file)
        message(paste0("Saved creds to ", path.expand("~"),"/Kenshoo.FTP.Creds"))
    }

    return(df)
}

#Create an environment to hold credentials
Kenshoo <- new.env(parent = emptyenv())
