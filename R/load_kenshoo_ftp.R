#' @title Load a Kenshoo report from an FTP location
#' @description Load a Kenshoo CSV report from an FTP location
#' @param report_name String of the report filename.  ".csv" is appended if you exclude it.
#' @param ftp_address String with the domain name.  Default is "ftp.kenshoo.com"
#' @param col_types One of NULL, a list, a named list or a string, as documented in \code{\link{read_csv}}
#' @param is.csv Boolean if the file is csv.
#' @param username FTP site username.
#' @param password FTP site password. Can be left as NULL if this function is run in the current R session or if the Kenshoo.FTP.Creds list is saved on disk
#'
#' @param save.creds Boolean to save Kenshoo.FTP.Creds to disk for next time.
#' @param creds.file String to supply location of already-saved Kenshoo.FTP.Creds. If NULL, the function will check "~/Kenshoo.FTP.Creds"
#' @importFrom readr read_csv
#' @importFrom stringr str_sub str_trim
#' @return A data frame for the specified file.
#' @export
#'
load_kenshoo_ftp <- function(report_name,
                             ftp_address = "ftp.kenshoo.com",
                             col_types=NULL,
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

    # Append ".csv" to report_name if missing and is.csv == T.
    report_name = str_trim(report_name)
    if (str_sub(report_name, -4) != ".csv") {
        if (is.csv) {
            str_sub = paste0(report_name, ".csv")
        } else {
            stop("Report must be in csv format.")
        }
    }

    if (str_sub(str_trim(ftp_address), -1) != "/") {
        ftp_address = paste0(str_trim(ftp_address), "/")
    }

    filename <- paste0("ftp://", username, ":", password, "@", ftp_address, "/")
    df = read_csv(filename)

    Kenshoo.FTP.Creds <- Kenshoo$Kenshoo.FTP.Creds
    if (save.creds) {
        save(Kenshoo.FTP.Creds, file = creds.file)
        message(paste0("Saved creds to ", path.expand("~"),"/Kenshoo.FTP.Creds"))
    }

    return(df)
}

#Create an environment to hold credentials
Kenshoo <- new.env(parent = emptyenv())
