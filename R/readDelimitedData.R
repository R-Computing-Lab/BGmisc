#' Load Delimited Data File
#'
#' This function reads a text file containing delimited data and converts it into a data frame.
#' It supports multiple delimiters, including pipe ('|') and comma (',').
#'
#' @param path A character string specifying the file path to the data file. Default is 'Z:/Data/2024/Smith_Burt_143000_Demographic_Export.txt'.
#' @param var_nrows An integer specifying the number of rows to read from the file.
#' @param delim A character string specifying the delimiter used in the data file. Default is '|'. Accepts '|' or ','.
#' @param as.is Logical. If TRUE, keeps character columns as strings without converting to factors. Default is TRUE.
#'
#' @return A data frame containing the parsed data.
#'
#' @import data.table stringi
#' @examples
#' # Example usage
#' df <- loadDelimitedData(path = 'data.txt', var_nrows = 100, delim = '|', as.is = TRUE)
#'
#' @export
readDelimitedData <- function(path='Z:/Data/2024/Smith_Burt_143000_Demographic_Export.txt',
                       var_nrows,
                       delim='|',
                       as.is=TRUE){

  require(data.table) # for transpose() list function
  require(stringi) # for stri_split_fixed() function
  
  temp_x <- readLines(
    path,
    n=var_nrows)

  ## extract var names
  temp_n <- temp_x[1]
  if(delim == '\\|' | delim == '|'){
    temp_n <- strsplit(temp_n, '|', fixed = TRUE)[[1]]
    temp_x <- temp_x[-1]

    # fix df
    df <- as.data.frame(data.table::transpose(stringi::stri_split_fixed(temp_x,
                                                   '|',
                                                   n=length(temp_n)))[],
                        col.names=temp_n)
  } else if(delim == ','){
    temp_n <- strsplit(temp_n, ',')[[1]]
    temp_x <- temp_x[-1]

    # fix df
    df <- as.data.frame(data.table::transpose(stringi::stri_split_fixed(temp_x,
                                     ',',
                                     n=length(temp_n))),
                    col.names=temp_n)
  } else {
    stop("Uncoded Delim selected")
  }

  # when feeling lazy
  df <- type.convert(df, as.is=as.is) # keeps strings as strings

  return(df)
}

#' @rdname loadDelimitedData
#' @aliases read_MTDNA
#' @export
read_MTDNA<- readDelimitedData