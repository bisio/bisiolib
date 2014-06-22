#' save a bunch of objects in a xlsx file 
#' 
#' This function saves the objects in '...' argument to the 'file' xlsx file
#'  one object per sheet.
#' @param file path of the xlsx file
#' @param ... objects to be saved
#' @return nothing
#' @details 'file' path is passed verbatim to java so no bash style interpolation (eg '~')
#'  is performed
#' 
#' @export
#' @seealso \code{write.xlsx}
#' @importFrom xlsx write.xlsx
  
save.xlsx <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}

#' Change factors columns to character 
#' 
#' This function changes factors columns in a \code{data.frame} to character.
#' @param df the \code{data.frame} to manipulate.
#' @return a \code{data.frame} with characters vectors instead of factors
#' @export
    
defactorize <- function(df) {
  l <- lapply(df,function(x) {
    if (class(x) == "factor")
      as.character(x)
    else 
      x
  })
  as.data.frame(l,stringsAsFactors=F)
}

