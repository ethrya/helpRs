#' Combine all pdfs in a folder.
#'
#' @param folder_of_pdfs path to folder containing pdfs
#' @param output_name Name of saved file
#' @param output_path (str; default = folder_of_pdfs) Where should the pdfs be stored
#'
#' @return Location of the outputted pdf file
#' @export
#'
combine_pdf_folder <- function(folder_of_pdfs,
                               output_name,
                               output_path = folder_of_pdfs){
  # Check file extention has been entered
  assertthat::assert_that(stringr::str_detect(output_name, ".pdf$"),
                          msg = "Output file does not have a .pdf extention. This might make it hard to open.")

  pdf_files <- sort(list.files(folder_of_pdfs, pattern = ".pdf", full.names = T))

  output_location <- file.path(output_path, output_name)

  pdftools::pdf_combine(pdf_files, output = output_location)

  cat("Combined pdf saved to:", crayon::cyan(output_location), "\n",
      "Combined pdf has", crayon::green(pdftools::pdf_length(output_location)),
      "pages")

  return(output_location)
}
