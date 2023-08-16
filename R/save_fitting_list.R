#' Save fitted PhenoFlex model
#' 
#' Allows to save fitted PhenoFlex models. They can be loaded again using \link[LarsChill]{load_fitting_result}
#' 
#' This function saves the fitted model with all the additional information provided by MEIGOR function and
#' saves it to .txt files. 
#' 
#' @param fit_list list with the fitted PhenoFlex models. Assumes that it is a list with at least one model. Elements should be
#' named, because these will be used when creating the file names.
#' 
#' @param path character, specifying the location (relative to the working directory)
#' 
#' @param prefix character, added in front of the standard file name
#' 
#' @return list with the fitted objects. Elements are named after the files (without numbering and prefix)
#' 
#' @importFrom utils capture.output
#' 
#' @author Lars Caspersen
#' @keywords utility
#'  
#' @export save_fitting_list
save_fitting_list <- function(fit_list, path, prefix){
  
  purrr::pmap(list(fit_list, names(fit_list), 1:length(fit_list)), function(fitted, cultivar, n){
    fname <- paste0(path, prefix, n,'_', cultivar, '.txt')
    
    utils::capture.output(fitted, file = fname)
  })
  
  
}