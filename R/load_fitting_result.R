#' Load fitted PhenoFlex model
#' 
#' Allows to read fitted PhenoFlex models, which were saved using \link[LarsChill]{save_fitting_list}
#' 
#' This function reads the saved PhenoFlex model to a list.
#' 
#' @param path character, specifying the location (relative to the working directory)
#' 
#' @param prefix character, added in front of the standard file name
#' 
#' @return list with the fitted objects. Elements are named after the files (without numbering and prefix)
#' 
#' @author Lars Caspersen
#' @keywords utility
#' @importFrom readr parse_number
#'  
#' @export load_fitting_result

load_fitting_result <- function(path, prefix){
  
  files <- list.files(path)
  file_prefixes <- lapply(files, function(x) substr(x, 1, 
                                                    nchar(prefix)))
  scenario_files <- files[which(file_prefixes == prefix)]
  # scenario_num_names <- lapply(scenario_files, function(x) substr(x, 
  #                                                                 nchar(prefix) + 2, nchar(x)))
  scenario_num_names <- lapply(scenario_files, function(x) substr(x, 
                                                                  nchar(prefix)+1, nchar(x)))
  nums <- unlist(lapply(scenario_num_names, function(x) as.numeric(strsplit(x, 
                                                                            "_")[[1]][1])))
  ordered_scenarios <- scenario_files[order(nums)]
  ordered_scenario_names <- scenario_num_names[order(nums)]
  scennames <- lapply(ordered_scenario_names, function(x) {
    num <- strsplit(x, "_")[[1]][1]
    substr(x, nchar(num) + 2, nchar(x) - 4)
  })
  output <- list()
  for (i in 1:length(ordered_scenarios)) output[[i]] <- load_fitting_result_single(path = file.path(path, 
                                                                                                    ordered_scenarios[[i]]))
  names(output) <- scennames
  return(output)
}


#function to read raw fitting results
load_fitting_result_single <- function(path){
  
  #read raw text file
  x <- scan(path, what="", sep="\n", quiet = TRUE)
  
  #bring back to list
  pos.f <- which(x == '$f') 
  pos.x <- which(x == '$x')
  pos.time <- which(x == '$time')
  pos.neval <- which(x == '$neval')
  pos.fbest <- which(x == '$fbest')
  pos.xbest <- which(x == '$xbest')
  pos.numeval <- which(x == '$numeval')
  pos_end_crit <- which(x == '$end_crit')
  pos.cpu_time <- which(x == '$cpu_time')
  pos.Refset <- which(x == '$Refset')
  
  #also scan the Refset data
  pos.Refset.x <- which(x == '$Refset$x')
  pos.Refset.f <- which(x == '$Refset$f')
  pos.Refset.fpen <- which(x == '$Refset$fpen')
  pos.Refset.const <- which(x == '$Refset$const')
  pos.Refset.penalty <- which(x == '$Refset$penalty')
  
  
  
  
  #data in pos f
  
  extract_res_vector <- function(x, pos.data){
    #function to extract one row
    extraxct_number_vector <- function(x, row){
      #split by blank space
      test <- strsplit(x[row], ' ')
      #remove empty strings
      test_filtered <- test[[1]][test[[1]] != '']
      #remove [number] values
      pattern <- "(\\[.*?\\])"
      matches <- gregexpr(pattern, test_filtered)
      overlap <- regmatches(test_filtered, matches, invert = TRUE)
      overlap_clean <- unlist(overlap)
      overlap_clean <- overlap_clean[overlap_clean != '']
      #convert to numeric
      data.f <- as.numeric(overlap_clean)
      
      return(data.f)
    }
    
    #apply function to all rows
    data.f <- purrr::map(pos.data, function(row) extraxct_number_vector(x, row)) %>% 
      unlist()
    
    return(data.f)
  }
  
  extract_res_x <- function(x, pos.data.x){
    
    #extract one row of data from the string
    extract_number_matrix <- function(x, row){
      #extract row of interest, split by empty spaces
      test <- strsplit(x[row], ' ')
      #convert to number
      tes_number <- readr::parse_number(test[[1]][-1])
      #remove cases in which could be converted to number (because it was text)
      number_extracted <- tes_number[is.na(tes_number) == FALSE]
      return(number_extracted)
    }
    
    #decide number of column and rows
    nrow <- length(pos.data.x)-1
    ncol <- max(extract_number_matrix(x, pos.data.x[1]))
    
    #extract data and bind it
    data.x <- purrr::map(pos.data.x[-1], .f = function(row) extract_number_matrix(x, row)) %>% 
      unlist() %>% 
      matrix(nrow = nrow, ncol = ncol, byrow = TRUE)
    
    return(data.x)
    
  }
  
  #get the lines which belong to the different items
  pos.data.f <- (pos.f + 1):(pos.x-1)
  pos.data.x <- (pos.x+1):(pos.time-1)
  pos.data.time <- (pos.time+1):(pos.neval-1)
  #the first and every second roq is "elapsed", skip those
  pos.data.time <- pos.data.time[seq(2,length(pos.data.time), by = 2)]
  
  pos.data.neval <- (pos.neval+1):(pos.fbest-1)
  pos.data.fbest <- (pos.fbest+1):(pos.xbest-1)
  pos.data.xbest <- (pos.xbest+1):(pos.numeval-1)
  pos.data.numeval <- (pos.numeval+1):(pos_end_crit-1)
  pos.data.cpu_time <- (pos.cpu_time+1):(pos.Refset-1)
  #in case of cpu time, drop first row
  pos.data.cpu_time <- pos.data.cpu_time[-1]
  pos.data.end_crit <- (pos_end_crit+1):(pos.cpu_time-1)
  
  #position of Refset data
  pos.data.Refset.x <- (pos.Refset.x + 1):(pos.Refset.f-1)
  pos.data.Refset.f <- (pos.Refset.f + 1):(pos.Refset.fpen-1)
  pos.data.Refset.fpen <- (pos.Refset.fpen + 1):(pos.Refset.const-1)
  pos.data.Refset.const <- (pos.Refset.const + 1):(pos.Refset.penalty-1)
  pos.data.Refset.penalty <- (pos.Refset.penalty + 1):(length(x))
  
  #time data was named, so read and set name per value
  data.time <- extract_res_vector(x, pos.data.time)
  names(data.time) <- rep('elapsed', length(data.time))
  
  data.cpu.time <- extract_res_vector(x, pos.data.cpu_time)
  names(data.cpu.time) <- rep('elapsed', length(data.cpu.time))
  
  #bind them together
  return(
    list('f' = extract_res_vector(x, pos.data.f),
         'x' = extract_res_x(x, pos.data.x),
         'time' = data.time,
         'neval' = extract_res_vector(x, pos.data.neval),
         'fbest' = extract_res_vector(x, pos.data.fbest),
         'xbest' = extract_res_vector(x, pos.data.xbest),
         'numeval' = extract_res_vector(x, pos.data.numeval),
         'end_crit' = extract_res_vector(x, pos.data.end_crit),
         'cpu_time' = data.cpu.time,
         'Refset' = list('x' = extract_res_x(x, pos.data.Refset.x),
                         'f' = extract_res_vector(x, pos.data.Refset.f),
                         'fpen' = extract_res_vector(x, pos.data.Refset.fpen),
                         'const' = extract_res_vector(x, pos.data.Refset.const),
                         'penalty' = extract_res_vector(x, pos.data.Refset.penalty)))
  )
}




