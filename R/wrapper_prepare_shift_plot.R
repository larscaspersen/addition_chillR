#'Prepare a the data.frame to make a shift plot
#'
#'Takes a data.frame as an input, usually the result of some kind of climate change impact projection.
#'Prepares data, so that I can make easily shift plots as in Caspersen et al. (in review), or the 
#'analysis of apple phenology at lake Constance. 
#'
#'Makes relative strong assumptions on the structure of the input. Make sure to format it accordingly.
#'
#' @param shift_df data.frame containing the summarized climate change impact projections and the baseline values. Assumes that it is already summarized (e.g. by median).
#' Assumes that the difference of future and baseline is summarized in the column "shift". Assumes baseline is summarized in column "baseline".
#' @param group_col character vector, containing the column names to group the shift data. Usually this includes the climate scenario,the scenario year and cultivar name.
#' All columns need to be present in shift_df. 
#' @param cut_share numeric vector, segments of the shift plot. Should be between 0 and 1. For each interval, the function will calculate the percentage of shifts within or exceeding the interval#'
#' @return data.frame, with additional columns for the intervals and the agreement among the climate projections regarding the intensity of the shift
#' 
#' @author Lars Caspersen
#' @keywords plotting
#' 
#' @importFrom purrr map
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom tidyr separate
#' 
#' @export wrapper_prepare_shift_plot

wrapper_prepare_shift_plot <- function(shift_df, group_col, cut_share = c(0 ,0.2, 0.4, 0.6, 0.8, 1)){
  
  #make sure the column shift is present
  stopifnot('shift' %in% colnames(shift_df))
  #make sure group col are all present
  stopifnot(all(group_col %in% colnames(shift_df)))
  stopifnot('baseline' %in% colnames(shift_df))
  
  #combine all names to data.frame
  shift_df$id <- apply(shift_df[ , group_col ] , 1 , paste , collapse = "----" )
  
  
  break_df <- split(shift_df, f = shift_df$id) %>% 
    purrr::map(create_break_df, cut_share = cut_share) %>% 
    dplyr::bind_rows(.id = 'id') %>% 
    tidyr::separate(col = 'id', into = group_col, sep = '----')
  
  rownames(break_df) <- NULL
  return(break_df)
  
  #now I lost the baseline value. how can I maintain that?
}

#create dataframe with breaks of shift
create_break_df <- function(sub, cut_share = c(0 ,0.2, 0.4, 0.6, 0.8, 1)){
  
  baseline <- unique(sub$baseline)
  
  #make sure cut_share covers 0 and 1 and is between 0 and 1
  stopifnot(min(cut_share) == 0)
  stopifnot(max(cut_share) == 1)
  
  #find points of data-set that correspond to the share cut-off points
  cut_off_points <- stats::quantile(sub$shift, probs = cut_share)
  
  #find point of transition from plus to minus
  if(max(sub$shift) > 0 & min(sub$shift) < 0){
    #find point where the shift is
    i_change <- which(cut_off_points > 0) %>%  min()
    
    #need to include 0 into the start points
    breaks <- c(cut_off_points[1:(i_change-1)], 0, cut_off_points[i_change:length(cut_off_points)])
    
    #bind breaks together, so far only valid for negative shifts
    break_df <- data.frame(end = breaks[1:length(breaks)-1],
                           start = breaks[2:length(breaks)],
                           share_end = cut_share,
                           share_start = c(cut_share[2:length(cut_share)], NA),
                           cover = NA)
    
    #positive shifts (from zero to max) need to be encoded inversely in terms of coverage
    break_df$share_end_neg <- 1 - break_df$share_end
    break_df$share_start_neg <- c(NA,break_df$share_end_neg[1:nrow(break_df)-1])
    
    #make sure positive shares apply only to positive cut-off values and 
    #negative shares only to negative cut-off values
    break_df$share_end_neg <- ifelse(break_df$start <= 0, yes = NA, no = break_df$share_end_neg)
    break_df$share_start_neg <- ifelse(break_df$start <= 0, yes = NA, no = break_df$share_start_neg)
    break_df$share_end <- ifelse(is.na(break_df$share_end_neg), yes = break_df$share_end, no = NA)
    break_df$share_start <- ifelse(is.na(break_df$share_end_neg), yes = break_df$share_start, no = NA)
    
    #get label for the coverage group
    break_df$cover <-  ifelse(break_df$end < 0, 
                              yes = paste0(break_df$share_end * 100, '% - ', break_df$share_start * 100, '%'),
                              no = paste0(break_df$share_end_neg * 100, '% - ', break_df$share_start_neg * 100, '%'))
    
    #in case the end or end_neg includes 0, change label to <= share start
    break_df$cover <- ifelse(is.na(break_df$share_end) == FALSE &  break_df$share_end== 0,
                             yes = paste0('<=', break_df$share_start * 100, '%'),
                             no = break_df$cover)
    break_df$cover <- ifelse(is.na(break_df$share_end_neg) == FALSE & break_df$share_end_neg == 0,
                             yes = paste0('<=', break_df$share_start_neg * 100, '%'),
                             no = break_df$cover)
    
    #if share start ==100 <- make to >= share end
    break_df$cover <- ifelse(is.na(break_df$share_start) == FALSE & break_df$share_start == 1,
                             yes = paste0('>=', break_df$share_end * 100, '%'),
                             no = break_df$cover)
    break_df$cover <- ifelse(is.na(break_df$share_start_neg) == FALSE &break_df$share_start_neg == 1,
                             yes = paste0('>=', break_df$share_end_neg * 100, '%'),
                             no = break_df$cover)
    
    #add column with minimum and maximum cover share
    break_df$min_cover <- ifelse(is.na(break_df$share_end), yes = break_df$share_end_neg, no = break_df$share_end)
    break_df$max_cover <- ifelse(is.na(break_df$share_start), yes = break_df$share_start_neg, no = break_df$share_start)
    
    #drop the intermediate columns used to construct the interval cover names
    break_df <- break_df[,c("end", "start", "min_cover", "max_cover", "cover")] 
    break_df$baseline <- baseline

    return(break_df)
  }
  
  #case there are only negative shifts
  if(max(sub$shift) <= 0){
    
    #replace lowest bound with zero
    cut_off_points[length(cut_off_points)] <- 0
    
    #bind breaks together
    break_df <- data.frame(end = cut_off_points[1:length(cut_off_points)-1],
                           start = cut_off_points[2:length(cut_off_points)],
                           min_cover = cut_share[1:length(cut_off_points)-1],
                           max_cover = cut_share[2:length(cut_off_points)],
                           cover = NA)
    
    break_df$cover <- paste0(break_df$min_cover*100, '% - ', break_df$max_cover*100, '%')
    
    #replace upper and lower boundary with >= and <= sign
    break_df$cover[1] <- paste0('<=', break_df$max_cover[1] * 100, '%')
    break_df$cover[nrow(break_df)] <- paste0('>=', break_df$min_cover[nrow(break_df)] * 100, '%')
    break_df$baseline <- baseline
    
    return(break_df)
  }
  
  #in case we have only positive values
  if(min(sub$shift) > 0){
    
    #in case of only positive values, the quantile breaks correspond to share cut-off values in reverse
    #so lowest point covers 100% and highest point covers 0%
    
    #replace lowest bound with zero
    cut_off_points[1] <- 0
    
    #bind breaks together
    break_df <- data.frame(start = cut_off_points[1:length(cut_off_points)-1],
                           end = cut_off_points[2:length(cut_off_points)],
                           min_cover = rev(cut_share)[1:length(cut_off_points)-1],
                           max_cover = rev(cut_share)[2:length(cut_off_points)],
                           cover = NA)
    
    break_df$cover <- paste0(break_df$min_cover*100, '% - ', break_df$max_cover*100, '%')
    
    #replace upper and lower boundary with >= and <= sign
    break_df$cover[1] <- paste0('<=', break_df$max_cover[1] * 100, '%')
    break_df$cover[nrow(break_df)] <- paste0('>=', break_df$min_cover[nrow(break_df)] * 100, '%')
    break_df$baseline <- baseline
    return(break_df)
  }
}