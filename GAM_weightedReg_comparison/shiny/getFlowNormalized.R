## flow normalized

getFlowNormalized <- function(mod_in, dat_in, flow,  allflo = F){
  
  # add year, month columns to dat_in
  dat_in <- mutate(dat_in, 
                   month = as.numeric(strftime(date, '%m')), 
                   year = as.numeric(strftime(date, '%Y'))
  )
  to_plo <- dat_in
  
  # flo values to predict
  flo_vals <- flow
  
  
  # get model predictions across range of flow values
  dynadat <- rep(flo_vals, each = nrow(to_plo)) %>%  ## 13020
    matrix(., nrow = nrow(to_plo), ncol = 1) %>% ## 434 x 30
    cbind(to_plo[, c('dec_time', 'doy')], .) %>% # 434 x 32
    gather('split', 'flo', -dec_time, -doy) %>% ##13020 x 4
    select(-split) %>%  ## 13020 x 3
    data.frame(., res = predict(mod_in, .)) %>%  ## 13020 x 4
    spread(flo, res) %>%  ## 434 x 32
    select(-dec_time, -doy) ## 434 x 30
  
  # merge predictions with year, month data, make long format
  to_plo <- select(to_plo, year, month) %>% 
    cbind(., dynadat) %>%  ## 434 x 32
    gather('flo', 'res', -year, -month) %>% ## 13020 x 4
    mutate(flo = as.numeric(as.character(flo)))
  
  # constrain plots to salinity limits for the selected month
  if(!allflo){
    
    #min, max salinity values to plot
    lim_vals <- group_by(data.frame(dat_in), month) %>% 
      summarise(
        Low = quantile(flo, 0.05, na.rm = TRUE),
        High = quantile(flo, 0.95, na.rm = TRUE)
      )
    ## do I need to do this to match? why would we want to do this in general?
    ## default: don't do this for now

    # merge limts with months
    to_plo <- left_join(to_plo, lim_vals, by = 'month')

    
    # reduce data
    sel_vec <- with(to_plo, 
                    flo >= Low &
                      flo <= High
    )
    to_plo <- to_plo[sel_vec, !names(to_plo) %in% c('Low', 'High')]
    to_plo <- arrange(to_plo, year, month)
    
  }
  
  # reshape data frame, average by year, month for symmetry
  to_plo <- group_by(to_plo, year, month, flo) %>% 
    summarise(
      res = mean(res, na.rm = TRUE)
    )
  
 return(to_plo)
  
}