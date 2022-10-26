# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
# .rs.restartR()                      # Restart R session
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, car, OutliersO3)) # xlsx, openxlsx

root <- 'D:/OneDrive - CGIAR/Sustainable Food Systems'

# Summary table
smy <- readxl::read_excel(path = paste0(root,'/2021/profiles/HND/data_revised/Honduras_indicators.xlsx'), sheet = 'Resume')
smy <- smy[-which(is.na(smy$ISO)),]

# Groups for each country
grp <- read.csv(paste0(root,'/2021/profiles/neighbours.csv'))

# Vectorized grep function
grep2 <- Vectorize(FUN = grep, vectorize.args = 'pattern')
# Function to compute the arithmetic average removing outliers
mean_corrected <- function(dff){
  if(sum(is.na(dff)) != length(dff)){
    dff <- dff %>% na.omit() %>% as.numeric() %>% sort()
    ## Outliers detection
    # Method 1: boxplot
    bxp   <- boxplot(dff)
    m1_id <- which(dff %in% bxp$out)
    # Method 2: Robust Location and Scatter Estimation via MCD
    dfm <- data.frame(dff = dff, time = 1:length(dff))
    tryCatch(expr = {O3s <- dfm %>% OutliersO3::O3prep(method = c("MCD"), tols = .1, boxplotLimits = 6)}, error = function(e) cat('Error obtained\n'))
    if(exists('O3s')){ # Check if Method 2 gets results
      m2_id <- O3s$outList$outM[[1]]$outlierIndices
      if(length(m1_id) > 0 | length(m2_id) > 0){
        otlr <- unique(c(m1_id,m2_id))
        dfm <- dfm[-otlr,]
      }
    }
    if(length(m1_id) > 0){ # Check if Method 1 gets results
      otlr <- m1_id
      dfm <- dfm[-otlr,]
    }
    dff <- mean(dfm$dff)
  } else {
    dff <- NA
  }
  return(dff)
}
# Time series computation
tmsr_cmp <- function(j){
  # cat(paste0(df$ISO[j],'\n'))
  x <- as.numeric(tmsr[j,])
  x <- x[1:(length(x)-2)]
  if(any(x == 0, na.rm = T)){ x[which(x == 0)] <- x[which(x == 0)] + 0.00001 }
  na_rle <- rle(is.na(x)) # NA length of consecutive sequences
  if(any(na_rle$values)){
    
    if(sum(is.na(x)) != length(x)){
      # Remove initial and ending missing values because they do not have how to be computed
      if(na_rle$values[1] == T){
        # NA positions on the time series
        na_pos_ini <- which(is.na(x))[(1:na_rle$lengths[1])]
        na_pos <- which(is.na(x))[-(1:na_rle$lengths[1])]
        na_rle$values <- na_rle$values[-1]
        na_rle$lengths <- na_rle$lengths[-1]
      }
      if(na_rle$values[length(na_rle$values)] == T){
        # NA positions on the time series
        na_pos_end <- which(is.na(x))[( (length(which(is.na(x))) - na_rle$lengths[length(na_rle$lengths)] + 1):length(which(is.na(x))) )]
        na_pos <- which(is.na(x))[-( (length(which(is.na(x))) - na_rle$lengths[length(na_rle$lengths)] + 1):length(which(is.na(x))) )]
        na_rle$values <- na_rle$values[-length(na_rle$values)]
        na_rle$lengths <- na_rle$lengths[-length(na_rle$values)]
      } else {
        na_pos <- which(is.na(x))
      }
      
      # Number of years to cover with calculated rate of change
      na_yrs <- na_rle$lengths[na_rle$values == T] + 1
      
      if(length(na_yrs) > 0){
        # Original time series positions
        org_pos <- 1:length(x)
        # Time series positions removing NA positions
        cln_pos <- base::setdiff(org_pos, na_pos)
        if(exists('na_pos_ini')){cln_pos <- base::setdiff(cln_pos,na_pos_ini)}
        if(exists('na_pos_end')){cln_pos <- base::setdiff(cln_pos,na_pos_end)}
        rm(na_pos_ini, na_pos_end)
        
        changes <- cln_pos - dplyr::lag(cln_pos, n = 1)
        # Identify replacement positions
        rpl_pos <- which(changes > 1)
        
        # Remove missing data from original time series
        x_full <- as.numeric(na.omit(x))
        
        # Compute rate of change RoC
        dff <- (x_full/dplyr::lag(x_full, n = 1)) - 1
        
        # Compute annual RoC for corresponding positions
        RoC_yrs <- dff[rpl_pos]/na_yrs
        
        # Replace missing values by annual RoC estimations
        dff2 <- x
        dff2[cln_pos] <- dff
        dff2[na_pos]  <- rep(RoC_yrs, na_rle$lengths[na_rle$values == T])
        dff2[base::setdiff(na_pos + 1, na_pos)] <- RoC_yrs # Last year replacement
        
        dff <- c(dff2, NA, NA); rm(dff2)
      } else {
        dff <- c((x/dplyr::lag(x, n = 1)) - 1, NA, NA)
      }
    } else {
      dff <- c(x, NA, NA)
    }
    rm(changes,cln_pos,dff2,na_pos,na_yrs,org_pos,RoC_yrs,rpl_pos)
  } else {
    dff <- c((x/dplyr::lag(x, n = 1)) - 1, NA, NA)
  }
  return(dff)
}

# Summarize each indicator using: mean, median, sd, mad, min, and max
tbls <- 1:30 %>%
  purrr::map(.f = function(i){
    
    # Define indicator of interest
    sheet <- paste0('Indicator',i)
    # Load the data for the specific indicator
    df <- readxl::read_excel(path = paste0(root,'/2021/profiles/HND/data_revised/Honduras_indicators.xlsx'), sheet = sheet)
    # Remove not needed columns
    if('Country' %in% names(df)){df$Country <- NULL}
    if('Element' %in% names(df)){df$Element <- NULL}
    if('Unit' %in% names(df)){df$Unit <- NULL}
    
    df <- base::as.data.frame(df)
    # Identify last year column with information for country reference
    col_id <- which(apply(X = base::as.data.frame(df[which(df$ISO == 'HND'),]), MARGIN = 2, FUN = function(x){!is.na(x)}))
    # Identify start and ending years
    end <- as.numeric(gsub(pattern = '[a-z]|[A-Z]', replacement = '', x = names(col_id[length(col_id)]))); rm(col_id)
    
    if(i == 20){
      
      ini <- end - 10
      # Time series values per country
      tmsr <- df[,unlist(grep2(pattern = paste0('Y',ini:end), x = names(df)))]
      names(tmsr) <- gsub(pattern = 'Y', replacement = 'RC', x = names(tmsr))
      tmsr$rate_of_change         <- NA
      tmsr$rate_of_change_cleaned <- NA
      tmsr <<- tmsr
      
      for(idx in 1:nrow(tmsr)){
        dff <- tmsr_cmp(j = idx)
        tmsr[idx,] <- dff/5
        tmsr$rate_of_change[idx] <- mean(x = as.numeric(tmsr[idx,]), na.rm = T)
        tmsr$rate_of_change_cleaned[idx] <- tmsr$rate_of_change[idx]
        rm(dff, idx)
      }
      
      tmsr[,1] <- NULL
      tmsr <- round(tmsr * 100, 2)
      ## The new column 'Reference' has the mean of relative changes for driver variables
      df$Initial_year <- 2010
      df$Recent_year  <- 2020
      df$Initial <- df$Y2010
      df$Recent  <- df$Y2020
      
      df <- cbind(df, tmsr); rm(tmsr)
      df <- dplyr::left_join(x = df,
                             y = grp %>%
                               dplyr::filter(Reference == 'HND') %>%
                               dplyr::select(ISO,Neighbours), by = 'ISO') %>%
        base::as.data.frame()
      df$Neighbours[is.na(df$Neighbours)] <- 'World'
      df$Neighbours[which(df$ISO == 'HND')] <- 'Honduras'
      df$Order <- sheet
      
    } else {
      if(i %in% c(11, 14, 18)){
        
        ini <- end - 1
        tmsr <- df[,unlist(grep2(pattern = paste0('Y',ini:end), x = names(df)))]
        names(tmsr) <- gsub(pattern = 'Y', replacement = 'RC', x = names(tmsr))
        tmsr$rate_of_change         <- NA
        tmsr$rate_of_change_cleaned <- NA
        tmsr <<- tmsr
        
        for(idx in 1:nrow(tmsr)){
          dff <- tmsr_cmp(j = idx)
          tmsr[idx,] <- dff
          tmsr$rate_of_change[idx] <- mean(x = as.numeric(tmsr[idx,]), na.rm = T)
          tmsr$rate_of_change_cleaned[idx] <- tmsr$rate_of_change[idx]
          rm(dff, idx)
        }
        
        tmsr[,1] <- NULL
        tmsr <- round(tmsr * 100, 2)
        ## The new column 'Reference' has the mean of relative changes for driver variables
        df$Initial_year <- 2017
        df$Recent_year  <- 2018
        df$Initial <- df$Y2017
        df$Recent  <- df$Y2018
        
        df <- cbind(df, tmsr); rm(tmsr)
        df <- dplyr::left_join(x = df,
                               y = grp %>%
                                 dplyr::filter(Reference == 'HND') %>%
                                 dplyr::select(ISO,Neighbours), by = 'ISO') %>%
          base::as.data.frame()
        df$Neighbours[is.na(df$Neighbours)] <- 'World'
        df$Neighbours[which(df$ISO == 'HND')] <- 'Honduras'
        df$Order <- sheet
        
      } else {
        
        # Years intervals
        upp <- c(end - 2, end + 2)
        ini <- end - 9
        lwr <- c(ini - 2, ini + 2)
        yrs <- lwr[1]:upp[2]
        # Final list of years to analyze
        yrs <- paste0('Y',yrs)
        
        # Define the intervals
        lw <- paste0('Y',lwr[1]:lwr[2])
        up <- paste0('Y',upp[1]:upp[2])
        lw <- lw[lw %in% names(df)]
        up <- up[up %in% names(df)]
        
        # Find the closest non-missing value to the Initial year
        lw_vals <- df[,c('ISO',lw)] %>%
          tidyr::pivot_longer(cols = 2:ncol(.), names_to = 'year', values_to = 'value') %>%
          dplyr::mutate(year = as.numeric(gsub('Y','',year))) %>%
          dplyr::mutate(distance = abs(year - as.numeric(ini))) %>%
          dplyr::filter(!is.na(value)) %>%
          dplyr::group_by(ISO) %>%
          dplyr::filter(distance == min(distance)) %>%
          dplyr::filter(row_number() == min(row_number()))
        
        # Find the closest non-missing value to the Recent year
        up_vals <- df[,c('ISO',up)] %>%
          tidyr::pivot_longer(cols = 2:ncol(.), names_to = 'year', values_to = 'value') %>%
          dplyr::mutate(year = as.numeric(gsub('Y','',year))) %>%
          dplyr::mutate(distance = abs(year - as.numeric(end))) %>%
          dplyr::filter(!is.na(value)) %>%
          dplyr::group_by(ISO) %>%
          dplyr::filter(distance == min(distance)) %>%
          dplyr::filter(row_number() == min(row_number()))
        
        df_new <- dplyr::full_join(x = lw_vals %>% dplyr::select(-distance) %>% dplyr::rename(Initial = value, Initial_year = year),
                                   y = up_vals %>% dplyr::select(-distance) %>% dplyr::rename(Recent = value, Recent_year = year),
                                   by = 'ISO')
        # Replace values
        df[match(df_new$ISO, df$ISO),paste0('Y',ini)] <- df_new$Initial
        df[match(df_new$ISO, df$ISO),paste0('Y',end)] <- df_new$Recent
        
        # Time series values per country
        tmsr <- df[,grep2(pattern = paste0('Y',ini:end), x = names(df))]
        names(tmsr) <- gsub(pattern = 'Y', replacement = 'RC', x = names(tmsr))
        tmsr$rate_of_change         <- NA
        tmsr$rate_of_change_cleaned <- NA
        tmsr <<- tmsr
        
        for(idx in 1:nrow(tmsr)){
          dff <- tmsr_cmp(j = idx)
          tmsr[idx,] <- dff
          tmsr$rate_of_change[idx] <- mean(x = as.numeric(tmsr[idx,]), na.rm = T)
          tmsr$rate_of_change_cleaned[idx] <- mean_corrected(dff = dff)
          rm(dff, idx)
        }
        
        tmsr[,1] <- NULL
        tmsr <- round(tmsr * 100, 2)
        ## The new column 'Reference' has the mean of relative changes for driver variables
        df$Initial_year <- NA
        df$Recent_year  <- NA
        df[match(df_new$ISO, df$ISO),'Initial_year'] <- df_new$Initial_year
        df[match(df_new$ISO, df$ISO),'Recent_year']  <- df_new$Recent_year
        df$Initial <- NA
        df$Recent  <- NA
        df[match(df_new$ISO, df$ISO),'Initial'] <- df_new$Initial
        df[match(df_new$ISO, df$ISO),'Recent']  <- df_new$Recent
        
        df <- cbind(df, tmsr); rm(tmsr)
        df <- dplyr::left_join(x = df,
                               y = grp %>%
                                 dplyr::filter(Reference == 'HND') %>%
                                 dplyr::select(ISO,Neighbours), by = 'ISO') %>%
          base::as.data.frame()
        df$Neighbours[is.na(df$Neighbours)] <- 'World'
        df$Neighbours[which(df$ISO == 'HND')] <- 'Honduras'
        df$Order <- sheet
        
      }
    }
    
    return(df) # cat(i,'\n') # df
    
  })

tbls <- tbls %>%
  purrr::map(.f = function(df){
    df <- dplyr::inner_join(x = smy %>% dplyr::select(Country, ISO),
                            y = df,
                            by = 'ISO')
    return(df)
  })

names(tbls) <- paste0('Indicator',1:30)
out <- paste0(root,'/2021/profiles/HND/results/HND_calculations_v2.xlsx')
if(!dir.exists(dirname(path = out))){dir.create(path = dirname(path = out), F, T)}
openxlsx::write.xlsx(tbls, file = out, overwrite = T)
