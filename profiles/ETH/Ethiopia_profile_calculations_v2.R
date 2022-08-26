# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
# .rs.restartR()                      # Restart R session
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, car, OutliersO3))

root <- '.'

# Summary table
smy <- readxl::read_excel(path = paste0(root,'/2021/profiles/ETH/data_revised/Ethiopia_indicators.xlsx'), sheet = 'Resume')
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

# Summarize each indicator using: mean, median, sd, mad, min, and max
tbls <- 1:30 %>%
  purrr::map(.f = function(i){
    # Define indicator of interest
    sheet <- paste0('Indicator',i)
    # Load the data for the specific indicator
    tryCatch(expr = {
      dfm <- readxl::read_excel(path = paste0(root,'/2021/profiles/ETH/data_revised/Ethiopia_indicators.xlsx'), sheet = sheet)
    },
    error = function(e){
      cat(paste0('Indicator ',i,' does not exist\n'))
    })
    if(exists('dfm')){
      df <- dfm; rm(dfm)
      # Remove not needed columns
      if('Country' %in% names(df)){df$Country <- NULL}
      if('Element' %in% names(df)){df$Element <- NULL}
      if('Unit' %in% names(df)){df$Unit <- NULL}
      
      df <- base::as.data.frame(df)
      # Identify last year column with information for country reference
      col_id <- which(apply(X = base::as.data.frame(df[which(df$ISO == 'ETH'),]), MARGIN = 2, FUN = function(x){!is.na(x)}))
      # Identify start and ending years
      end <- as.numeric(gsub(pattern = '[a-z]|[A-Z]', replacement = '', x = names(col_id[length(col_id)])))
      yrs <<- names(df)[grep(pattern = '^Y[0-9]', x = names(df))]
      yrs <<- yrs[1:grep(pattern = paste0('Y',end), x = yrs)]
      l00 <- as.numeric(gsub(pattern = 'Y', replacement = '', x = yrs)) < 2000
      if(i != 3){
        if(sum(l00) == length(l00)){ # All years are inferior than 2000
          yrs <<- yrs
        } else {
          if(any(l00)){
            yrs <<- yrs[-which(as.numeric(gsub(pattern = 'Y', replacement = '', x = yrs)) < 2000)]
          }
        }
      }
      
      ### All indicators
      if(length(yrs) < 11){
        
        ini <- as.numeric(gsub(pattern = '[a-z]|[A-Z]', replacement = '', x = yrs[1]))
        tmsr <- df[,yrs]
        if(is.vector(tmsr)){
          tmsr <- data.frame(x = tmsr); colnames(tmsr) <- yrs
        }
        
        names(tmsr) <- gsub(pattern = 'Y', replacement = 'RC', x = names(tmsr))
        tmsr$rate_of_change         <- NA
        tmsr$rate_of_change_cleaned <- NA
        if(length(yrs) == 1){
          tmsr[,1]                    <- 0
          tmsr$rate_of_change         <- 0
          tmsr$rate_of_change_cleaned <- 0
          tmsr <- round(tmsr * 100, 2)
        } else {
          for(j in 1:nrow(tmsr)){
            x <- as.numeric(tmsr[j,])
            x <- x[1:(length(x)-2)]
            if(any(x == 0, na.rm = T)){ x[which(x == 0)] <- x[which(x == 0)] + 0.00001 }
            na_rle <- rle(is.na(x)) # NA length of consecutive sequences
            if(any(na_rle$values)){
              
              # NA positions on the time series
              na_pos <- which(is.na(x))
              
              # Number of years to cover with calculated rate of change
              na_yrs <- na_rle$lengths[na_rle$values == T] + 1
              
              # Original time series positions
              org_pos <- 1:length(x)
              # Time series positions removing NA positions
              cln_pos <- base::setdiff(org_pos, na_pos)
              
              if(!(length(org_pos) %in% na_pos) && !(1 %in% na_pos)){
                # Identify non-consecutive jumps in time series
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
              dff <- c((x/dplyr::lag(x, n = 1)) - 1, NA, NA)
            }
            tmsr[j,] <- dff
            tmsr$rate_of_change[j] <- mean(x = as.numeric(tmsr[j,]), na.rm = T)
            tmsr$rate_of_change_cleaned[j] <- mean_corrected(dff = dff)
          }; rm(j)
          tmsr <- round(tmsr * 100, 2)
        }
        ## The new column 'Reference' has the mean of relative changes for driver variables
        df <- cbind(df, tmsr); rm(tmsr)
        df <- dplyr::left_join(x = df,
                               y = grp %>%
                                 dplyr::filter(Reference == 'ETH') %>%
                                 dplyr::select(ISO,Neighbours), by = 'ISO') %>%
          base::as.data.frame()
        df$Neighbours[is.na(df$Neighbours)] <- 'World'
        df$Neighbours[which(df$ISO == 'ETH')] <- 'Ethiopia'
        df$Order <- sheet
        
      } else {
        
        ini <- end - 10
        yrs <<- yrs[grep(pattern = ini, x = yrs):length(yrs)]
        # Time series values per country
        tmsr <- df[,grep2(pattern = ini:end, x = names(df))]
        
        names(tmsr) <- gsub(pattern = 'Y', replacement = 'RC', x = names(tmsr))
        tmsr$rate_of_change         <- NA
        tmsr$rate_of_change_cleaned <- NA
        for(j in 1:nrow(tmsr)){
          x <- as.numeric(tmsr[j,])
          x <- x[1:(length(x)-2)]
          if(any(x == 0, na.rm = T)){ x[which(x == 0)] <- x[which(x == 0)] + 0.00001 }
          na_rle <- rle(is.na(x)) # NA length of consecutive sequences
          if(any(na_rle$values)){
            
            # NA positions on the time series
            na_pos <- which(is.na(x))
            
            # Number of years to cover with calculated rate of change
            na_yrs <- na_rle$lengths[na_rle$values == T] + 1
            
            # Original time series positions
            org_pos <- 1:length(x)
            # Time series positions removing NA positions
            cln_pos <- base::setdiff(org_pos, na_pos)
            
            if(!(length(org_pos) %in% na_pos) && !(1 %in% na_pos)){
              # Identify non-consecutive jumps in time series
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
            dff <- c((x/dplyr::lag(x, n = 1)) - 1, NA, NA)
          }
          tmsr[j,] <- dff
          tmsr$rate_of_change[j] <- mean(x = as.numeric(tmsr[j,]), na.rm = T)
          tmsr$rate_of_change_cleaned[j] <- mean_corrected(dff = dff)
        }; rm(j)
        tmsr[,1] <- NULL
        tmsr <- round(tmsr * 100, 2)
        ## The new column 'Reference' has the mean of relative changes for driver variables
        df <- cbind(df, tmsr); rm(tmsr)
        df <- dplyr::left_join(x = df,
                               y = grp %>%
                                 dplyr::filter(Reference == 'ETH') %>%
                                 dplyr::select(ISO,Neighbours), by = 'ISO') %>%
          base::as.data.frame()
        df$Neighbours[is.na(df$Neighbours)] <- 'World'
        df$Neighbours[which(df$ISO == 'ETH')] <- 'Ethiopia'
        df$Order <- sheet
      }
      return(df) # cat(i,'\n') # df
    } else {
      return(NULL)
    }
  })

names(tbls) <- paste0('Indicator',1:30)
tbls <- tbls[!sapply(tbls,is.null)]

tbls <- tbls %>%
  purrr::map(.f = function(df){
    df <- dplyr::inner_join(x = smy %>% dplyr::select(Country, ISO),
                            y = df,
                            by = 'ISO')
    return(df)
  })

out <- paste0(root,'/2021/profiles/ETH/results/ETH_calculations_v2.xlsx')
if(!dir.exists(dirname(path = out))){dir.create(path = dirname(path = out), F, T)}
openxlsx::write.xlsx(tbls, file = out, overwrite = T)
