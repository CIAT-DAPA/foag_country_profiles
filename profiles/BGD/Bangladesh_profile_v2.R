# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
# .rs.restartR()                      # Restart R session
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(if(!require(pacman)){install.packages('pacman');library(pacman)} else {library(pacman)})
suppressMessages(pacman::p_load(tidyverse, MetBrewer))

root <- '.'

# Summarizing metrics
sts <- 1:30 %>%
  purrr::map(.f = function(i){
    
    rch <- readxl::read_excel(path = paste0(root,'/2021/profiles/BGD/results/BGD_calculations_v2.xlsx'), sheet = i)
    rcs <- grep(pattern = '^RC', x = names(rch))
    rcs2 <- match(gsub(pattern = 'RC', replacement = 'Y', x = names(rch)[rcs]), names(rch))
    if(length(rcs2) == 10 & class(rch %>% dplyr::pull(3)) != 'character'){ rcs2 <- c(rcs2[1]-1,rcs2) }
    yrs <- gsub(pattern = 'Y', replacement = '', x = names(rch)[c(rcs2[1], rcs2[length(rcs2)])])
    lwr <- as.numeric(yrs) - 2
    upp <- as.numeric(yrs) + 2
    
    lw <- paste0('Y',lwr[1]:upp[1])
    up <- paste0('Y',lwr[2]:upp[2])
    lw <- lw[lw %in% names(rch)]
    up <- up[up %in% names(rch)]
    
    if(identical(yrs[1],yrs[2])){ # One year
      ref <- rch[,c(paste0('Y',unique(yrs)),'rate_of_change', 'rate_of_change_cleaned','Neighbours')]
      sts <- rbind(ref %>%
                     dplyr::filter(Neighbours != 'World') %>%
                     dplyr::group_by(Neighbours) %>%
                     dplyr::summarise_all(mean, na.rm = T),
                   ref %>%
                     dplyr::mutate(Neighbours = 'World') %>%
                     dplyr::group_by(Neighbours) %>%
                     dplyr::summarise_all(mean, na.rm = T)) %>% base::as.data.frame()
      sts$New <- sts[,2]
      names(sts)[ncol(sts)] <- names(sts)[2]
      sts <- sts[,c(names(sts)[1:2],names(sts)[ncol(sts)],names(sts)[3:4])]
      names(sts)[2:3] <- paste0('Y',yrs)
    } else { # More than one year
      # Table of interest
      if(!(i %in% c(11,14,18))){
        ## Past alternative
        # lw_avg <- rowMeans(x = rch[,lw], na.rm = T)
        # up_avg <- rowMeans(x = rch[,up], na.rm = T)
        # ref <- rch[,c(paste0('Y',yrs),'rate_of_change', 'rate_of_change_cleaned','Neighbours')]
        # ref[,paste0('Y',yrs)] <- data.frame(lw_avg, up_avg)
        ## New alternative: finding the closest non-missing value to replace
        lw_vals <- rch[,c('ISO',lw)] %>%
          tidyr::pivot_longer(cols = 2:ncol(.), names_to = 'year', values_to = 'value') %>%
          dplyr::mutate(year = as.numeric(gsub('Y','',year))) %>%
          dplyr::mutate(distance = abs(year - as.numeric(yrs[1]))) %>%
          dplyr::filter(!is.na(value)) %>%
          dplyr::group_by(ISO) %>%
          dplyr::filter(distance == min(distance)) %>%
          dplyr::filter(row_number() == min(row_number()))
        
        up_vals <- rch[,c('ISO',up)] %>%
          tidyr::pivot_longer(cols = 2:ncol(.), names_to = 'year', values_to = 'value') %>%
          dplyr::mutate(year = as.numeric(gsub('Y','',year))) %>%
          dplyr::mutate(distance = abs(year - as.numeric(yrs[2]))) %>%
          dplyr::filter(!is.na(value)) %>%
          dplyr::group_by(ISO) %>%
          dplyr::filter(distance == min(distance)) %>%
          dplyr::filter(row_number() == min(row_number()))
        
        ref <- dplyr::full_join(x = lw_vals %>% dplyr::select(-year,-distance) %>% dplyr::rename(Initial = value),
                                y = up_vals %>% dplyr::select(-year,-distance) %>% dplyr::rename(Recent = value),
                                by = 'ISO')
        ref <- dplyr::left_join(x = ref,
                                y = rch[,c('ISO','rate_of_change','rate_of_change_cleaned','Neighbours')],
                                by = 'ISO') %>%
          dplyr::ungroup() %>%
          dplyr::select(-ISO) %>%
          base::as.data.frame()
      } else {
        ref <- rch[,c(paste0('Y',yrs),'rate_of_change', 'rate_of_change_cleaned','Neighbours')]
      }
      sts <- rbind(ref %>%
                     dplyr::filter(Neighbours != 'World') %>%
                     dplyr::group_by(Neighbours) %>%
                     dplyr::summarise_all(mean, na.rm = T),
                   ref %>%
                     dplyr::mutate(Neighbours = 'World') %>%
                     dplyr::group_by(Neighbours) %>%
                     dplyr::summarise_all(mean, na.rm = T)) %>% base::as.data.frame()
    }
    
    names(sts) <- c('Group','Initial','Recent','Mean_delta','Cleaned_mean_delta')
    sts$Indicator <- rch %>% base::as.data.frame() %>% .[,3] %>% unique()
    sts$Order <- paste0('Indicator',i)
    sts$Period <- paste(yrs, collapse = '-')
    sts$Last_year <- yrs[2]
    sts <- sts %>% dplyr::mutate_if(is.numeric, round, 1)
    sts$Group <- factor(x = sts$Group,
                        levels = c('World','GDP pc comparable','Geographic neighboring','Bangladesh'),
                        labels = c('Global average','Countries with similar GDP pc','Geographic neighbors','Bangladesh'),
                        ordered = T)
    return(sts)
    
  }) %>%
  dplyr::bind_rows()

# Saving the summarization metrics
out <- paste0(root,'/2021/profiles/BGD/results/BGD_ind_summary.xlsx')
if(!file.exists(out)){openxlsx::write.xlsx(sts, file = out, overwrite = T)}; rm(out)

# Graph 1: change in 2 periods
sts$Order %>%
  unique() %>%
  purrr::map(.f = function(vr){
    tst <- sts %>% dplyr::filter(Order == vr)
    yrs <- strsplit(x = tst$Period, split = '-')[[1]]
    
    gg <- ggplot2::ggplot(tst) +
      ggplot2::geom_segment(aes(x = 1, xend = 2, y = Initial, yend = Recent),size = 1.6, col = "#6ba0c7") + # Draw straight line between two points
      ### Set location, size, and color of points
      ggplot2::geom_point(aes(x = 1, y = Initial, colour = Group), size = 22, alpha = 1) +
      ggplot2::geom_point(aes(x = 2, y = Recent, colour = Group), size = 22, alpha = 1) +
      #ggplot2::geom_text(aes(x = .95, y = Initial, label = paste0(Group)), size = 6, col = "black", vjust = -0.3, hjust = 1) + # Display names of variables, initial and ending years
      ggplot2::geom_text(aes(x = 1, y = Initial, label = paste0(round(Initial,1))), size = 6, col = "black", hjust = 2) + # , vjust = -1
      ggplot2::geom_text(aes(x = 2, y = Recent, label = paste0(round(Recent,1))), size = 6, col = "black", hjust = -1) + # , vjust = -1
      ggplot2::geom_text(aes(x = 1, y = max(Initial,Recent), label = yrs[1]), size = 8, col = "black", fontface = "bold", vjust = -1.5) +
      ggplot2::geom_text(aes(x = 2, y = max(Initial,Recent), label = yrs[2]), size = 8, col = "black", fontface = "bold", vjust = -1.5) +
      ggplot2::scale_colour_manual(values = MetBrewer::met.brewer(name = 'Cassatt1', n = 4)) +
      ggplot2::xlim(0.5, 2.5) +
      ggplot2::ylim(min(c(tst$Initial,tst$Recent)), max(c(tst$Initial,tst$Recent)) * 1.1) + #  +- 3
      ggplot2::theme_minimal() +
      ### Add title and data source
      ggplot2::labs(title=unique(tst$Indicator)) +
      ###  Set size and position of title and source
      ggplot2::theme(text            = element_text(size = 17, colour = 'black'),
                     axis.text       = element_text(size = 16, colour = 'black'),
                     legend.text     = element_text(size = 13, colour = 'black'),
                     legend.title    = element_blank(),
                     plot.title      = element_text(size = 25, colour = 'black'),
                     plot.subtitle   = element_text(size = 17, colour = 'black'),
                     strip.text.x    = element_text(size = 17, colour = 'black'),
                     strip.text.y    = element_text(size = 17, colour = 'black'),
                     plot.caption    = element_text(size = 15, hjust = 0, colour = 'black'),
                     legend.position = "bottom",
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.x  = element_blank(),
                     axis.text.y  = element_blank())
    
    ggplot2::ggsave(filename = paste0(root,'/2021/profiles/BGD/results/plots/points/',vr,'_graph.svg'),
                    plot = gg, device = 'svg', units = 'in', width = 11, height = 10, dpi = 350)
    ggplot2::ggsave(filename = paste0(root,'/2021/profiles/BGD/results/plots/points/',vr,'_graph.png'),
                    plot = gg, device = 'png', units = 'in', width = 11, height = 10, dpi = 350)
    
  })

lbls <- readxl::read_excel(path = paste0(root,'/2021/profiles/info.xlsx'), sheet = 'BGD')

# Graph 2: time series
1:30 %>%
  purrr::map(.f = function(i){
    
    rch <- readxl::read_excel(path = paste0(root,'/2021/profiles/BGD/results/BGD_calculations_v2.xlsx'), sheet = i)
    rcs <- grep(pattern = '^RC', x = names(rch))
    rcs2 <- match(gsub(pattern = 'RC', replacement = 'Y', x = names(rch)[rcs]), names(rch))
    if(length(rcs2) == 10 & class(rch %>% dplyr::pull(3)) != 'character'){ rcs2 <- c(rcs2[1]-1,rcs2) }
    yrs <- gsub(pattern = 'Y', replacement = '', x = names(rch)[c(rcs2[1], rcs2[length(rcs2)])])
    end <- as.numeric(yrs[2])
    ini <- as.character(end - 10)
    yrs[1] <- ifelse(paste0('Y',ini) %in% names(rch), ini, yrs[1])
    ini <- as.numeric(yrs[1])
    
    if(!identical(yrs[1],yrs[2])){
      pos <- which(names(rch) %in% paste0('Y',yrs))
      gg <- rch[,c(pos[1]:pos[2],grep('Neighbours',names(rch)))] %>%
        dplyr::group_by(Neighbours) %>%
        dplyr::summarise_all(mean, na.rm = T) %>%
        tidyr::pivot_longer(cols = 2:ncol(.), names_to = 'Year', values_to = 'Value') %>%
        dplyr::mutate(Year = gsub('Y','',Year) %>% as.numeric()) %>%
        ggplot2::ggplot(aes(x = Year, y = Value, colour = Neighbours)) +
        ggplot2::geom_line(size = 1.2) +
        ggplot2::scale_x_continuous(breaks = ini:end) +
        ggplot2::scale_colour_manual(values = MetBrewer::met.brewer(name = 'Egypt', n = 4)) + # 
        ggplot2::theme_bw() +
        ggplot2::theme(text            = element_text(size = 17, colour = 'black'),
                       axis.text       = element_text(size = 16, colour = 'black'),
                       axis.title      = element_text(size = 20, colour = 'black'),
                       legend.text     = element_text(size = 13, colour = 'black'),
                       legend.title    = element_blank(),
                       plot.title      = element_text(size = 25, colour = 'black'),
                       plot.subtitle   = element_text(size = 17, colour = 'black'),
                       strip.text.x    = element_text(size = 17, colour = 'black'),
                       strip.text.y    = element_text(size = 17, colour = 'black'),
                       plot.caption    = element_text(size = 15, hjust = 0, colour = 'black'),
                       legend.position = "bottom") +
        ggplot2::guides(colour = guide_legend(nrow = 2)) +
        ggplot2::labs(subtitle = lbls$Name[lbls$Indicator == paste0('Indicator',i)],
                      x = 'Time',
                      y = ifelse(!is.na(lbls$Units[lbls$Indicator == paste0('Indicator',i)]),lbls$Units[lbls$Indicator == paste0('Indicator',i)],''))
      ggplot2::ggsave(filename = paste0(root,'/2021/profiles/BGD/results/plots/time_series/indicator',i,'_time_series.svg'),
                      plot = gg, device = 'svg', units = 'in', width = 12, height = 8, dpi = 350)
      ggplot2::ggsave(filename = paste0(root,'/2021/profiles/BGD/results/plots/time_series/indicator',i,'_time_series.png'),
                      plot = gg, device = 'png', units = 'in', width = 12, height = 8, dpi = 350)
    } else {
      cat(paste0('Indicator ',i,' does not have information over time\n'))
    }
    return(cat(paste0('Indicator ',i,' ready!\n')))
  })
