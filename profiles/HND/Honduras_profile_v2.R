# R options
g <- gc(reset = T); rm(list = ls()) # Empty garbage collector
# .rs.restartR()                      # Restart R session
options(warn = -1, scipen = 999)    # Remove warning alerts and scientific notation
suppressMessages(if(!require(pacman)){install.packages('pacman');library(pacman)} else {library(pacman)})
suppressMessages(pacman::p_load(tidyverse, MetBrewer))

root <- 'D:/OneDrive - CGIAR/Sustainable Food Systems'

# Summarizing metrics
sts <- 1:30 %>%
  purrr::map(.f = function(i){
    
    ref <- readxl::read_excel(path = paste0(root,'/2021/profiles/HND/results/HND_calculations_v2.xlsx'), sheet = i) %>% base::as.data.frame()
    ind <- unique(as.character(ref[,3]))
    per <- paste0(median(ref$Initial_year, na.rm = T),'-',median(ref$Recent_year, na.rm = T))
    lst <- median(ref$Recent_year, na.rm = T)
    ref <- ref[,c('Initial','Recent','rate_of_change','rate_of_change_cleaned','Neighbours')]
    sts <- rbind(ref %>%
                   dplyr::filter(Neighbours != 'World') %>%
                   dplyr::group_by(Neighbours) %>%
                   dplyr::summarise_all(mean, na.rm = T),
                 ref %>%
                   dplyr::mutate(Neighbours = 'World') %>%
                   dplyr::group_by(Neighbours) %>%
                   dplyr::summarise_all(mean, na.rm = T)) %>% base::as.data.frame()
    
    names(sts) <- c('Group','Initial','Recent','Mean_delta','Cleaned_mean_delta')
    sts$Indicator <- ind; rm(ind)
    sts$Order <- paste0('Indicator',i)
    sts$Period <- per; rm(per)
    sts$Last_year  <- lst; rm(lst)
    sts <- sts %>% dplyr::mutate_if(is.numeric, round, 1)
    sts$Group <- factor(x = sts$Group,
                        levels = c('World','GDP pc comparable','Geographic neighboring','Honduras'),
                        labels = c('Global average','Countries with similar GDP pc','Geographic neighbors','Honduras'),
                        ordered = T)
    return(sts) # sts; cat(i,'\n')
    
  }) %>%
  dplyr::bind_rows()

# Saving the summarization metrics
out <- paste0(root,'/2021/profiles/HND/results/HND_ind_summary.xlsx')
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
    
    ggplot2::ggsave(filename = paste0(root,'/2021/profiles/HND/results/plots/points/',vr,'_graph.svg'),
                    plot = gg, device = 'svg', units = 'in', width = 11, height = 10, dpi = 350)
    ggplot2::ggsave(filename = paste0(root,'/2021/profiles/HND/results/plots/points/',vr,'_graph.png'),
                    plot = gg, device = 'png', units = 'in', width = 11, height = 10, dpi = 350)
    
  })

lbls <- readxl::read_excel(path = paste0(root,'/2021/profiles/info.xlsx'), sheet = 'HND')

# Graph 2: time series
1:30 %>%
  purrr::map(.f = function(i){
    
    rch <- readxl::read_excel(path = paste0(root,'/2021/profiles/HND/results/HND_calculations_v2.xlsx'), sheet = i)
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
      ggplot2::ggsave(filename = paste0(root,'/2021/profiles/HND/results/plots/time_series/indicator',i,'_time_series.svg'),
                      plot = gg, device = 'svg', units = 'in', width = 12, height = 8, dpi = 350)
      ggplot2::ggsave(filename = paste0(root,'/2021/profiles/HND/results/plots/time_series/indicator',i,'_time_series.png'),
                      plot = gg, device = 'png', units = 'in', width = 12, height = 8, dpi = 350)
    } else {
      cat(paste0('Indicator ',i,' does not have information over time\n'))
    }
    return(cat(paste0('Indicator ',i,' ready!\n')))
  })






















































# ### Lollipop
# sts$Indicator <- factor(x = sts$Indicator, levels = unique(sts$Indicator), ordered = T)
# gg <- sts %>%
#   ggplot2::ggplot(aes(x = Group, y = Cleaned_mean, label = Max)) + 
#   ggplot2::geom_segment(aes(x = Group, y = 0, yend = Mean, xend = Group), size = 2)+ 
#   ggplot2::geom_point(stat = 'identity', col = '#74DAF3', size = 14)  +
#   ggplot2::guides(colour = guide_legend(override.aes = list(size = 2))) +
#   ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', color = 'red', size = 1.5) +
#   ggplot2::geom_text(color = "black", size = 5) + # nudge_x = -0.5
#   ggplot2::coord_flip() +
#   ggplot2::xlab('') +
#   ggplot2::ylab('Relative change') +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(text            = element_text(size = 17, colour = 'black'),
#                  axis.text       = element_text(size = 16, colour = 'black'),
#                  axis.title      = element_text(size = 20, colour = 'black'),
#                  legend.text     = element_text(size = 13, colour = 'black'),
#                  legend.title    = element_blank(),
#                  plot.title      = element_text(size = 25, colour = 'black'),
#                  plot.subtitle   = element_text(size = 17, colour = 'black'),
#                  strip.text.x    = element_text(size = 17, colour = 'black'),
#                  strip.text.y    = element_text(size = 17, colour = 'black'),
#                  plot.caption    = element_text(size = 15, hjust = 0, colour = 'black'),
#                  legend.position = "bottom") +
#   ggplot2::facet_wrap(~Indicator, scales = 'free_x')
# 
# # drivers, actors, food_env, cons_behv, outcomes
# ggplot2::ggsave(filename = paste0(root,'/2021/profiles/HND/results/plots/outcomes.svg'),
#                 plot = gg, device = 'svg', units = 'in', width = 20, height = 12, dpi = 350)
# ggplot2::ggsave(filename = paste0(root,'/2021/profiles/HND/results/plots/outcomes.png'),
#                 plot = gg, device = 'png', units = 'in', width = 20, height = 12, dpi = 350)
# write.csv(x = sts, file = paste0(root,'/2021/profiles/HND/results/plots/outcomes.csv'), row.names = F)
# 
# 
# # outcomes
# sts <- 23:30 %>% # 7:11, 12:17, 18:22, 23:30
#   purrr::map(.f = function(i){
#     rch <- readxl::read_excel(path = paste0(root,'/2021/profiles/HND/results/HND_calculations.xlsx'), sheet = i)
#     rcs <- grep(pattern = '^RC', x = names(rch))
#     yrs <- gsub(pattern = 'RC', replacement = '', x = names(rch)[rcs[c(1,length(rcs))]])
#     # Table of interest
#     if('AvgRC_no_outliers' %in% names(rch)){ref <- rch[,c(paste0('Y',yrs),'AvgRC_no_outliers','Neighbours')]} else {ref <- rch[,c(paste0('Y',yrs),'AvgRC','Neighbours')]}
#     sts <- rbind(ref %>%
#                    dplyr::filter(Neighbours != 'World') %>%
#                    dplyr::group_by(Neighbours) %>%
#                    dplyr::summarise_all(mean, na.rm = T),
#                  ref %>%
#                    dplyr::mutate(Neighbours = 'World') %>%
#                    dplyr::group_by(Neighbours) %>%
#                    dplyr::summarise_all(mean, na.rm = T))
#     
#     names(sts) <- c('Group','Min','Max','Mean')
#     sts$Indicator <- rch %>% base::as.data.frame() %>% .[,3] %>% unique()
#     sts$Order <- paste0('Indicator',i)
#     sts$Category <- 'Drivers'
#     sts$Period <- paste(yrs, collapse = '-')
#     sts$Lst_year <- yrs[2]
#     sts <- sts %>% dplyr::mutate_if(is.numeric, round, 1)
#     sts$Group <- factor(x = sts$Group,
#                         levels = c('World','GDP pc comparable','Geographic neighboring','Honduras'),
#                         labels = c('Global average','Countries with similar GDP pc','Geographic neighbors','Honduras'),
#                         ordered = T)
#     return(sts)
#   }) %>%
#   dplyr::bind_rows()
# 
# gg <- ggplot2::ggplot(sts) +
#   ggplot2::geom_segment(aes(x = 1, xend = 2, y = Min, yend = Max),size = 1.6, col = "#6ba0c7") + # Draw straight line between two points
#   ### Set location, size, and color of points
#   ggplot2::geom_point(aes(x = 1, y = Min, colour = Group), size = 22, alpha = 1) +
#   ggplot2::geom_point(aes(x = 2, y = Max, colour = Group), size = 22, alpha = 1) +
#   #ggplot2::geom_text(aes(x = .95, y = Min, label = paste0(Group)), size = 6, col = "black", vjust = -0.3, hjust = 1) + # Display names of variables, initial and ending years
#   ggplot2::geom_text(aes(x = 1, y = Min, label = paste0(round(Min,1))), size = 6, col = "black", hjust = 2) + # , vjust = -1
#   ggplot2::geom_text(aes(x = 2, y = Max, label = paste0(round(Max,1))), size = 6, col = "black", hjust = -1) + # , vjust = -1
#   ggplot2::geom_text(aes(x = 1, y = max(Min,Max) + 2.8, label = yrs[1]), size = 8, col = "black", fontface = "bold") +
#   ggplot2::geom_text(aes(x = 2, y = max(Min,Max) + 2.8, label = yrs[2]), size = 8, col = "black", fontface = "bold") +
#   ggplot2::scale_colour_manual(values = MetBrewer::met.brewer(name = 'Cassatt1', n = 4)) +
#   ggplot2::xlim(0.5, 2.5) +
#   ggplot2::ylim(min(c(sts$Min,sts$Max)) - 0.5, max(c(sts$Min,sts$Max)) + 3) +
#   ggplot2::theme_minimal() +
#   ### Add title and data source
#   ggplot2::labs(title=unique(sts$Indicator)) +
#   ###  Set size and position of title and source
#   ggplot2::theme(text            = element_text(size = 17, colour = 'black'),
#                  axis.text       = element_text(size = 16, colour = 'black'),
#                  legend.text     = element_text(size = 13, colour = 'black'),
#                  legend.title    = element_blank(),
#                  plot.title      = element_text(size = 25, colour = 'black'),
#                  plot.subtitle   = element_text(size = 17, colour = 'black'),
#                  strip.text.x    = element_text(size = 17, colour = 'black'),
#                  strip.text.y    = element_text(size = 17, colour = 'black'),
#                  plot.caption    = element_text(size = 15, hjust = 0, colour = 'black'),
#                  legend.position = "bottom",
#                  axis.title.y = element_blank(),
#                  axis.title.x = element_blank(),
#                  axis.text.x  = element_blank(),
#                  axis.text.y  = element_blank())
# # drivers, actors, food_env, cons_behv, outcomes
# ggplot2::ggsave(filename = paste0(root,'/2021/profiles/HND/results/plots/drivers_ind3.svg'),
#                 plot = gg, device = 'svg', units = 'in', width = 11, height = 10, dpi = 350)
# ggplot2::ggsave(filename = paste0(root,'/2021/profiles/HND/results/plots/drivers_ind3.png'),
#                 plot = gg, device = 'png', units = 'in', width = 11, height = 10, dpi = 350)
# 
# 
# 
# 
# ggplot2::ggplot(sts) +
#   ggplot2::geom_segment(aes(x = 1, xend = 2,
#                             y = Y2009, yend = Y2019),
#                         size = 1.6, col = "#6ba0c7") + # Draw straight line between two points
#   ggplot2::geom_text(aes(x = .93, y = Y2009, label = paste0(Neighbours)), size = 12, col = "black", vjust = -0.3, hjust = 1) + # Display names of variables, initial and ending years
#   ggplot2::geom_text(aes(x = 1.01, y = Y2009, label = paste0(round(Y2009,1))), size = 8.6, col = "black", vjust = -1) +
#   ggplot2::geom_text(aes(x = 2, y = Y2019, label = paste0(round(Y2019,1))), size = 8.6, col = "black", vjust = -1) +
#   ### Set location, size, and color of points
#   ggplot2::geom_point(aes(x = 1, y = Y2009, colour = Neighbours), size = 16, alpha = 1) +
#   ggplot2::geom_point(aes(x = 2, y = Y2019, colour = Neighbours), size = 16, alpha = 1) +
#   ggplot2::geom_text(aes(x = 0.5, y = 90, label = "2009"), size = 10, col = "black", hjust = -2) +
#   ggplot2::geom_text(aes(x = 2.5, y = 90, label = "2019"), size = 10, col = "black", hjust = 1.5) +
#   ggplot2::scale_colour_manual(values = MetBrewer::met.brewer(name = 'Egypt', n = 4)) +
#   ggplot2::theme_minimal() +
#   ### Add title and data source 
#   ggplot2::labs(title="Nutrient nitrogen N (total)")+
#   ###  Set size and position of title and source
#   ggplot2::theme(plot.title   = element_text(size = 26, hjust = .5),
#                  plot.caption = element_text(size = 16, hjust = 0.5, vjust = 4),
#                  axis.title.y = element_blank(), axis.title.x = element_blank(),
#                  axis.text.x  = element_blank(), axis.text.y = element_blank(),
#                  legend.position = 'none')
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ref <- rch[,c('Reference_no_outliers','Neighbours')]
# # sts <- rbind(ref %>%
# #                dplyr::filter(Neighbours != 'World') %>%
# #                dplyr::group_by(Neighbours) %>%
# #                dplyr::summarise_all(.tbl = ., funs(Min = min(.),
# #                                                    Max = max(.),
# #                                                    Mean = mean(.)), na.rm = T),
# #              ref %>%
# #                dplyr::mutate(Neighbours = 'World') %>%
# #                dplyr::group_by(Neighbours) %>%
# #                dplyr::summarise_all(.tbl = ., funs(Min = min(.),
# #                                                    Max = max(.),
# #                                                    Mean = mean(.)), na.rm = T))
# 
# sts %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_pointrange(aes(x      = Group,
#                                y      = Mean,
#                                ymin   = Min,
#                                ymax   = Max,
#                                colour = Group), size = 1.5) +
#   ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
#   ggplot2::scale_x_discrete(expand = c(1.3, 0)) +
#   ggplot2::scale_colour_manual(values = MetBrewer::met.brewer(name = 'Egypt', n = 4)) +
#   ggplot2::coord_flip() +
#   ggplot2::theme_bw() +
#   ggplot2::theme(text            = element_text(size = 17, colour = 'black'),
#                  axis.text       = element_text(size = 16, colour = 'black'),
#                  axis.title      = element_text(size = 20, colour = 'black'),
#                  legend.text     = element_text(size = 13, colour = 'black'),
#                  legend.title    = element_blank(),
#                  plot.title      = element_text(size = 25, colour = 'black'),
#                  plot.subtitle   = element_text(size = 17, colour = 'black'),
#                  strip.text.x    = element_text(size = 17, colour = 'black'),
#                  strip.text.y    = element_text(size = 17, colour = 'black'),
#                  plot.caption    = element_text(size = 15, hjust = 0, colour = 'black'),
#                  legend.position = "none") +
#   ggplot2::xlab('') +
#   ggplot2::ylab('') +
#   ggplot2::labs(title = 'Nutrient nitrogen N (total)',
#                 subtitle = 'Rate of change (%)')
# 
# ###
# ### Create a data frame 
# outcome<-c("IPV", "GBV", "Both")
# baseline<-c(74, 60, 41)
# month_3<-c(63, 29,15)
# 
# project_impact<-cbind.data.frame(outcome, baseline, month_3)
# project_impact
# 
# slope_plot<-ggplot(project_impact)+
#   ### Draw straight line between two points
#   geom_segment(aes(x=1, xend=2, y=baseline, yend=month_3), size=1.6, col="#6ba0c7")+ 
#   ### Display names of variables, baseline and 3 month results
#   geom_text(aes(x=.93, y=baseline,label = paste0(outcome)),
#             size=12,col="black", vjust=-0.3, hjust=1) +geom_text(aes(x=1.01, y = baseline,
#                                                                      label = paste0(baseline,"%")), size=8.6, col="black", vjust=-1
#             )+geom_text(aes(x = 2, 
#                             y = month_3, 
#                             label = paste0( 
#                               month_3,"%")), size=8.6, col="black", vjust=-1)+
#   ### Set location, size, and color of points
#   geom_point(aes(x = 1, 
#                  y = baseline), size = 16,
#              col = "#c7c06b", alpha=1, col="black")+
#   geom_point(aes(x = 2, 
#                  y = month_3), size=16,
#              col = "#c76b72", alpha=1)+
#   geom_text(aes(x=0.5, y=90, label="Baseline"), size=10, col="black",
#             hjust=-2)+
#   geom_text(aes(x=2.5, y=90, label="3 Month Follow-up"), size=10, col="black", hjust=1.5)+theme_minimal()+
#   ### Add title and data source 
#   labs(title="Prevalence of Intimate Partner Violence(IPV) and Gender Based Violence(GBV)",
#        caption=" Data Source: WINGS of Hope, Kyrgyzstan")+
#   ###  Set size and position of title and source
#   theme(plot.title = element_text(size=26, hjust=.5),
#         plot.caption = element_text(size =16, hjust = 0.5,vjust=4))+
#   ### Remove X and Y titles and texts 
#   theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
#         axis.text.x=element_blank(), axis.text.y = element_blank())+
#   ylim(0,95)
# ###
# 
# 
# 
# 
# 
# ###
# 
# # Summary table
# smy <- readxl::read_excel(path = paste0(root,'/2021/profiles/HND/data_revised/Honduras_indicators.xlsx'), sheet = 'Resume')
# smy <- smy[-which(is.na(smy$ISO)),]
# 
# # Groups for each country
# grp <- read.csv(paste0(root,'/2021/profiles/neighbours.csv'))
# 
# # Vectorized grep function
# grep2 <- Vectorize(FUN = grep, vectorize.args = 'pattern')
# # Function to compute relative change
# p_dff <- function(x){
#   if(length(as.numeric(na.omit(x))) >= round(0.8 * length(yrs), 0)){
#     if(any(x == 0, na.rm = T)){ x[which(x == 0)] <- x[which(x == 0)] + 0.00001 }
#     if(var(x, na.rm = T) != 0){
#       dff <- (x/dplyr::lag(x, n = 1)) - 1
#       dff <- dff %>% na.omit() %>% as.numeric() %>% sort()
#       ## Outliers detection
#       # Method 1: boxplot
#       bxp   <- boxplot(dff)
#       m1_id <- which(dff %in% bxp$out)
#       # Method 2: Robust Location and Scatter Estimation via MCD
#       dfm <- data.frame(dff = dff, time = 1:length(dff))
#       tryCatch(expr = {O3s <- dfm %>% OutliersO3::O3prep(method = c("MCD"), tols = .1, boxplotLimits = 6)}, error = function(e) cat('Error obtained\n'))
#       if(exists('O3s')){ # Check if Method 2 gets results
#         m2_id <- O3s$outList$outM[[1]]$outlierIndices
#         if(length(m1_id) > 0 | length(m2_id) > 0){
#           otlr <- unique(c(m1_id,m2_id))
#           dfm <- dfm[-otlr,]
#         }
#       }
#       if(length(m1_id) > 0){ # Check if Method 1 gets results
#         otlr <- m1_id
#         dfm <- dfm[-otlr,]
#       }
#       dff <- mean(dfm$dff)
#       return(dff)
#     } else { return(0) }
#   } else { return(NA) }
# }
# 
# # Summarize each indicator using: mean, median, sd, mad, min, and max
# tbls <- 1:30 %>%
#   purrr::map(.f = function(i){
#     # Define indicator of interest
#     sheet <- paste0('Indicator',i)
#     # Load the data for the specific indicator
#     df <- readxl::read_excel(path = paste0(root,'/2021/profiles/HND/data_revised/Honduras_indicators.xlsx'), sheet = sheet)
#     # Remove not needed columns
#     if('Country' %in% names(df)){df$Country <- NULL}
#     if('Element' %in% names(df)){df$Element <- NULL}
#     if('Unit' %in% names(df)){df$Unit <- NULL}
#     if(sheet == 'Indicator29'){
#       vars <- df %>% dplyr::pull(sheet) %>% unique() %>% as.character()
#       df1 <- df[df$Indicator29 == vars[1],]
#       df2 <- df[df$Indicator29 == vars[2],]
#       if(identical(df1$ISO, df2$ISO)){
#         df <- cbind(df1[1:2],round(df1[-(1:2)]/df2[-(1:2)],2))
#         df$Indicator29 <- 'Gender gap for obesity (women/men prevalence)'
#         rm(df1, df2)
#       }
#     }
#     
#     df <- base::as.data.frame(df)
#     # Identify last year column with information for country reference
#     col_id <- which(apply(X = base::as.data.frame(df[which(df$ISO == 'HND'),]), MARGIN = 2, FUN = function(x){!is.na(x)}))
#     # Identify start and ending years
#     end <- as.numeric(gsub(pattern = '[a-z]|[A-Z]', replacement = '', x = names(col_id[length(col_id)])))
#     yrs <<- names(df)[grep(pattern = '^Y[0-9]', x = names(df))]
#     yrs <<- yrs[1:grep(pattern = paste0('Y',end), x = yrs)]
#     l00 <- as.numeric(gsub(pattern = 'Y', replacement = '', x = yrs)) < 2000
#     if(sum(l00) == length(l00)){ # All years are inferior than 2000
#       yrs <<- yrs
#     } else {
#       if(any(l00)){
#         yrs <<- yrs[-which(as.numeric(gsub(pattern = 'Y', replacement = '', x = yrs)) < 2000)]
#       }
#     }
#     
#     ### Drivers
#     if(i %in% 1:6){
#       ini <- end - 10
#       yrs <<- yrs[grep(pattern = ini, x = yrs):length(yrs)]
#       # Time series values per country
#       tmsr <- df[,grep2(pattern = ini:end, x = names(df))]
#       
#       # Verify if the interest's indicator corresponds to relative change itself
#       iname <- unique(df %>% base::as.data.frame() %>% .[,sheet])
#       evl <- grep(pattern = '[cC]hange|[gG]rowth', x = iname, fixed = F)
#       if(length(evl) == 0){
#         df$Target <- apply(X = tmsr, MARGIN = 1, FUN = p_dff) * 100
#       } else {
#         df$Target <- apply(X = tmsr, MARGIN = 1, FUN = mean, na.rm = T)
#       }
#       ## The new column called 'Target' has the relative change for driver variables
#       df <- dplyr::left_join(x = df,
#                              y = grp %>%
#                                dplyr::filter(Reference == 'HND') %>%
#                                dplyr::select(ISO,Neighbours), by = 'ISO') %>%
#         base::as.data.frame()
#       smm <- df[,c('ISO',paste0('Y',c(ini,end)),'Neighbours','Target')] # Drivers
#       smm$Neighbours[is.na(smm$Neighbours)] <- 'World'
#       smm$Neighbours[which(smm$ISO == 'HND')] <- 'Honduras'
#       
#       stts <- smm %>%
#         psych::describeBy(x = ., group = 'Neighbours')
#       stts <- 1:length(stts) %>%
#         purrr::map(.f = function(j){
#           tbl <- base::as.data.frame(stts[[j]])
#           tbl <- tbl[,c('mean','median','sd','mad','min','max')] %>% as.matrix()
#           tbl <- round(tbl, 2)
#           tbl <- base::as.data.frame(tbl)
#           tbl$Time <- rownames(tbl)
#           rownames(tbl) <- 1:nrow(tbl)
#           tbl <- tbl[-grep2(pattern = c('ISO','Neighbours'), x = tbl$Time),]
#           tbl$Group    <- names(stts[j])
#           tbl$Variable <- iname
#           tbl$Order    <- sheet
#           return(tbl)
#         }) %>%
#         dplyr::bind_rows()
#       
#     } else { ### Non-drivers
#       # Condition if the number of available years is lower than 10 years
#       if(length(yrs) < 11){
#         ini <- as.numeric(gsub(pattern = '[a-z]|[A-Z]', replacement = '', x = yrs[1]))
#         tmsr <- df[,yrs]
#         if(is.vector(tmsr)){
#           tmsr <- data.frame(x = tmsr); colnames(tmsr) <- yrs
#         }
#         # Verify if the interest's indicator corresponds to relative change itself
#         iname <- unique(df %>% base::as.data.frame() %>% .[,sheet])
#         evl <- grep(pattern = '[cC]hange|[gG]rowth', x = iname, fixed = F)
#         if(length(evl) == 0 & length(yrs) > 1){
#           df$Target <- apply(X = tmsr, MARGIN = 1, FUN = p_dff) * 100
#           if(length(evl) == 0 & length(yrs) == 1){
#             df$Target <- apply(X = tmsr, MARGIN = 1, FUN = mean, na.rm = T)
#           }
#         } else {
#           df$Target <- apply(X = tmsr, MARGIN = 1, FUN = mean, na.rm = T)
#         }
#         ## The new column called 'Target' has the latest value
#         df <- dplyr::left_join(x = df,
#                                y = grp %>%
#                                  dplyr::filter(Reference == 'HND') %>%
#                                  dplyr::select(ISO,Neighbours), by = 'ISO') %>%
#           base::as.data.frame()
#         smm <- df[,unique(c('ISO',paste0('Y',c(ini,end)),'Neighbours','Target'))] # Non-drivers
#         smm$Neighbours[is.na(smm$Neighbours)] <- 'World'
#         smm$Neighbours[which(smm$ISO == 'HND')] <- 'Honduras'
#         
#         stts <- smm %>%
#           psych::describeBy(x = ., group = 'Neighbours')
#         stts <- 1:length(stts) %>%
#           purrr::map(.f = function(j){
#             tbl <- base::as.data.frame(stts[[j]])
#             tbl <- tbl[,c('mean','median','sd','mad','min','max')] %>% as.matrix()
#             tbl <- round(tbl, 2)
#             tbl <- base::as.data.frame(tbl)
#             tbl$Time <- rownames(tbl)
#             rownames(tbl) <- 1:nrow(tbl)
#             tbl <- tbl[-grep2(pattern = c('ISO','Neighbours'), x = tbl$Time),]
#             tbl$Group    <- names(stts[j])
#             tbl$Variable <- iname
#             tbl$Order    <- sheet
#             return(tbl)
#           }) %>%
#           dplyr::bind_rows()
#         
#       } else {
#         # Identify starting year
#         ini  <- end - 10
#         yrs <<- yrs[grep(pattern = ini, x = yrs):length(yrs)]
#         # Time series values per country
#         tmsr <- df[,grep2(pattern = ini:end, x = names(df))]
#         # Verify if the interest's indicator corresponds to relative change itself
#         iname <- unique(df %>% base::as.data.frame() %>% .[,sheet])
#         evl <- grep(pattern = '[cC]hange|[gG]rowth', x = iname, fixed = F)
#         if(length(evl) == 0){
#           df$Target <- apply(X = tmsr, MARGIN = 1, FUN = p_dff) * 100
#         } else {
#           df$Target <- apply(X = tmsr, MARGIN = 1, FUN = median, na.rm = T)
#         }
#         ## The new column called 'Target' has the relative change for driver variables
#         df <- dplyr::left_join(x = df,
#                                y = grp %>%
#                                  dplyr::filter(Reference == 'HND') %>%
#                                  dplyr::select(ISO,Neighbours), by = 'ISO') %>%
#           base::as.data.frame()
#         smm <- df[,c('ISO',paste0('Y',c(ini,end)),'Neighbours','Target')] # Drivers
#         smm$Neighbours[is.na(smm$Neighbours)] <- 'World'
#         smm$Neighbours[which(smm$ISO == 'HND')] <- 'Honduras'
#         
#         stts <- smm %>%
#           psych::describeBy(x = ., group = 'Neighbours')
#         stts <- 1:length(stts) %>%
#           purrr::map(.f = function(j){
#             tbl <- base::as.data.frame(stts[[j]])
#             tbl <- tbl[,c('mean','median','sd','mad','min','max')] %>% as.matrix()
#             tbl <- round(tbl, 2)
#             tbl <- base::as.data.frame(tbl)
#             tbl$Time <- rownames(tbl)
#             rownames(tbl) <- 1:nrow(tbl)
#             tbl <- tbl[-grep2(pattern = c('ISO','Neighbours'), x = tbl$Time),]
#             tbl$Group    <- names(stts[j])
#             tbl$Variable <- iname
#             tbl$Order    <- sheet
#             return(tbl)
#           }) %>%
#           dplyr::bind_rows()
#       }
#     }
#     return(stts)
#   }) %>%
#   dplyr::bind_rows()
# tbls$Component <- NA
# tbls$Component[tbls$Order %in% paste0('Indicator',1:6)] <- 'Drivers'
# tbls$Component[tbls$Order %in% paste0('Indicator',7:11)] <- 'Supply actors and activities'
# tbls$Component[tbls$Order %in% paste0('Indicator',12:17)] <- 'Food Environment'
# tbls$Component[tbls$Order %in% paste0('Indicator',18:22)] <- 'Consumer choice'
# tbls$Component[tbls$Order %in% paste0('Indicator',23:30)] <- 'Outcomes'
# 
# utils::write.csv(x = tbls, file = paste0(root,'/2021/profiles/HND/results/HND_statistics.csv'), row.names = F)
# 
# # ------------------------------------------------------------------------------------- #
# 
# tbls %>%
#   dplyr::filter(Order == 'Indicator1' & Time == 'Target') %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_pointrange(aes(x      = Group,
#                                y      = median,
#                                ymin   = min,
#                                ymax   = max,
#                                colour = Group), size = 1.5) +
#   ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
#   ggplot2::scale_x_discrete(expand = c(1.3, 0)) +
#   ggplot2::scale_colour_manual(values = MetBrewer::met.brewer(name = 'Egypt', n = 4)) +
#   ggplot2::coord_flip() +
#   ggplot2::theme_bw() +
#   ggplot2::theme(text            = element_text(size = 17, colour = 'black'),
#                  axis.text       = element_text(size = 16, colour = 'black'),
#                  axis.title      = element_text(size = 20, colour = 'black'),
#                  legend.text     = element_text(size = 13, colour = 'black'),
#                  legend.title    = element_blank(),
#                  plot.title      = element_text(size = 25, colour = 'black'),
#                  plot.subtitle   = element_text(size = 17, colour = 'black'),
#                  strip.text.x    = element_text(size = 17, colour = 'black'),
#                  strip.text.y    = element_text(size = 17, colour = 'black'),
#                  plot.caption    = element_text(size = 15, hjust = 0, colour = 'black'),
#                  legend.position = "none") +
#   ggplot2::xlab('') +
#   ggplot2::ylab('') +
#   ggplot2::labs(title = 'Nutrient nitrogen N (total)',
#                 subtitle = 'Rate of change (%)')
# 
# 
# 
# 
# 
# 
# tbls$Min <- NA
# tbls$Max <- NA
# 
# tbls$Min[tbls$Order == 'Indicator1'] <- 0; tbls$Max[tbls$Order == 'Indicator1'] <- 200
# tbls$Min[tbls$Order == 'Indicator2'] <- 0; tbls$Max[tbls$Order == 'Indicator2'] <- 2
# tbls$Min[tbls$Order == 'Indicator3'] <- 0; tbls$Max[tbls$Order == 'Indicator3'] <- 100
# tbls$Min[tbls$Order == 'Indicator4'] <- 0; tbls$Max[tbls$Order == 'Indicator4'] <- 20
# tbls$Min[tbls$Order == 'Indicator5'] <- 0; tbls$Max[tbls$Order == 'Indicator5'] <- 8
# tbls$Min[tbls$Order == 'Indicator6'] <- 0; tbls$Max[tbls$Order == 'Indicator6'] <- 7
# tbls$Min[tbls$Order == 'Indicator7'] <- 0; tbls$Max[tbls$Order == 'Indicator7'] <- 100
# tbls$Min[tbls$Order == 'Indicator8'] <- 0; tbls$Max[tbls$Order == 'Indicator8'] <- 110
# tbls$Min[tbls$Order == 'Indicator9'] <- 0; tbls$Max[tbls$Order == 'Indicator9'] <- 100
# tbls$Min[tbls$Order == 'Indicator10'] <- 0; tbls$Max[tbls$Order == 'Indicator10'] <- 100
# tbls$Min[tbls$Order == 'Indicator11'] <- 0; tbls$Max[tbls$Order == 'Indicator11'] <- 50
# tbls$Min[tbls$Order == 'Indicator12'] <- 0; tbls$Max[tbls$Order == 'Indicator12'] <- 130
# tbls$Min[tbls$Order == 'Indicator13'] <- 0; tbls$Max[tbls$Order == 'Indicator13'] <- 20
# tbls$Min[tbls$Order == 'Indicator14'] <- 0; tbls$Max[tbls$Order == 'Indicator14'] <- 600
# tbls$Min[tbls$Order == 'Indicator15'] <- 0; tbls$Max[tbls$Order == 'Indicator15'] <- 100
# tbls$Min[tbls$Order == 'Indicator16'] <- 0; tbls$Max[tbls$Order == 'Indicator16'] <- 1
# tbls$Min[tbls$Order == 'Indicator17'] <- 0; tbls$Max[tbls$Order == 'Indicator17'] <- 20
# tbls$Min[tbls$Order == 'Indicator18'] <- 0; tbls$Max[tbls$Order == 'Indicator18'] <- 400
# tbls$Min[tbls$Order == 'Indicator19'] <- 0; tbls$Max[tbls$Order == 'Indicator19'] <- 80
# tbls$Min[tbls$Order == 'Indicator20'] <- 15; tbls$Max[tbls$Order == 'Indicator20'] <- 35
# tbls$Min[tbls$Order == 'Indicator21'] <- 0; tbls$Max[tbls$Order == 'Indicator21'] <- 100
# tbls$Min[tbls$Order == 'Indicator22'] <- 0; tbls$Max[tbls$Order == 'Indicator22'] <- 100
# tbls$Min[tbls$Order == 'Indicator23'] <- 0; tbls$Max[tbls$Order == 'Indicator23'] <- 150000
# tbls$Min[tbls$Order == 'Indicator24'] <- 30000; tbls$Max[tbls$Order == 'Indicator24'] <- 150000
# tbls$Min[tbls$Order == 'Indicator25'] <- 0; tbls$Max[tbls$Order == 'Indicator25'] <- 30
# tbls$Min[tbls$Order == 'Indicator26'] <- 5; tbls$Max[tbls$Order == 'Indicator26'] <- 25
# tbls$Min[tbls$Order == 'Indicator27'] <- 0; tbls$Max[tbls$Order == 'Indicator27'] <- 20000
# tbls$Min[tbls$Order == 'Indicator28'] <- 0; tbls$Max[tbls$Order == 'Indicator28'] <- 100
# tbls$Min[tbls$Order == 'Indicator29'] <- 0; tbls$Max[tbls$Order == 'Indicator29'] <- 5
# tbls$Min[tbls$Order == 'Indicator30'] <- 0; tbls$Max[tbls$Order == 'Indicator30'] <- 100
# 
# aux <- unique(tbls[,c('Indicator','Order')]) %>% base::as.data.frame()
# rownames(aux) <- 1:nrow(aux)
# 
# order.labs <- aux$Indicator
# names(order.labs) <- aux$Order
# rm(aux)
# 
# lst <- unique(tbls$Component)
# 
# # First alternative
# lst %>%
#   purrr::map(.f = function(lbl){
#     # Plot the means across the groups
#     gg <- tbls %>%
#       dplyr::mutate(Order = factor(Order, levels = paste0('Indicator',1:30))) %>%
#       dplyr::filter(Component == lbl) %>%
#       ggplot2::ggplot(aes(x = Order, y = Mean, colour = Neighbours)) +
#       ggplot2::geom_point(size = 10) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group != 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(0.7), width = 0.2, size = 3) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group == 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(1), width = 0.2, size = 3, alpha = 0.4) +
#       ggplot2::xlab(lbl) +
#       ggplot2::ylab('') +
#       ggplot2::coord_flip() +
#       ggplot2::facet_wrap(~Order, ncol = 1, scales = 'free', labeller = labeller(Order = order.labs)) +
#       ggplot2::theme_bw() +
#       ggplot2::scale_colour_brewer(palette = 'Set1') +
#       ggplot2::theme(axis.text.y = element_blank(),
#                      axis.text.x = element_text(size = 30, colour = 'black'),
#                      axis.title = element_text(size = 40, colour = 'black'),
#                      legend.text = element_text(size = 25, colour = 'black'),
#                      legend.title = element_text(size = 40, colour = 'black'),
#                      strip.text.x = element_text(size = 27, colour = 'black', face = 'bold'),
#                      legend.key.height = unit(2, units = 'cm'))
#     ggplot2::ggsave(filename = paste0(root,'/2021/profiles/HND/automatic/Honduras_',gsub(pattern = ' ', replacement = '_', x = lbl),'.jpeg'), plot = gg, device = 'jpeg', width = 19, height = 20, units = 'in')
#   })
# 
# pacman::p_load(ggpubr)
# 
# # Second alternative
# unique(tbls[,c('Order','Component')])
# 
# # Drivers
# drvs <- paste0('Indicator',1:6) %>%
#   purrr::map(.f = function(ind){
#     df <- tbls %>%
#       dplyr::filter(Order == ind)
#     lms <- c(unique(df$Min),unique(df$Max))
#     gg <- df %>%
#       ggplot2::ggplot(aes(x = Order, y = Mean, colour = Group)) +
#       ggplot2::geom_point(size = 10) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group != 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(0.7), width = 0.2, size = 3) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group == 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(1), width = 0.2, size = 3, alpha = 0.4) +
#       ggplot2::ylim(lms) +
#       ggplot2::xlab('') +
#       ggplot2::ylab('') +
#       ggplot2::coord_flip() +
#       ggplot2::facet_wrap(~Order, ncol = 1, scales = 'free', labeller = labeller(Order = order.labs)) +
#       ggplot2::theme_bw() +
#       ggplot2::scale_colour_brewer(palette = 'Set1') +
#       ggplot2::theme(axis.text.y = element_blank(),
#                      axis.text.x = element_text(size = 30, colour = 'black'),
#                      axis.title = element_text(size = 40, colour = 'black'),
#                      legend.text = element_text(size = 25, colour = 'black'),
#                      legend.title = element_text(size = 40, colour = 'black'),
#                      strip.text.x = element_text(size = 27, colour = 'black', face = 'bold'),
#                      legend.key.height = unit(2, units = 'cm'),
#                      legend.position = "none")
#     return(gg)
#   })
# tst <- ggpubr::ggarrange(plotlist = drvs, ncol = 1)
# ggplot2::ggsave(filename = paste0(root,'/2021/profiles/manual/Honduras_Drivers2.jpeg'), plot = tst, device = 'jpeg', width = 19, height = 20, units = 'in')
# 
# # Supply actors and activities
# sply <- paste0('Indicator',7:11) %>%
#   purrr::map(.f = function(ind){
#     df <- tbls %>%
#       dplyr::filter(Order == ind)
#     lms <- c(unique(df$Min),unique(df$Max))
#     gg <- df %>%
#       ggplot2::ggplot(aes(x = Order, y = Mean, colour = Group)) +
#       ggplot2::geom_point(size = 10) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group != 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(0.7), width = 0.2, size = 3) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group == 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(1), width = 0.2, size = 3, alpha = 0.4) +
#       ggplot2::ylim(lms) +
#       ggplot2::xlab('') +
#       ggplot2::ylab('') +
#       ggplot2::coord_flip() +
#       ggplot2::facet_wrap(~Order, ncol = 1, scales = 'free', labeller = labeller(Order = order.labs)) +
#       ggplot2::theme_bw() +
#       ggplot2::scale_colour_brewer(palette = 'Set1') +
#       ggplot2::theme(axis.text.y = element_blank(),
#                      axis.text.x = element_text(size = 30, colour = 'black'),
#                      axis.title = element_text(size = 40, colour = 'black'),
#                      legend.text = element_text(size = 25, colour = 'black'),
#                      legend.title = element_text(size = 40, colour = 'black'),
#                      strip.text.x = element_text(size = 27, colour = 'black', face = 'bold'),
#                      legend.key.height = unit(2, units = 'cm'),
#                      legend.position = "none")
#     return(gg)
#   })
# tst <- ggpubr::ggarrange(plotlist = sply, ncol = 1)
# ggplot2::ggsave(filename = paste0(root,'/2021/profiles/manual/Honduras_Supply_actors_and_activities2.jpeg'), plot = tst, device = 'jpeg', width = 19, height = 20, units = 'in')
# 
# # Food Environment
# fden <- paste0('Indicator',12:17) %>%
#   purrr::map(.f = function(ind){
#     df <- tbls %>%
#       dplyr::filter(Order == ind)
#     lms <- c(unique(df$Min),unique(df$Max))
#     gg <- df %>%
#       ggplot2::ggplot(aes(x = Order, y = Mean, colour = Group)) +
#       ggplot2::geom_point(size = 10) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group != 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(0.7), width = 0.2, size = 3) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group == 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(1), width = 0.2, size = 3, alpha = 0.4) +
#       ggplot2::ylim(lms) +
#       ggplot2::xlab('') +
#       ggplot2::ylab('') +
#       ggplot2::coord_flip() +
#       ggplot2::facet_wrap(~Order, ncol = 1, scales = 'free', labeller = labeller(Order = order.labs)) +
#       ggplot2::theme_bw() +
#       ggplot2::scale_colour_brewer(palette = 'Set1') +
#       ggplot2::theme(axis.text.y = element_blank(),
#                      axis.text.x = element_text(size = 30, colour = 'black'),
#                      axis.title = element_text(size = 40, colour = 'black'),
#                      legend.text = element_text(size = 25, colour = 'black'),
#                      legend.title = element_text(size = 40, colour = 'black'),
#                      strip.text.x = element_text(size = 27, colour = 'black', face = 'bold'),
#                      legend.key.height = unit(2, units = 'cm'),
#                      legend.position = "none")
#     return(gg)
#   })
# tst <- ggpubr::ggarrange(plotlist = fden, ncol = 1)
# ggplot2::ggsave(filename = paste0(root,'/2021/profiles/manual/Honduras_Food_Environment2.jpeg'), plot = tst, device = 'jpeg', width = 19, height = 20, units = 'in')
# 
# # Consumer choice
# csch <- paste0('Indicator',18:22) %>%
#   purrr::map(.f = function(ind){
#     df <- tbls %>%
#       dplyr::filter(Order == ind)
#     lms <- c(unique(df$Min),unique(df$Max))
#     gg <- df %>%
#       ggplot2::ggplot(aes(x = Order, y = Mean, colour = Group)) +
#       ggplot2::geom_point(size = 10) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group != 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(0.7), width = 0.2, size = 3) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group == 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(1), width = 0.2, size = 3, alpha = 0.4) +
#       ggplot2::ylim(lms) +
#       ggplot2::xlab('') +
#       ggplot2::ylab('') +
#       ggplot2::coord_flip() +
#       ggplot2::facet_wrap(~Order, ncol = 1, scales = 'free', labeller = labeller(Order = order.labs)) +
#       ggplot2::theme_bw() +
#       ggplot2::scale_colour_brewer(palette = 'Set1') +
#       ggplot2::theme(axis.text.y = element_blank(),
#                      axis.text.x = element_text(size = 30, colour = 'black'),
#                      axis.title = element_text(size = 40, colour = 'black'),
#                      legend.text = element_text(size = 25, colour = 'black'),
#                      legend.title = element_text(size = 40, colour = 'black'),
#                      strip.text.x = element_text(size = 27, colour = 'black', face = 'bold'),
#                      legend.key.height = unit(2, units = 'cm'),
#                      legend.position = "none")
#     return(gg)
#   })
# tst <- ggpubr::ggarrange(plotlist = csch, ncol = 1)
# ggplot2::ggsave(filename = paste0(root,'/2021/profiles/manual/Honduras_Consumer_choice2.jpeg'), plot = tst, device = 'jpeg', width = 19, height = 20, units = 'in')
# 
# # Outcomes
# outc <- paste0('Indicator',23:30) %>%
#   purrr::map(.f = function(ind){
#     df <- tbls %>%
#       dplyr::filter(Order == ind)
#     lms <- c(unique(df$Min),unique(df$Max))
#     gg <- df %>%
#       ggplot2::ggplot(aes(x = Order, y = Mean, colour = Group)) +
#       ggplot2::geom_point(size = 10) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group != 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(0.7), width = 0.2, size = 3) +
#       # ggplot2::geom_pointrange(data = . %>% dplyr::filter(Group == 'World'), aes(ymin = Mean-Sd, ymax = Mean+Sd), position = position_dodge(1), width = 0.2, size = 3, alpha = 0.4) +
#       ggplot2::ylim(lms) +
#       ggplot2::xlab('') +
#       ggplot2::ylab('') +
#       ggplot2::coord_flip() +
#       ggplot2::facet_wrap(~Order, ncol = 1, scales = 'free', labeller = labeller(Order = order.labs)) +
#       ggplot2::theme_bw() +
#       ggplot2::scale_colour_brewer(palette = 'Set1') +
#       ggplot2::theme(axis.text.y = element_blank(),
#                      axis.text.x = element_text(size = 30, colour = 'black'),
#                      axis.title = element_text(size = 40, colour = 'black'),
#                      legend.text = element_text(size = 25, colour = 'black'),
#                      legend.title = element_text(size = 40, colour = 'black'),
#                      strip.text.x = element_text(size = 27, colour = 'black', face = 'bold'),
#                      legend.key.height = unit(2, units = 'cm'),
#                      legend.position = "none")
#     return(gg)
#   })
# tst <- ggpubr::ggarrange(plotlist = outc, ncol = 1)
# ggplot2::ggsave(filename = paste0(root,'/2021/profiles/manual/Honduras_Outcomes2.jpeg'), plot = tst, device = 'jpeg', width = 19, height = 20, units = 'in')
# 
# if(!file.exists(paste0(root,'/2021/profiles/Honduras_statistics.csv'))){
#   write.csv(tbls, paste0(root,'/2021/profiles/Honduras_statistics.csv'), row.names = F)
# }
