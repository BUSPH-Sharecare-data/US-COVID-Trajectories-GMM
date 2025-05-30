########################################################################################
# PROJECT         :	COVID trajectory category analysis
# PROGRAM NAME    : COVID_trajectory_analysis_for_Publish.R
# DESCRIPTION     : Generated tables and figures
# NOTE            : 
#                 : "out_path", "traj_path" need to be updated
########################################################################################
 

########################################################
########################################################
##                                                    ##
##                                                    ##
##     generate tables and figures for the paper      ##
##                                                    ##
##                                                    ##
########################################################
########################################################

rm(list=ls())

## Return plot legend 

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


### Prepare workspace #### 

library(readxl)
library(dplyr)
library(ggplot2)
library(urbnmapr)
library(gridExtra)
library(stargazer)
library(nnet)
library(ggpubr)
library(plotrix)


# load saved data file

traj_path=""

load(paste0(traj_path,"dat.RData"))

names(dat)

# output files

out_path=""

#############################################################################################################
#### Table 1: Descriptive statistics for geographic classifications and county-level measures (N=3,142)  ####
#############################################################################################################

## Geography

dat$indicator <- 1

# check that we have no missing u/r values, population
dim(dat)   # 3142*21
length(which(is.na(dat$ur_group)))
length(which(is.na(dat$population)))

ur_all <- dat %>% dplyr::group_by(ur_group) %>% dplyr::summarise(n_county = sum(indicator),
                                                                 pct_county = round(((sum(indicator)/3142) * 100), 1))

pop_tot <- sum(dat$Population) # 328239523

ur_population <- dat %>% dplyr::group_by(ur_group) %>% dplyr::summarise(n_population = sum(Population),
                                                                        pct_population = round(((sum(Population)/pop_tot) * 100), 1))

ur_all$Division <- "U.S."
ur_all <- ur_all[,c(4,1:3)]


ur_region_all <- dat %>% dplyr::group_by(Division) %>% dplyr::summarise(n_county = sum(indicator))
ur_region_all$n <- rep(3142, 9)
ur_region_all$pct_county <- round((ur_region_all$n_county/ur_region_all$n) * 100, 1)
ur_region_all$ur_group <- "Both"
ur_region_all <- ur_region_all[,c("Division","ur_group","n_county","pct_county")]

ur_region <- dat %>% dplyr::group_by(Division, ur_group) %>% dplyr::summarise(n_county = sum(indicator))
ur_region$n <- as.numeric(c(rep(ur_all$n_county,9)))
ur_region$pct_county <- round((ur_region$n_county/ur_region$n) * 100, 1)
ur_region <- ur_region[,-4]


ur_reg_all <- dat %>% dplyr::group_by(Region) %>% dplyr::summarise(n_county = sum(indicator))
ur_reg_all$n <- rep(3142, 4)
ur_reg_all$pct_county <- round((ur_reg_all$n_county/ur_reg_all$n) * 100, 1)
ur_reg_all$ur_group <- "Both"
ur_reg_all <- ur_reg_all[,c("Region","ur_group","n_county","pct_county")]

ur_reg <- dat %>% dplyr::group_by(Region, ur_group) %>% dplyr::summarise(n_county = sum(indicator))
ur_reg$n <- as.numeric(c(rep(ur_all$n_county,4)))
ur_reg$pct_county <- round((ur_reg$n_county/ur_reg$n) * 100, 1)
ur_reg <- ur_reg[,-4]

tab1_part1 <- rbind(ur_region_all, ur_region)

tab1_part1_format <- tab1_part1
tab1_part1_format$county <- paste0(tab1_part1_format$n_county, " (", tab1_part1_format$pct_county, ")")

tab1_part1_format <- tab1_part1_format[,c("ur_group","Division","county")]



tab1_part2 <- rbind(ur_reg_all, ur_reg)

write.csv(tab1_part1_format, paste0(out_path,"Tab1_part1.csv"), row.names = TRUE)
write.csv(tab1_part2, paste0(out_path,"Tab1_part2.csv"), row.names = TRUE)


## Demographics  

tab2_all <- dat %>% summarize(mean_pct_black = round(mean(pct_black), 1),
                              sd_pct_black = round(sd(pct_black), 1),
                              median_pct_black = round(median(pct_black), 1),
                              min_pct_black = round(min(pct_black), 1),
                              max_pct_black = round(max(pct_black), 1),
                              
                              mean_pct_hisp = round(mean(pct_hisp), 1),
                              sd_pct_hisp = round(sd(pct_hisp), 1),
                              median_pct_hisp = round(median(pct_hisp), 1),
                              min_pct_hisp = round(min(pct_hisp), 1),
                              max_pct_hisp = round(max(pct_hisp), 1),
                              
                              mean_median_age = round(mean(median_age), 1),
                              sd_median_age = round(sd(median_age), 1),
                              median_median_age = round(median(median_age), 1),
                              min_median_age = round(min(median_age), 1),
                              max_median_age = round(max(median_age), 1),
                              
                              # one county missing income
                              mean_median_household_income = round(mean(median_household_income,na.rm=TRUE), 0),
                              sd_median_household_income = round(sd(median_household_income,na.rm=TRUE), 0),
                              median_median_household_income = round(median(median_household_income,na.rm=TRUE), 0),
                              min_median_household_income = round(min(median_household_income,na.rm=TRUE), 0),
                              max_median_household_income = round(max(median_household_income,na.rm=TRUE), 0),
                              
                              mean_sdoh2 = round(mean(sdoh2), 1),
                              sd_sdoh2 = round(sd(sdoh2), 1),
                              median_sdoh2 = round(median(sdoh2), 1),
                              min_sdoh2 = round(min(sdoh2), 1),
                              max_sdoh2 = round(max(sdoh2), 1))



tab2_ur <- dat %>% group_by(ur_group) %>%  summarize(mean_pct_black = round(mean(pct_black), 1),
                                                     sd_pct_black = round(sd(pct_black), 1),
                                                     median_pct_black = round(median(pct_black), 1),
                                                     min_pct_black = round(min(pct_black), 1),
                                                     max_pct_black = round(max(pct_black), 1),
                                                     
                                                     mean_pct_hisp = round(mean(pct_hisp), 1),
                                                     sd_pct_hisp = round(sd(pct_hisp), 1),
                                                     median_pct_hisp = round(median(pct_hisp), 1),
                                                     min_pct_hisp = round(min(pct_hisp), 1),
                                                     max_pct_hisp = round(max(pct_hisp), 1),
                                                     
                                                     mean_median_age = round(mean(median_age), 1),
                                                     sd_median_age = round(sd(median_age), 1),
                                                     median_median_age = round(median(median_age), 1),
                                                     min_median_age = round(min(median_age), 1),
                                                     max_median_age = round(max(median_age), 1),
                                                     
                                                     mean_median_household_income = round(mean(median_household_income,na.rm=TRUE), 0),
                                                     sd_median_household_income = round(sd(median_household_income,na.rm=TRUE), 0),
                                                     median_median_household_income = round(median(median_household_income,na.rm=TRUE), 0),
                                                     min_median_household_income = round(min(median_household_income,na.rm=TRUE), 0),
                                                     max_median_household_income = round(max(median_household_income,na.rm=TRUE), 0),
                                                     
                                                     mean_sdoh2 = round(mean(sdoh2), 1),
                                                     sd_sdoh2 = round(sd(sdoh2), 1),
                                                     median_sdoh2 = round(median(sdoh2), 1),
                                                     min_sdoh2 = round(min(sdoh2), 1),
                                                     max_sdoh2 = round(max(sdoh2), 1))


tab2_format <- data.frame(total = rep(NA, 10),
                          urban = rep(NA, 10),
                          rural = rep(NA, 10))


tab2_format[1,1] <- paste0(tab2_all$mean_pct_black, " (", tab2_all$sd_pct_black, ")")
tab2_format[2,1] <- paste0(tab2_all$median_pct_black, " [", tab2_all$min_pct_black, ":", tab2_all$max_pct_black, "]")

tab2_format[3,1] <- paste0(tab2_all$mean_pct_hisp, " (", tab2_all$sd_pct_hisp, ")")
tab2_format[4,1] <- paste0(tab2_all$median_pct_hisp, " [", tab2_all$min_pct_hisp, ":", tab2_all$max_pct_hisp, "]")

tab2_format[5,1] <- paste0(tab2_all$mean_median_age, " (", tab2_all$sd_median_age, ")")
tab2_format[6,1] <- paste0(tab2_all$median_median_age, " [", tab2_all$min_median_age, ":", tab2_all$max_median_age, "]")

tab2_format[7,1] <- paste0(tab2_all$mean_median_household_income, " (", tab2_all$sd_median_household_income, ")")
tab2_format[8,1] <- paste0(tab2_all$median_median_household_income, " [", tab2_all$min_median_household_income, ":", tab2_all$max_median_household_income, "]")

tab2_format[9,1] <- paste0(tab2_all$mean_sdoh2, " (", tab2_all$sd_sdoh2, ")")
tab2_format[10,1] <- paste0(tab2_all$median_sdoh2, " [", tab2_all$min_sdoh2, ":", tab2_all$max_sdoh2, "]")


tab2_urban <- tab2_ur[which(tab2_ur$ur_group == "urban"),]
tab2_format[1,2] <- paste0(tab2_urban$mean_pct_black, " (", tab2_urban$sd_pct_black, ")")
tab2_format[2,2] <- paste0(tab2_urban$median_pct_black, " [", tab2_urban$min_pct_black, ":", tab2_urban$max_pct_black, "]")

tab2_format[3,2] <- paste0(tab2_urban$mean_pct_hisp, " (", tab2_urban$sd_pct_hisp, ")")
tab2_format[4,2] <- paste0(tab2_urban$median_pct_hisp, " [", tab2_urban$min_pct_hisp, ":", tab2_urban$max_pct_hisp, "]")

tab2_format[5,2] <- paste0(tab2_urban$mean_median_age, " (", tab2_urban$sd_median_age, ")")
tab2_format[6,2] <- paste0(tab2_urban$median_median_age, " [", tab2_urban$min_median_age, ":", tab2_urban$max_median_age, "]")

tab2_format[7,2] <- paste0(tab2_urban$mean_median_household_income, " (", tab2_urban$sd_median_household_income, ")")
tab2_format[8,2] <- paste0(tab2_urban$median_median_household_income, " [", tab2_urban$min_median_household_income, ":", tab2_urban$max_median_household_income, "]")

tab2_format[9,2] <- paste0(tab2_urban$mean_sdoh2, " (", tab2_urban$sd_sdoh2, ")")
tab2_format[10,2] <- paste0(tab2_urban$median_sdoh2, " [", tab2_urban$min_sdoh2, ":", tab2_urban$max_sdoh2, "]")


tab2_rural <- tab2_ur[which(tab2_ur$ur_group == "rural"),]
tab2_format[1,3] <- paste0(tab2_rural$mean_pct_black, " (", tab2_rural$sd_pct_black, ")")
tab2_format[2,3] <- paste0(tab2_rural$median_pct_black, " [", tab2_rural$min_pct_black, ":", tab2_rural$max_pct_black, "]")

tab2_format[3,3] <- paste0(tab2_rural$mean_pct_hisp, " (", tab2_rural$sd_pct_hisp, ")")
tab2_format[4,3] <- paste0(tab2_rural$median_pct_hisp, " [", tab2_rural$min_pct_hisp, ":", tab2_rural$max_pct_hisp, "]")

tab2_format[5,3] <- paste0(tab2_rural$mean_median_age, " (", tab2_rural$sd_median_age, ")")
tab2_format[6,3] <- paste0(tab2_rural$median_median_age, " [", tab2_rural$min_median_age, ":", tab2_rural$max_median_age, "]")

tab2_format[7,3] <- paste0(tab2_rural$mean_median_household_income, " (", tab2_rural$sd_median_household_income, ")")
tab2_format[8,3] <- paste0(tab2_rural$median_median_household_income, " [", tab2_rural$min_median_household_income, ":", tab2_rural$max_median_household_income, "]")

tab2_format[9,3] <- paste0(tab2_rural$mean_sdoh2, " (", tab2_rural$sd_sdoh2, ")")
tab2_format[10,3] <- paste0(tab2_rural$median_sdoh2, " [", tab2_rural$min_sdoh2, ":", tab2_rural$max_sdoh2, "]")


rownames(tab2_format) <-c("Percent Black or African American mean(SD) ",
                          "Percent Black or African American median(min:max)",
                          "Percent Hispanic or Latino mean(SD)",
                          "Percent Hispanic or Latino median(min:max)",
                          "Age  mean(SD)",
                          "Age median(min:max)",
                          "Household Income (US Dollars)  mean(SD)",
                          "Household Income (US Dollars) median(min:max)",
                          "Social Determinants of Health Index (SDOH)  mean(SD)",
                          "Social Determinants of Health Index (SDOH) median(min:max)") 

write.csv(tab2_format, paste0(out_path,"Tab1_part3.csv"), row.names = TRUE)

## SDOH 

tab3a <- table(dat$sdoh2_quant)
tab3b <- table(dat$sdoh2_quant, dat$ur_group)

tab3_format <- data.frame(total = rep(NA, 4),
                          urban = rep(NA, 4),
                          rural = rep(NA, 4))

tab3_format[1,1] <- paste0(tab3a[1], " (", round((tab3a[1]/nrow(dat)) * 100, 1), ")")
tab3_format[2,1] <- paste0(tab3a[2], " (", round((tab3a[2]/nrow(dat)) * 100, 1), ")")
tab3_format[3,1] <- paste0(tab3a[3], " (", round((tab3a[3]/nrow(dat)) * 100, 1), ")")
tab3_format[4,1] <- paste0(tab3a[4], " (", round((tab3a[4]/nrow(dat)) * 100, 1), ")")


tab3_format[1,2] <- paste0(tab3b[1,2], " (", round((tab3b[1,2]/nrow(dat[which(dat$ur_group == "urban"),])) * 100, 1), ")")
tab3_format[2,2] <- paste0(tab3b[2,2], " (", round((tab3b[2,2]/nrow(dat[which(dat$ur_group == "urban"),])) * 100, 1), ")")
tab3_format[3,2] <- paste0(tab3b[3,2], " (", round((tab3b[3,2]/nrow(dat[which(dat$ur_group == "urban"),])) * 100, 1), ")")
tab3_format[4,2] <- paste0(tab3b[4,2], " (", round((tab3b[4,2]/nrow(dat[which(dat$ur_group == "urban"),])) * 100, 1), ")")


tab3_format[1,3] <- paste0(tab3b[1,1], " (", round((tab3b[1,1]/nrow(dat[which(dat$ur_group == "rural"),])) * 100, 1), ")")
tab3_format[2,3] <- paste0(tab3b[2,1], " (", round((tab3b[2,1]/nrow(dat[which(dat$ur_group == "rural"),])) * 100, 1), ")")
tab3_format[3,3] <- paste0(tab3b[3,1], " (", round((tab3b[3,1]/nrow(dat[which(dat$ur_group == "rural"),])) * 100, 1), ")")
tab3_format[4,3] <- paste0(tab3b[4,1], " (", round((tab3b[4,1]/nrow(dat[which(dat$ur_group == "rural"),])) * 100, 1), ")")

rownames(tab3_format) <-c("SDOH <10th ",
                          "SDOH >=10th to <50th",
                          "SDOH >=50th to <90th",
                          "SDOH >=90th")

write.csv(tab3_format, paste0(out_path,"Tab1_part5.csv"), row.names = TRUE)

## Table 4 - COVID rates updated 4/8/2025, direct the numbers from the analytic data



tab4_format <- data.frame(total = rep(NA, 2),
                          period1 = rep(NA, 2),
                          period2 = rep(NA, 2),
                          period3 = rep(NA, 2))


tab4_format[1,1] <- paste0(round(mean(dat$IncidenceRate, na.rm = TRUE), 1), " (", round(sd(dat$IncidenceRate, na.rm = TRUE), 1), ")")
tab4_format[2,1] <- paste0(round(median(dat$IncidenceRate, na.rm = TRUE), 1), " [", round(min(dat$IncidenceRate, na.rm = TRUE), 1), ":", round(max(dat$IncidenceRate, na.rm = TRUE), 1), "]")

tab4_format[1,2] <- paste0(round(mean(dat$newcases_p1, na.rm = TRUE), 1), " (", round(sd(dat$newcases_p1, na.rm = TRUE), 1), ")")
tab4_format[2,2] <- paste0(round(median(dat$newcases_p1, na.rm = TRUE), 1), " [", round(min(dat$newcases_p1, na.rm = TRUE), 1),  ":", round(max(dat$newcases_p1, na.rm = TRUE), 1), "]")

tab4_format[1,3] <- paste0(round(mean(dat$newcases_p2, na.rm = TRUE), 1), " (", round(sd(dat$newcases_p2, na.rm = TRUE), 1), ")")
tab4_format[2,3] <- paste0(round(median(dat$newcases_p2, na.rm = TRUE), 1), " [", round(min(dat$newcases_p2, na.rm = TRUE), 1), ":", round(max(dat$newcases_p2, na.rm = TRUE), 1), "]")

tab4_format[1,4] <- paste0(round(mean(dat$newcases_p3, na.rm = TRUE), 1), " (", round(sd(dat$newcases_p3, na.rm = TRUE), 1), ")")
tab4_format[2,4] <- paste0(round(median(dat$newcases_p3, na.rm = TRUE), 1), " [", round(min(dat$newcases_p3, na.rm = TRUE), 1),  ":", round(max(dat$newcases_p3, na.rm = TRUE), 1), "]")


rownames(tab4_format) <-c("mean(sd)","median(min:max)")

write.csv(tab4_format, paste0(out_path,"Tab1_part4.csv"), row.names = TRUE)

#############################################################################################################
####  Table 2:  Characteristics of Trajectory Classes by Period                                          ####
#############################################################################################################

#### Table 2: Period 1 ####

dat$county_count <- 1

counties <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(county_count = sum(county_count))
counties$counties_percent <- round(counties$county_count/sum(dat$county_count)*100, 1)

pops <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(pop = sum(Population))
pops$pops_pct <- round(pops$pop/sum(dat$Population)*100, 1)

rural <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(rural_count = length(which(ur_group == "rural")),
                                                                 urban_count = length(which(ur_group == "urban")),
                                                                 pct_rural = round(prop.table(table(ur_group))[1], 4))
rural$rural_pct <- round(rural$pct_rural*100, 1)
rural$rural_urban <- paste0(rural$rural_count, "/", rural$urban_count)

counties_all_p1 <- cbind(counties, pops[,"pops_pct"], rural[,c("rural_urban", "rural_pct")])

write.csv(counties_all_p1, paste0(out_path,"Tab2_p1.csv"), row.names = TRUE)

#### Table 2: Period 2 ####


counties <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(county_count = sum(county_count))
counties$counties_percent <- round(counties$county_count/sum(dat$county_count)*100, 1)

pops <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(pop = sum(Population))
pops$pops_pct <- round(pops$pop/sum(dat$Population)*100, 1)

rural <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(rural_count = length(which(ur_group == "rural")),
                                                                 urban_count = length(which(ur_group == "urban")),
                                                                 pct_rural = round(prop.table(table(ur_group))[1], 4))
rural$rural_pct <- round(rural$pct_rural*100, 1)
rural$rural_urban <- paste0(rural$rural_count, "/", rural$urban_count)

counties_all_p2 <- cbind(counties, pops[,"pops_pct"], rural[,c("rural_urban", "rural_pct")])

write.csv(counties_all_p2, paste0(out_path,"Tab2_p2.csv"), row.names = TRUE)

#### Table 2: Period 3 ####


counties <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(county_count = sum(county_count))
counties$counties_percent <- round(counties$county_count/sum(dat$county_count)*100, 1)

pops <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(pop = sum(Population))
pops$pops_pct <- round(pops$pop/sum(dat$Population)*100, 1)

rural <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(rural_count = length(which(ur_group == "rural")),
                                                                 urban_count = length(which(ur_group == "urban")),
                                                                 pct_rural = round(prop.table(table(ur_group))[1], 4))
rural$rural_pct <- round(rural$pct_rural*100, 1)
rural$rural_urban <- paste0(rural$rural_count, "/", rural$urban_count)

counties_all_p3 <- cbind(counties, pops[,"pops_pct"], rural[,c("rural_urban", "rural_pct")])

write.csv(counties_all_p3, paste0(out_path,"Tab2_p3.csv"), row.names = TRUE)

#############################################################################################################
#### FIGURE 1: SDOHI MAP                                                                           ##########
#############################################################################################################


colors <- c("#eff3ff",
            "#bdd7e7",
            "#6baed6",
            "#2171b5")

mapr_counties <- urbnmapr::counties  # shapefiles
mapr_counties <- plyr::rename(mapr_counties, c("county_fips" = "FIPS_CODE")) #ensure ID var name matches

map_data <- left_join(dat, mapr_counties, by = "FIPS_CODE") 
map_data$sdoh2_quant_rename <- as.character(map_data$sdoh2_quant)
map_data$sdoh2_quant_rename[which(map_data$sdoh2_quant == 1)] <- "<10th"
map_data$sdoh2_quant_rename[which(map_data$sdoh2_quant == 2)] <- "≥10th to <50th"
map_data$sdoh2_quant_rename[which(map_data$sdoh2_quant == 3)] <- "≥50th to <90th"
map_data$sdoh2_quant_rename[which(map_data$sdoh2_quant == 4)] <- "≥90th"


sdohi <- map_data %>%
  ggplot(aes(long, lat, group = group, fill = as.factor(sdoh2_quant_rename))) +
  geom_polygon(color = NA) +
  borders("state", colour = "Black") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "SDOH Group Percentile")+
  scale_fill_manual(values = colors)+
  xlab("")+
  ylab("") + 
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank())

png(paste0(out_path,"SDOHi_map.png"), units="in", width=8, height=6, res=600)
sdohi
dev.off()


#############################################################################################################
#### FIGURE 2: Trajectories line plots and maps                                                         #####
#############################################################################################################

traj_path

##### Figure 2: Line plots #####


colors <- c("#7fc97f",
            "#06402B",
            "#ffff99",
            "#fdc086",
            "#beaed4")

pred_m5_p1=readRDS(paste0(traj_path,"/t1/pred_m5_t1.rds"))


x=seq(0,13,by=1)
#y=seq(0,max((pred_m3$pred))+100,length.out=14)
y=seq(0,500,length.out=14)

pred_m5_p1_melt <- reshape2::melt(pred_m5_p1$pred)

pred_m5_p1_melt$Var2 <- as.character(pred_m5_p1_melt$Var2)
pred_m5_p1_melt$Var2_orig <- as.character(pred_m5_p1_melt$Var2)
pred_m5_p1_melt$Class <- as.character(pred_m5_p1_melt$Var2)

# 1->1, 4->2, 2->3, 5->4, 3->5 % from TableS3
pred_m5_p1_melt$Var2[which(pred_m5_p1_melt$Var2_orig == "Ypred_class1")] <- "Ypred_class1"
pred_m5_p1_melt$Var2[which(pred_m5_p1_melt$Var2_orig == "Ypred_class4")] <- "Ypred_class2"
pred_m5_p1_melt$Var2[which(pred_m5_p1_melt$Var2_orig == "Ypred_class2")] <- "Ypred_class3"
pred_m5_p1_melt$Var2[which(pred_m5_p1_melt$Var2_orig == "Ypred_class5")] <- "Ypred_class4"
pred_m5_p1_melt$Var2[which(pred_m5_p1_melt$Var2_orig == "Ypred_class3")] <- "Ypred_class5"

pred_m5_p1_melt$Class[which(pred_m5_p1_melt$Var2 == "Ypred_class1")] <- "Class 1 (18.6%)"
pred_m5_p1_melt$Class[which(pred_m5_p1_melt$Var2 == "Ypred_class2")] <- "Class 2 (43.3%)"
pred_m5_p1_melt$Class[which(pred_m5_p1_melt$Var2 == "Ypred_class3")] <- "Class 3 ( 8.9%)"
pred_m5_p1_melt$Class[which(pred_m5_p1_melt$Var2 == "Ypred_class4")] <- "Class 4 (22.0%)"
pred_m5_p1_melt$Class[which(pred_m5_p1_melt$Var2 == "Ypred_class5")] <- "Class 5 ( 7.2%)"



p1_line_plot <- ggplot(pred_m5_p1_melt, aes(Var1, value, fill = Class, color = Class)) +   geom_line(cex = 1.2) + 
  ylim(0, 500) + 
  scale_x_continuous(name = "Weeks",
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
                     labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)) +
  scale_color_manual(values = colors[c(1,2,3,4,5)]) + 
  ylab("Incidence Rate") +
  xlab("Week") + 
  # ggtitle("Predicted Weekly Incidence Rate \n (March 15 to June 14)") +
  theme_bw() + 
  theme(legend.position = c(0.20, 0.50),panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## Period 2

colors <- c("#06402B",
            "#ffff99",
            "#fdc086",
            "#beaed4")


pred_m4_p2=readRDS(paste0(traj_path,"t2/pred_m4_t2.rds"))


x=seq(0,8,by=1)
#y=seq(0,max((pred_m4$pred))+100,length.out=9)
y=seq(0,500,length.out=9)


pred_m4_p2_melt <- reshape2::melt(pred_m4_p2$pred)

pred_m4_p2_melt$Var2_save <- as.character(pred_m4_p2_melt$Var2)
pred_m4_p2_melt$Var2 <- as.character(pred_m4_p2_melt$Var2)
pred_m4_p2_melt$Class <- as.character(pred_m4_p2_melt$Var2)

# 2->1 3->2 1->3 4->4, % from TableS3

pred_m4_p2_melt$Var2[which(pred_m4_p2_melt$Var2_save == "Ypred_class2")] <- "Ypred_class1"
pred_m4_p2_melt$Var2[which(pred_m4_p2_melt$Var2_save == "Ypred_class3")] <- "Ypred_class2"
pred_m4_p2_melt$Var2[which(pred_m4_p2_melt$Var2_save == "Ypred_class1")] <- "Ypred_class3"
pred_m4_p2_melt$Var2[which(pred_m4_p2_melt$Var2_save == "Ypred_class4")] <- "Ypred_class4"

pred_m4_p2_melt$Class[which(pred_m4_p2_melt$Var2 == "Ypred_class1")] <- "Class 1/2 (15.3%)" 
pred_m4_p2_melt$Class[which(pred_m4_p2_melt$Var2 == "Ypred_class2")] <- "Class 3   ( 8.2%)"
pred_m4_p2_melt$Class[which(pred_m4_p2_melt$Var2 == "Ypred_class3")] <- "Class 4   (40.2%)"
pred_m4_p2_melt$Class[which(pred_m4_p2_melt$Var2 == "Ypred_class4")] <- "Class 5   (36.3%)"



p2_line_plot <- ggplot(pred_m4_p2_melt, aes(Var1, value, fill = Class, color = Class)) +   geom_line(cex = 1.2) + 
  ylim(0, 500) + 
  scale_x_continuous(name = "Weeks",
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
                     labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)) +
  scale_color_manual(values = colors) + 
  ylab("Incidence Rate") +
  xlab("Week") + 
  #ggtitle("Predicted Weekly Incidence Rate \n (June 15 to August 14)") +
  theme_bw() + 
  theme(legend.position = c(0.20, 0.50),panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## Period 3

colors <- c("#06402B",
            "#ffff99",
            "#fdc086",
            "#beaed4")

x=seq(0,12,by=1)
#y=seq(0,max((pred_m4$pred))+100,length.out=14)
y=seq(0,500,length.out=14)

pred_m4_p3=readRDS(paste0(traj_path,"t3/pred_m4_t3.rds"))
pred_m4_p3$pred=pred_m4_p3$pred[,c(1:4)]

pred_m4_p3_melt <- reshape2::melt(pred_m4_p3$pred)

pred_m4_p3_melt$Var2_save <- as.character(pred_m4_p3_melt$Var2)
pred_m4_p3_melt$Var2 <- as.character(pred_m4_p3_melt$Var2)
pred_m4_p3_melt$Class <- as.character(pred_m4_p3_melt$Var2)

# 1->1, 3->2, 4->3, 2->4 % from TableS3

pred_m4_p3_melt$Var2[which(pred_m4_p3_melt$Var2_save == "Ypred_class1")] <- "Ypred_class1"
pred_m4_p3_melt$Var2[which(pred_m4_p3_melt$Var2_save == "Ypred_class3")] <- "Ypred_class2"
pred_m4_p3_melt$Var2[which(pred_m4_p3_melt$Var2_save == "Ypred_class4")] <- "Ypred_class3"
pred_m4_p3_melt$Var2[which(pred_m4_p3_melt$Var2_save == "Ypred_class2")] <- "Ypred_class4"


pred_m4_p3_melt$Class[which(pred_m4_p3_melt$Var2 == "Ypred_class1")] <- "Class 1/2 ( 4.5%)" 
pred_m4_p3_melt$Class[which(pred_m4_p3_melt$Var2 == "Ypred_class2")] <- "Class 3   ( 6.3%)"
pred_m4_p3_melt$Class[which(pred_m4_p3_melt$Var2 == "Ypred_class3")] <- "Class 4   (76.0%)"
pred_m4_p3_melt$Class[which(pred_m4_p3_melt$Var2 == "Ypred_class4")] <- "Class 5   (13.2%)"


p3_line_plot <- ggplot(pred_m4_p3_melt, aes(Var1, value, fill = Class, color = Class)) +   geom_line(cex = 1.2) + 
  ylim(0, 500) + 
  scale_x_continuous(name = "Weeks",
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
                     labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)) +
  scale_color_manual(values = colors) + 
  ylab("Incidence Rate") +
  xlab("Week") + 
  #ggtitle("Predicted Weekly Incidence Rate \n (August 15 to Novemer 15)") +
  theme_bw() + 
  theme(legend.position = c(0.20, 0.60),panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


##### Figure 2: Maps ####


mapr_counties <- urbnmapr::counties  # shapefiles
mapr_counties <- plyr::rename(mapr_counties, c("county_fips" = "FIPS_CODE")) #ensure ID var name matches

map_data <- left_join(dat, mapr_counties, by = "FIPS_CODE") 



colors <- c("#7fc97f",
            "#06402B",
            "#ffff99",
            "#fdc086",
            "#beaed4")


grp3_map_p1 <- map_data %>%
  ggplot(aes(long, lat, group = group, fill = as.factor(p1_class5))) +
  geom_polygon(color = NA) +
  borders("state", colour = "Black") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Trajectory Class")+
  scale_fill_manual(values = colors[c(1,2,3,4,5)])+
  #ggtitle("Geographic distribution: \n Period 1: March 15th to June 14th 2020") +
  theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank())


grp4_map_p2 <- map_data %>%
  ggplot(aes(long, lat, group = group, fill = as.factor(p2_class4))) +
  geom_polygon(color = NA) +
  borders("state", colour = "Black") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Trajectory Class")+
  scale_fill_manual(values = colors[c(2,3,4,5)])+
  #ggtitle("Geographic distribution: \n Period 2: June 15th to August 14th 2020") +
  theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank())



grp4_map_p3 <- map_data %>%
  ggplot(aes(long, lat, group = group, fill = as.factor(p3_class4))) +
  geom_polygon(color = NA) +
  borders("state", colour = "Black") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Trajectory Class")+
  scale_fill_manual(values = colors[c(2,3,4,5)])+
  #ggtitle("Geographic distribution: \n Period 3: August 15th to November 15th 2020") +
  theme_minimal() + 
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank()) 



##### Figure 2: Combined plot ####


png(paste0(out_path,"/map_trajectory_multiplot.png"), units="in", width=10, height=12, res=600)
ggarrange(p1_line_plot, 
          grp3_map_p1,
          p2_line_plot, 
          grp4_map_p2, 
          p3_line_plot, 
          grp4_map_p3,
          labels = c("A. Mar. 15 - Jun. 14", "B. Mar. 15 - Jun. 14", "C. Jun. 15 - Aug. 14", "D. Jun. 15 - Aug. 14", "E. Aug. 15 - Nov. 2", "F. Aug. 15 - Nov. 2"),
          ncol = 2, nrow = 3)
dev.off()





#######################################################################################################################
#### Table S5. County Geographic Distribution and Census Division by Trajectory Class and Time Period (N=3,142)   #####
#######################################################################################################################

count_p1 <- table(dat$Division,dat$p1_class5)
pct_p1 <- round(prop.table(table(dat$Division,dat$p1_class5),margin=1)*100,1)
com_p1 <- matrix(paste(count_p1, "(", pct_p1, ")", sep=""),nrow=9)
rownames(com_p1) <- rownames(count_p1)
colnames(com_p1) <- c("Class 1","Class 2","Class 3","Class 4","Class 5")

count_p2 <- table(dat$Division,dat$p2_class4)
pct_p2 <- round(prop.table(table(dat$Division,dat$p2_class4),margin=1)*100,1)
com_p2 <- matrix(paste(count_p2, "(", pct_p2, ")", sep=""),nrow=9)
rownames(com_p2) <- rownames(count_p2)
colnames(com_p2) <- c("Class 1","Class 2","Class 3","Class 4")

count_p3 <- table(dat$Division,dat$p3_class4)
pct_p3 <- round(prop.table(table(dat$Division,dat$p3_class4),margin=1)*100,1)
com_p3 <- matrix(paste(count_p3, "(", pct_p3, ")", sep=""),nrow=9)
rownames(com_p3) <- rownames(count_p3)
colnames(com_p3) <- c("Class 1","Class 2","Class 3","Class 4")


write.csv(com_p1, paste0(out_path,"Table_S5_p1.csv"), row.names = TRUE)
write.csv(com_p2, paste0(out_path,"Table_S5_p2.csv"), row.names = TRUE)
write.csv(com_p3, paste0(out_path,"Table_S5_p3.csv"), row.names = TRUE)



#############################################################################################################################
#### Table S6. Demographic Characteristics by Trajectory Class and Time Period, Overall and by SDOH Group (N=3,142)  ########
#############################################################################################################################


#####Table S6: Period 1 ######## 

## Overall 
pct_black <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(pct_black = mean(pct_black, na.rm =TRUE))
pct_black_sd <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(pct_black = sd(pct_black, na.rm =TRUE))

pct_hisp <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(pct_hisp = mean(pct_hisp, na.rm =TRUE))
pct_hisp_sd <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(pct_hisp = sd(pct_hisp, na.rm =TRUE))

med_age <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(med_age = mean(median_age, na.rm =TRUE))
med_age_sd <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(med_age = sd(median_age, na.rm =TRUE))

med_income <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(med_income = mean(median_household_income, na.rm =TRUE))
med_income_sd <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(med_income = sd(median_household_income, na.rm =TRUE))

tab1 <- rbind(t(pct_black),
              t(pct_hisp),
              t(med_age),
              t(med_income)
              )

tab2 <- rbind(t(pct_black_sd),
              t(pct_hisp_sd),
              t(med_age_sd),
              t(med_income_sd)
              )


tab_final_p1 <- as.data.frame(rbind(tab1[2,], 
                                    tab2[2,],
                                    tab1[4,], 
                                    tab2[4,],
                                    tab1[6,], 
                                    tab2[6,],
                                    tab1[8,], 
                                    tab2[8,]))


tab_final_p1$measure <- rep(c("Mean","SD"), 4)
tab_final_p1$var <- c("Percent Black","Percent Black", 
                      "Percent Hispanic", "Percent Hispanic", 
                      "Median Age", "Median Age",
                      "Median Income", "Median Income")

tab_final_p1 <- tab_final_p1[,c(7,6,1:5)]


## SDOH stratified

## pct black
r1_all <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(pct = mean(pct_black, na.rm =TRUE),
                                                                  sd = sd(pct_black, na.rm =TRUE))
names(r1_all)[c(2:3)] <- c("pct_black","sd_black")

r1_all_reorg <- cbind(r1_all[which(r1_all$p1_class5 == 1),], 
                      r1_all[which(r1_all$p1_class5 == 2),],
                      r1_all[which(r1_all$p1_class5 == 3),],
                      r1_all[which(r1_all$p1_class5 == 4),],
                      r1_all[which(r1_all$p1_class5 == 5),])
r1_all_reorg <- r1_all_reorg[,c(2,3,5,6,8,9,11,12,14,15)]
names(r1_all_reorg) <- c("mean_cat1","sd_cat1",
                         "mean_cat2","sd_cat2",
                         "mean_cat3","sd_cat3",
                         "mean_cat4","sd_cat4",
                         "mean_cat5","sd_cat5")


r1 <- dat %>% dplyr::group_by(p1_class5, sdoh2_quant) %>% dplyr::summarize(pct = mean(pct_black, na.rm =TRUE),
                                                                           sd = sd(pct_black, na.rm =TRUE))
names(r1)[c(3:4)] <- c("pct_black","sd_black")


r1_reorg <- cbind(r1[which(r1$p1_class5 == 1),], 
                  r1[which(r1$p1_class5 == 2),],
                  r1[which(r1$p1_class5 == 3),],
                  r1[which(r1$p1_class5 == 4),],
                  r1[which(r1$p1_class5 == 5),])
r1_reorg <- r1_reorg[,c(3,4,7,8,11,12,15,16,19,20)]
names(r1_reorg) <- c("mean_cat1","sd_cat1",
                     "mean_cat2","sd_cat2",
                     "mean_cat3","sd_cat3",
                     "mean_cat4","sd_cat4",
                     "mean_cat5","sd_cat5")


## percent hispanic/latino
r2_all <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(pct = mean(pct_hisp, na.rm =TRUE),
                                                                  sd = sd(pct_hisp, na.rm =TRUE))
names(r2_all)[c(2:3)] <- c("pct_hisp","sd_hisp")


r2_all_reorg <- cbind(r2_all[which(r2_all$p1_class5 == 1),], 
                      r2_all[which(r2_all$p1_class5 == 2),],
                      r2_all[which(r2_all$p1_class5 == 3),],
                      r2_all[which(r2_all$p1_class5 == 4),],
                      r2_all[which(r2_all$p1_class5 == 5),])
r2_all_reorg <- r2_all_reorg[,c(2,3,5,6,8,9,11,12,14,15)]
names(r2_all_reorg) <- c("mean_cat1","sd_cat1",
                         "mean_cat2","sd_cat2",
                         "mean_cat3","sd_cat3",
                         "mean_cat4","sd_cat4",
                         "mean_cat5","sd_cat5")


r2 <- dat %>% dplyr::group_by(p1_class5, sdoh2_quant) %>% dplyr::summarize(pct = mean(pct_hisp, na.rm =TRUE),
                                                                           sd = sd(pct_hisp, na.rm =TRUE))
names(r2)[c(3:4)] <- c("pct_hisp","sd_hisp")


r2_reorg <- cbind(r2[which(r2$p1_class5 == 1),], 
                  r2[which(r2$p1_class5 == 2),],
                  r2[which(r2$p1_class5 == 3),],
                  r2[which(r2$p1_class5 == 4),],
                  r2[which(r2$p1_class5 == 5),])
r2_reorg <- r2_reorg[,c(3,4,7,8,11,12,15,16,19,20)]
names(r2_reorg) <- c("mean_cat1","sd_cat1",
                     "mean_cat2","sd_cat2",
                     "mean_cat3","sd_cat3",
                     "mean_cat4","sd_cat4",
                     "mean_cat5","sd_cat5")



## median age
r3_all <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(pct = mean(median_age, na.rm =TRUE),
                                                                  sd = sd(median_age, na.rm =TRUE))
names(r3_all)[c(2:3)] <- c("pct_age","sd_age")


r3_all_reorg <- cbind(r3_all[which(r3_all$p1_class5 == 1),], 
                      r3_all[which(r3_all$p1_class5 == 2),],
                      r3_all[which(r3_all$p1_class5 == 3),],
                      r3_all[which(r3_all$p1_class5 == 4),],
                      r3_all[which(r3_all$p1_class5 == 5),])
r3_all_reorg <- r3_all_reorg[,c(2,3,5,6,8,9,11,12,14,15)]
names(r3_all_reorg) <- c("mean_cat1","sd_cat1",
                         "mean_cat2","sd_cat2",
                         "mean_cat3","sd_cat3",
                         "mean_cat4","sd_cat4",
                         "mean_cat5","sd_cat5")


r3 <- dat %>% dplyr::group_by(p1_class5, sdoh2_quant) %>% dplyr::summarize(pct = mean(median_age, na.rm =TRUE),
                                                                           sd = sd(median_age, na.rm =TRUE))
names(r3)[c(3:4)] <- c("pct_age","sd_age")

r3_reorg <- cbind(r3[which(r3$p1_class5 == 1),], 
                  r3[which(r3$p1_class5 == 2),],
                  r3[which(r3$p1_class5 == 3),],
                  r3[which(r3$p1_class5 == 4),],
                  r3[which(r3$p1_class5 == 5),])
r3_reorg <- r3_reorg[,c(3,4,7,8,11,12,15,16,19,20)]
names(r3_reorg) <- c("mean_cat1","sd_cat1",
                     "mean_cat2","sd_cat2",
                     "mean_cat3","sd_cat3",
                     "mean_cat4","sd_cat4",
                     "mean_cat5","sd_cat5")


### income
r4_all <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(pct = mean(median_household_income, na.rm =TRUE),
                                                                  sd = sd(median_household_income, na.rm =TRUE))
names(r4_all)[c(2:3)] <- c("pct_income","sd_income")


r4_all_reorg <- cbind(r4_all[which(r4_all$p1_class5 == 1),], 
                      r4_all[which(r4_all$p1_class5 == 2),],
                      r4_all[which(r4_all$p1_class5 == 3),],
                      r4_all[which(r4_all$p1_class5 == 4),],
                      r4_all[which(r4_all$p1_class5 == 5),])
r4_all_reorg <- r4_all_reorg[,c(2,3,5,6,8,9,11,12,14,15)]
names(r4_all_reorg) <- c("mean_cat1","sd_cat1",
                         "mean_cat2","sd_cat2",
                         "mean_cat3","sd_cat3",
                         "mean_cat4","sd_cat4",
                         "mean_cat5","sd_cat5")


r4 <- dat %>% dplyr::group_by(p1_class5, sdoh2_quant) %>% dplyr::summarize(pct = mean(median_household_income, na.rm =TRUE),
                                                                           sd = sd(median_household_income, na.rm =TRUE))
names(r4)[c(3:4)] <- c("pct_income","sd_income")


r4_reorg <- cbind(r4[which(r4$p1_class5 == 1),], 
                  r4[which(r4$p1_class5 == 2),],
                  r4[which(r4$p1_class5 == 3),],
                  r4[which(r4$p1_class5 == 4),],
                  r4[which(r4$p1_class5 == 5),])
r4_reorg <- r4_reorg[,c(3,4,7,8,11,12,15,16,19,20)]
names(r4_reorg) <- c("mean_cat1","sd_cat1",
                     "mean_cat2","sd_cat2",
                     "mean_cat3","sd_cat3",
                     "mean_cat4","sd_cat4",
                     "mean_cat5","sd_cat5")



summary <- rbind(r1_all_reorg, 
                 r1_reorg,
                 r2_all_reorg, 
                 r2_reorg,
                 r3_all_reorg, 
                 r3_reorg,
                 r4_all_reorg, 
                 r4_reorg)

summary$group <- rep(c("All", "SDOH Q1", "SDOH Q2", "SDOH Q3", "SDOH Q4"), 4)
summary$var <- c(rep("Pct. Black", 5), 
                 rep("Pct Hisp.", 5),
                 rep("Median age", 5),
                 rep("Median income", 5))


summary_final_p1 <- summary[,c(11,12,1:10)]



##### Table S6: Period 2 ######## 


## Overall

pct_black <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(pct_black = mean(pct_black, na.rm =TRUE))
pct_black_sd <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(pct_black = sd(pct_black, na.rm =TRUE))

pct_hisp <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(pct_hisp = mean(pct_hisp, na.rm =TRUE))
pct_hisp_sd <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(pct_hisp = sd(pct_hisp, na.rm =TRUE))

med_age <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(med_age = mean(median_age, na.rm =TRUE))
med_age_sd <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(med_age = sd(median_age, na.rm =TRUE))

med_income <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(med_income = mean(median_household_income, na.rm =TRUE))
med_income_sd <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(med_income = sd(median_household_income, na.rm =TRUE))

tab1 <- rbind(t(pct_black),
              t(pct_hisp),
              t(med_age),
              t(med_income))

tab2 <- rbind(t(pct_black_sd),
              t(pct_hisp_sd),
              t(med_age_sd),
              t(med_income_sd))


tab_final_p2 <- as.data.frame(rbind(tab1[2,], 
                                    tab2[2,],
                                    tab1[4,], 
                                    tab2[4,],
                                    tab1[6,], 
                                    tab2[6,],
                                    tab1[8,], 
                                    tab2[8,]))


tab_final_p2$measure <- rep(c("Mean","SD"), 4)
tab_final_p2$var <- c("Percent Black","Percent Black", 
                      "Percent Hispanic", "Percent Hispanic", 
                      "Median Age", "Median Age",
                      "Median Income", "Median Income")

tab_final_p2 <- tab_final_p2[,c(6,5,1:4)]


## SDOH stratified

## pct black
r1_all <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(pct = mean(pct_black, na.rm =TRUE),
                                                                  sd = sd(pct_black, na.rm =TRUE))

names(r1_all)[c(2:3)] <- c("pct_black", "sd_black")

r1_all_reorg <- cbind(r1_all[which(r1_all$p2_class4 == 1),], r1_all[which(r1_all$p2_class4 == 2),],r1_all[which(r1_all$p2_class4 == 3),],r1_all[which(r1_all$p2_class4 == 4),])
r1_all_reorg <- r1_all_reorg[,c(2,3,5,6,8,9,11,12)]
names(r1_all_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


r1 <- dat %>% dplyr::group_by(p2_class4,sdoh2_quant) %>% dplyr::summarize(pct = mean(pct_black, na.rm =TRUE),
                                                                          sd = sd(pct_black, na.rm =TRUE))


names(r1)[c(3,4)] <- c("pct_black", "sd_black")

r1_reorg <- cbind(r1[which(r1$p2_class4 == 1),], r1[which(r1$p2_class4 == 2),],r1[which(r1$p2_class4 == 3),],r1[which(r1$p2_class4 == 4),])
r1_reorg <- r1_reorg[,c(3,4,7,8,11,12,15,16)]
names(r1_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


## pct hispanic
r2_all <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(pct = mean(pct_hisp, na.rm =TRUE),
                                                                  sd = sd(pct_hisp, na.rm =TRUE))


names(r2_all)[c(2,3)] <- c("pct_hisp", "sd_hisp")

r2_all_reorg <- cbind(r2_all[which(r2_all$p2_class4 == 1),], r2_all[which(r2_all$p2_class4 == 2),],r2_all[which(r2_all$p2_class4 == 3),],r2_all[which(r2_all$p2_class4 == 4),])
r2_all_reorg <- r2_all_reorg[,c(2,3,5,6,8,9,11,12)]
names(r2_all_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


r2 <- dat %>% dplyr::group_by(p2_class4,sdoh2_quant) %>% dplyr::summarize(pct = mean(pct_hisp, na.rm =TRUE),
                                                                          sd = sd(pct_hisp, na.rm =TRUE))

names(r2)[c(3,4)] <- c("pct_hisp", "sd_hisp")

r2_reorg <- cbind(r2[which(r2$p2_class4 == 1),], r2[which(r2$p2_class4 == 2),],r2[which(r2$p2_class4 == 3),],r2[which(r2$p2_class4 == 4),])
r2_reorg <- r2_reorg[,c(3,4,7,8,11,12,15,16)]
names(r2_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


## Median age
r3_all <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(pct = mean(median_age, na.rm =TRUE),
                                                                  sd = sd(median_age, na.rm =TRUE))

names(r3_all)[c(2,3)] <- c("pct_age", "sd_age")

r3_all_reorg <- cbind(r3_all[which(r3_all$p2_class4 == 1),], r3_all[which(r3_all$p2_class4 == 2),],r3_all[which(r3_all$p2_class4 == 3),],r3_all[which(r3_all$p2_class4 == 4),])
r3_all_reorg <- r3_all_reorg[,c(2,3,5,6,8,9,11,12)]
names(r3_all_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


r3 <- dat %>% dplyr::group_by(p2_class4,sdoh2_quant) %>% dplyr::summarize(pct = mean(median_age, na.rm =TRUE),
                                                                          sd = sd(median_age, na.rm =TRUE))


names(r3)[c(3,4)] <- c("pct_age", "sd_age")

r3_reorg <- cbind(r3[which(r3$p2_class4 == 1),], r3[which(r3$p2_class4 == 2),],r3[which(r3$p2_class4 == 3),],r3[which(r3$p2_class4 == 4),])
r3_reorg <- r3_reorg[,c(3,4,7,8,11,12,15,16)]
names(r3_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")



## income 
r4_all <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(pct = mean(median_household_income, na.rm =TRUE),
                                                                  sd = sd(median_household_income, na.rm =TRUE))

names(r4_all)[c(2,3)] <- c("pct_income", "sd_income")

r4_all_reorg <- cbind(r4_all[which(r4_all$p2_class4 == 1),], r4_all[which(r4_all$p2_class4 == 2),],r4_all[which(r4_all$p2_class4 == 3),],r4_all[which(r4_all$p2_class4 == 4),])
r4_all_reorg <- r4_all_reorg[,c(2,3,5,6,8,9,11,12)]
names(r4_all_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


r4 <- dat %>% dplyr::group_by(p2_class4,sdoh2_quant) %>% dplyr::summarize(pct = mean(median_household_income, na.rm =TRUE),
                                                                          sd = sd(median_household_income, na.rm =TRUE))

names(r4)[c(3,4)] <- c("pct_income", "sd_income")

r4_reorg <- cbind(r4[which(r4$p2_class4 == 1),], r4[which(r4$p2_class4 == 2),],r4[which(r4$p2_class4 == 3),],r4[which(r4$p2_class4 == 4),])
r4_reorg <- r4_reorg[,c(3,4,7,8,11,12,15,16)]
names(r4_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


summary <- rbind(r1_all_reorg, 
                 r1_reorg,
                 r2_all_reorg, 
                 r2_reorg,
                 r3_all_reorg, 
                 r3_reorg,
                 r4_all_reorg, 
                 r4_reorg)

summary$group <- rep(c("All", "SDOH Q1", "SDOH Q2", "SDOH Q3", "SDOH Q4"), 4)
summary$var <- c(rep("Pct. Black", 5), 
                 rep("Pct Hisp.", 5),
                 rep("Median age", 5),
                 rep("Median income", 5))


summary_final_p2 <- summary[,c(10,9,1:8)]



##### Table S6: Period 3 ######## 

## Overall 

pct_black <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(pct_black = mean(pct_black, na.rm =TRUE))
pct_black_sd <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(pct_black = sd(pct_black, na.rm =TRUE))

pct_hisp <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(pct_hisp = mean(pct_hisp, na.rm =TRUE))
pct_hisp_sd <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(pct_hisp = sd(pct_hisp, na.rm =TRUE))

med_age <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(med_age = mean(median_age, na.rm =TRUE))
med_age_sd <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(med_age = sd(median_age, na.rm =TRUE))

med_income <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(med_income = mean(median_household_income, na.rm =TRUE))
med_income_sd <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(med_income = sd(median_household_income, na.rm =TRUE))

tab1 <- rbind(t(pct_black),
              t(pct_hisp),
              t(med_age),
              t(med_income))

tab2 <- rbind(t(pct_black_sd),
              t(pct_hisp_sd),
              t(med_age_sd),
              t(med_income_sd))


tab_final_p3 <- as.data.frame(rbind(tab1[2,], 
                                    tab2[2,],
                                    tab1[4,], 
                                    tab2[4,],
                                    tab1[6,], 
                                    tab2[6,],
                                    tab1[8,], 
                                    tab2[8,]))


tab_final_p3$measure <- rep(c("Mean","SD"), 4)
tab_final_p3$var <- c("Percent Black","Percent Black", 
                      "Percent Hispanic", "Percent Hispanic", 
                      "Median Age", "Median Age",
                      "Median Income", "Median Income")

tab_final_p3 <- tab_final_p3[,c(6,5,1:4)]


## SDOH stratified

## pct black
r1_all <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(pct = mean(pct_black, na.rm =TRUE),
                                                                  sd = sd(pct_black, na.rm =TRUE))

names(r1_all)[c(2,3)] <- c("pct_black","sd_black")

r1_all_reorg <- cbind(r1_all[which(r1_all$p3_class4 == 1),], r1_all[which(r1_all$p3_class4 == 2),],r1_all[which(r1_all$p3_class4 == 3),],r1_all[which(r1_all$p3_class4 == 4),])
r1_all_reorg <- r1_all_reorg[,c(2,3,5,6,8,9,11,12)]
names(r1_all_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


r1 <- dat %>% dplyr::group_by(p3_class4,sdoh2_quant) %>% dplyr::summarize(pct = mean(pct_black, na.rm =TRUE),
                                                                          sd = sd(pct_black, na.rm =TRUE))

names(r1)[c(3,4)] <- c("pct_black","sd_black")

r1_reorg <- cbind(r1[which(r1$p3_class4 == 1),], r1[which(r1$p3_class4 == 2),],r1[which(r1$p3_class4 == 3),],r1[which(r1$p3_class4 == 4),])
r1_reorg <- r1_reorg[,c(3,4,7,8,11,12,15,16)]
names(r1_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


## pct hispanic
r2_all <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(pct = mean(pct_hisp, na.rm =TRUE),
                                                                  sd = sd(pct_hisp, na.rm =TRUE))

names(r2_all)[c(2,3)] <- c("pct_hisp", "se_hisp")

r2_all_reorg <- cbind(r2_all[which(r2_all$p3_class4 == 1),], r2_all[which(r2_all$p3_class4 == 2),],r2_all[which(r2_all$p3_class4 == 3),],r2_all[which(r2_all$p3_class4 == 4),])
r2_all_reorg <- r2_all_reorg[,c(2,3,5,6,8,9,11,12)]
names(r2_all_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


r2 <- dat %>% dplyr::group_by(p3_class4,sdoh2_quant) %>% dplyr::summarize(pct = mean(pct_hisp, na.rm =TRUE),
                                                                          sd = sd(pct_hisp, na.rm =TRUE))

names(r2)[c(3,4)] <- c("pct_hisp", "se_hisp")

r2_reorg <- cbind(r2[which(r2$p3_class4 == 1),], r2[which(r2$p3_class4 == 2),],r2[which(r2$p3_class4 == 3),],r2[which(r2$p3_class4 == 4),])
r2_reorg <- r2_reorg[,c(3,4,7,8,11,12,15,16)]
names(r2_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")



## Median age
r3_all <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(pct = mean(median_age, na.rm =TRUE),
                                                                  sd = sd(median_age, na.rm =TRUE))

names(r3_all)[c(2,3)] <- c("pct_age", "sd_age")

r3_all_reorg <- cbind(r3_all[which(r3_all$p3_class4 == 1),], r3_all[which(r3_all$p3_class4 == 2),],r3_all[which(r3_all$p3_class4 == 3),],r3_all[which(r3_all$p3_class4 == 4),])
r3_all_reorg <- r3_all_reorg[,c(2,3,5,6,8,9,11,12)]
names(r3_all_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


r3 <- dat %>% dplyr::group_by(p3_class4,sdoh2_quant) %>% dplyr::summarize(pct = mean(median_age, na.rm =TRUE),
                                                                          sd = sd(median_age, na.rm =TRUE))


names(r3)[c(3,4)] <- c("pct_age", "sd_age")

r3_reorg <- cbind(r3[which(r3$p3_class4 == 1),], r3[which(r3$p3_class4 == 2),],r3[which(r3$p3_class4 == 3),],r3[which(r3$p3_class4 == 4),])
r3_reorg <- r3_reorg[,c(3,4,7,8,11,12,15,16)]
names(r3_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


## income 
r4_all <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(pct = mean(median_household_income, na.rm =TRUE),
                                                                  sd = sd(median_household_income, na.rm =TRUE))

names(r4_all)[c(3,4)] <- c("pct_income","sd_income")

r4_all_reorg <- cbind(r4_all[which(r4_all$p3_class4 == 1),], r4_all[which(r4_all$p3_class4 == 2),],r4_all[which(r4_all$p3_class4 == 3),],r4_all[which(r4_all$p3_class4 == 4),])
r4_all_reorg <- r4_all_reorg[,c(2,3,5,6,8,9,11,12)]
names(r4_all_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


r4 <- dat %>% dplyr::group_by(p3_class4,sdoh2_quant) %>% dplyr::summarize(pct = mean(median_household_income, na.rm =TRUE),
                                                                          sd = sd(median_household_income, na.rm =TRUE))

names(r4)[c(3,4)] <- c("pct_income", "sd_income")

r4_reorg <- cbind(r4[which(r4$p3_class4 == 1),], r4[which(r4$p3_class4 == 2),],r4[which(r4$p3_class4 == 3),],r4[which(r4$p3_class4 == 4),])
r4_reorg <- r4_reorg[,c(3,4,7,8,11,12,15,16)]
names(r4_reorg) <- c("mean_cat1","sd_cat1","mean_cat2","sd_cat2","mean_cat3","sd_cat3","mean_cat4","sd_cat4")


summary <- rbind(r1_all_reorg, 
                 r1_reorg,
                 r2_all_reorg, 
                 r2_reorg,
                 r3_all_reorg, 
                 r3_reorg,
                 r4_all_reorg, 
                 r4_reorg)

summary$group <- rep(c("All", "SDOH Q1", "SDOH Q2", "SDOH Q3", "SDOH Q4"), 4)
summary$var <- c(rep("Pct. Black", 5), 
                 rep("Pct Hisp.", 5),
                 rep("Median age", 5),
                 rep("Median income", 5))


summary_final_p3 <- summary[,c(10,9,1:8)]


write.csv(summary_final_p1, paste0(out_path,"Table_S6_p1.csv"), row.names = FALSE)
write.csv(summary_final_p2, paste0(out_path,"Table_S6_p2.csv"), row.names = FALSE)
write.csv(summary_final_p3, paste0(out_path,"Table_S6_p3.csv"), row.names = FALSE)


#############################################################################################################
#### Prepare files for FIGURE 3 and  Figure S2                                                         ######
#############################################################################################################

##################################################################
# For urban and rural 
# dim(dat)
# table(dat$ur_group) rural:1988, urban:1154
# save data
# dat_full <- dat
# dat <- dat_full[dat_full$ur_group=="rural",]
# dat <- dat_full[dat_full$ur_group=="urban",]
# dim(dat)
##################################################################

#### Figure 3: Period 1 ####

r1 <- dat %>% dplyr::group_by(p1_class5,sdoh2_quant) %>% dplyr::summarize(var = "Pct. Black or African American",
                                                                          pct = mean(pct_black, na.rm =TRUE),
                                                                          se = std.error(pct_black, na.rm =TRUE))

r2 <- dat %>% dplyr::group_by(p1_class5,sdoh2_quant) %>% dplyr::summarize(var = "Pct. Hispanic or Latino",
                                                                          pct = mean(pct_hisp, na.rm =TRUE),
                                                                          se = std.error(pct_hisp, na.rm =TRUE))

r3 <- dat %>% dplyr::group_by(p1_class5,sdoh2_quant) %>% dplyr::summarize(var = "Median age",
                                                                          pct = mean(median_age, na.rm =TRUE),
                                                                          se = std.error(median_age, na.rm =TRUE))

r4 <- dat %>% dplyr::group_by(p1_class5,sdoh2_quant) %>% dplyr::summarize(var = "Median household income",
                                                                          pct = mean(median_household_income, na.rm =TRUE),
                                                                          se = std.error(median_household_income, na.rm =TRUE))


r1_full <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(var = "Pct. Black or African American",
                                                                   pct = mean(pct_black, na.rm =TRUE),
                                                                   se = std.error(pct_black, na.rm =TRUE))

r2_full <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(var = "Pct. Hispanic or Latino",
                                                                   pct = mean(pct_hisp, na.rm =TRUE),
                                                                   se = std.error(pct_hisp, na.rm =TRUE))

r3_full <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(var = "Median age",
                                                                   pct = mean(median_age, na.rm =TRUE),
                                                                   se = std.error(median_age, na.rm =TRUE))

r4_full <- dat %>% dplyr::group_by(p1_class5) %>% dplyr::summarize(var = "Median household income",
                                                                   pct = mean(median_household_income, na.rm =TRUE),
                                                                   se = std.error(median_household_income, na.rm =TRUE))


all_p1 <- rbind(r1, r2, r3, r4)
all_p1_full <- rbind(r1_full, r2_full, r3_full, r4_full)

all_p1_full$sdoh2_quant <- "All"
all_p1_full <- all_p1_full[,c(1,5,2,3,4)]

all_p1 <- rbind(as.data.frame(all_p1_full), as.data.frame(all_p1))
                        
                        
                        
#### Figure 3: Period 2 #####

r1 <- dat %>% dplyr::group_by(p2_class4,sdoh2_quant) %>% dplyr::summarize(var = "Pct. Black or African American",
                                                                          pct = mean(pct_black, na.rm =TRUE),
                                                                          se = std.error(pct_black, na.rm =TRUE))

r2 <- dat %>% dplyr::group_by(p2_class4,sdoh2_quant) %>% dplyr::summarize(var = "Pct. Hispanic or Latino",
                                                                          pct = mean(pct_hisp, na.rm =TRUE),
                                                                          se = std.error(pct_hisp, na.rm =TRUE))

r3 <- dat %>% dplyr::group_by(p2_class4,sdoh2_quant) %>% dplyr::summarize(var = "Median age",
                                                                          pct = mean(median_age, na.rm =TRUE),
                                                                          se = std.error(median_age, na.rm =TRUE))

r4 <- dat %>% dplyr::group_by(p2_class4,sdoh2_quant) %>% dplyr::summarize(var = "Median household income",
                                                                          pct = mean(median_household_income, na.rm =TRUE),
                                                                          se = std.error(median_household_income, na.rm =TRUE))




r1_full <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(var = "Pct. Black or African American",
                                                                   pct = mean(pct_black, na.rm =TRUE),
                                                                   se = std.error(pct_black, na.rm =TRUE))

r2_full <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(var = "Pct. Hispanic or Latino",
                                                                   pct = mean(pct_hisp, na.rm =TRUE),
                                                                   se = std.error(pct_hisp, na.rm =TRUE))

r3_full <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(var = "Median age",
                                                                   pct = mean(median_age, na.rm =TRUE),
                                                                   se = std.error(median_age, na.rm =TRUE))

r4_full <- dat %>% dplyr::group_by(p2_class4) %>% dplyr::summarize(var = "Median household income",
                                                                   pct = mean(median_household_income, na.rm =TRUE),
                                                                   se = std.error(median_household_income, na.rm =TRUE))


all_p2 <- rbind(r1, r2, r3, r4)
all_p2_full <- rbind(r1_full, r2_full, r3_full, r4_full)

all_p2_full$sdoh2_quant <- "All"
all_p2_full <- all_p2_full[,c(1,5,2,3,4)]

all_p2 <- rbind(as.data.frame(all_p2_full), as.data.frame(all_p2))




##### Figure 3: Period 3 #####

r1 <- dat %>% dplyr::group_by(p3_class4,sdoh2_quant) %>% dplyr::summarize(var = "Pct. Black or African American",
                                                                          pct = mean(pct_black, na.rm =TRUE),
                                                                          se = std.error(pct_black, na.rm =TRUE))

r2 <- dat %>% dplyr::group_by(p3_class4,sdoh2_quant) %>% dplyr::summarize(var = "Pct. Hispanic or Latino",
                                                                          pct = mean(pct_hisp, na.rm =TRUE),
                                                                          se = std.error(pct_hisp, na.rm =TRUE))

r3 <- dat %>% dplyr::group_by(p3_class4,sdoh2_quant) %>% dplyr::summarize(var = "Median age",
                                                                          pct = mean(median_age, na.rm =TRUE),
                                                                          se = std.error(median_age, na.rm =TRUE))

r4 <- dat %>% dplyr::group_by(p3_class4,sdoh2_quant) %>% dplyr::summarize(var = "Median household income",
                                                                          pct = mean(median_household_income, na.rm =TRUE),
                                                                          se = std.error(median_household_income, na.rm =TRUE))


r1_full <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(var = "Pct. Black or African American",
                                                                   pct = mean(pct_black, na.rm =TRUE),
                                                                   se = std.error(pct_black, na.rm =TRUE))

r2_full <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(var = "Pct. Hispanic or Latino",
                                                                   pct = mean(pct_hisp, na.rm =TRUE),
                                                                   se = std.error(pct_hisp, na.rm =TRUE))

r3_full <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(var = "Median age",
                                                                   pct = mean(median_age, na.rm =TRUE),
                                                                   se = std.error(median_age, na.rm =TRUE))

r4_full <- dat %>% dplyr::group_by(p3_class4) %>% dplyr::summarize(var = "Median household income",
                                                                   pct = mean(median_household_income, na.rm =TRUE),
                                                                   se = std.error(median_household_income, na.rm =TRUE))


all_p3 <- rbind(r1, r2, r3, r4)
all_p3_full <- rbind(r1_full, r2_full, r3_full, r4_full)

all_p3_full$sdoh2_quant <- "All"
all_p3_full <- all_p3_full[,c(1,5,2,3,4)]

all_p3 <- rbind(as.data.frame(all_p3_full), as.data.frame(all_p3))


## create CIs and clean up

all_p1$upper <- all_p1$pct + (1.96*all_p1$se)
all_p1$lower <- all_p1$pct - (1.96*all_p1$se)

all_p2$upper <- all_p2$pct + (1.96*all_p2$se)
all_p2$lower <- all_p2$pct - (1.96*all_p2$se)

all_p3$upper <- all_p3$pct + (1.96*all_p3$se)
all_p3$lower <- all_p3$pct - (1.96*all_p3$se)


all_p1$period <- "T1: Mar. 15 to Jun. 14"
all_p2$period <- "T2: Jun. 15 to Aug. 14"
all_p3$period <- "T3: Aug. 15 to Nov. 2"

names(all_p1)[1] <- "trajGroup"
names(all_p2)[1] <- "trajGroup"
names(all_p3)[1] <- "trajGroup"


all <- rbind(all_p1, all_p2, all_p3)

all$trajGroup_save <- all$trajGroup
all$trajGroup[which(all$trajGroup_save == 1)] <- "Class 1"
all$trajGroup[which(all$trajGroup_save == 2)] <- "Class 2"
all$trajGroup[which(all$trajGroup_save == 3)] <- "Class 3"
all$trajGroup[which(all$trajGroup_save == 4)] <- "Class 4"
all$trajGroup[which(all$trajGroup_save == 5)] <- "Class 5"


# Period 1 

# Period 2

# Period 3


all_new <- all
all_new$trajGroup[which(all_new$period == "T1: Mar. 15 to Jun. 14" & all_new$trajGroup_save == 1)] <- "Class 1"
all_new$trajGroup[which(all_new$period == "T1: Mar. 15 to Jun. 14" & all_new$trajGroup_save == 2)] <- "Class 2"
all_new$trajGroup[which(all_new$period == "T1: Mar. 15 to Jun. 14" & all_new$trajGroup_save == 3)] <- "Class 3"
all_new$trajGroup[which(all_new$period == "T1: Mar. 15 to Jun. 14" & all_new$trajGroup_save == 4)] <- "Class 4"
all_new$trajGroup[which(all_new$period == "T1: Mar. 15 to Jun. 14" & all_new$trajGroup_save == 5)] <- "Class 5"

all_new$trajGroup[which(all_new$period == "T2: Jun. 15 to Aug. 14" & all_new$trajGroup_save == 1)] <- "Class 1/2"
all_new$trajGroup[which(all_new$period == "T2: Jun. 15 to Aug. 14" & all_new$trajGroup_save == 2)] <- "Class 3"
all_new$trajGroup[which(all_new$period == "T2: Jun. 15 to Aug. 14" & all_new$trajGroup_save == 3)] <- "Class 4"
all_new$trajGroup[which(all_new$period == "T2: Jun. 15 to Aug. 14" & all_new$trajGroup_save == 4)] <- "Class 5"


all_new$trajGroup[which(all_new$period == "T3: Aug. 15 to Nov. 2" & all_new$trajGroup_save == 1)] <- "Class 1/2"
all_new$trajGroup[which(all_new$period == "T3: Aug. 15 to Nov. 2" & all_new$trajGroup_save == 2)] <- "Class 3"
all_new$trajGroup[which(all_new$period == "T3: Aug. 15 to Nov. 2" & all_new$trajGroup_save == 3)] <- "Class 4"
all_new$trajGroup[which(all_new$period == "T3: Aug. 15 to Nov. 2" & all_new$trajGroup_save == 4)] <- "Class 5"

all_new$sdoh2_quant[which(all_new$sdoh2_quant == 1)] <- "<10th"
all_new$sdoh2_quant[which(all_new$sdoh2_quant == 2)] <- "≥10th to <50th"
all_new$sdoh2_quant[which(all_new$sdoh2_quant == 3)] <- "≥50th to <90th"
all_new$sdoh2_quant[which(all_new$sdoh2_quant == 4)] <- "≥90th"

all_new$sdoh2_quant <- factor(all_new$sdoh2_quant, levels = c("<10th",
                                                              "≥10th to <50th",
                                                              "≥50th to <90th",
                                                              "≥90th",
                                                              "All"))
                        

colors <- c("#4eb3d3",
            "#2b8cbe",
            "#0868ac",
            "#084081",
            "darkorange4")
                        
all_new$lower[all_new$lower < 0] <- 0 # truncate for visualization

income_bar1_legend <- all_new[which(all_new$var == "Median household income" & all_new$period == "T1: Mar. 15 to Jun. 14"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("Median Income: \n Mar. 15 - Jun. 14") +
  labs(x = "Trajectory group", y = "Mean income (95% CI)", color = "SDOH Group\n Percentile") + 
  #facet_wrap(.~period)  +
  coord_cartesian(ylim=c(30000, 90000)) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5), cex = 0.2) + 
  scale_color_manual(values = colors) 


income_bar1 <- all_new[which(all_new$var == "Median household income" & all_new$period == "T1: Mar. 15 to Jun. 14"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("d. Median Income: \n Mar. 15 - Jun. 14") +
  labs(x = "Trajectory group", y = "Mean income (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period)  +
  coord_cartesian(ylim=c(30000, 90000)) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5), cex = 0.2) + 
  scale_color_manual(values = colors) + 
  theme(legend.position = "none") 


income_bar2 <- all_new[which(all_new$var == "Median household income" & all_new$period == "T2: Jun. 15 to Aug. 14"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("e. Median Income: \n Jun. 15 - Aug. 14") +
  labs(x = "Trajectory group", y = "Mean income (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period)  +
  coord_cartesian(ylim=c(30000, 90000)) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5), cex = 0.2) +
  scale_color_manual(values = colors) + 
  theme(legend.position = "none")  


income_bar3 <- all_new[which(all_new$var == "Median household income" & all_new$period == "T3: Aug. 15 to Nov. 2"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("f. Median Income: \n Aug. 15 - Nov. 2") +
  labs(x = "Trajectory group", y = "Mean income (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period)  +
  coord_cartesian(ylim=c(30000, 90000)) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5), cex = 0.2) + 
  scale_color_manual(values = colors) +
  theme(legend.position = "none")  
                        

##

pct_black_bar1 <- all_new[which(all_new$var == "Pct. Black or African American" & all_new$period == "T1: Mar. 15 to Jun. 14"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("a. Pct. Black/African American: \n Mar. 15 - Jun. 14") +
  labs(x = "Trajectory group", y = "Mean percentage (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period)+
  coord_cartesian(ylim=c(0, 48)) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5), cex = 0.2) + 
  scale_color_manual(values = colors) + 
  theme(legend.position = "none")


pct_black_bar2 <- all_new[which(all_new$var == "Pct. Black or African American" & all_new$period == "T2: Jun. 15 to Aug. 14"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("b. Pct. Black/African American: \n Jun. 15 - Aug. 14") +
  labs(x = "Trajectory group", y = "Mean percentage (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period)+
  coord_cartesian(ylim=c(0, 48)) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5), cex = 0.2) + 
  scale_color_manual(values = colors) + 
  theme(legend.position = "none")  


pct_black_bar3 <- all_new[which(all_new$var == "Pct. Black or African American" & all_new$period == "T3: Aug. 15 to Nov. 2"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("c. Pct. Black/African American: \n Aug. 15 - Nov. 2") +
  labs(x = "Trajectory group", y = "Mean percentage (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period)+
  coord_cartesian(ylim=c(0, 48)) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5), cex = 0.2) + 
  scale_color_manual(values = colors) + 
  theme(legend.position = "none") 


## 

pct_hisp_bar1 <- all_new[which(all_new$var == "Pct. Hispanic or Latino" & all_new$period == "T1: Mar. 15 to Jun. 14"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("a. Pct. Hispanic/Latino/a: \n Mar. 15 - Jun. 14") +
  labs(x = "Trajectory group", y = "Mean percentage (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period) + 
  coord_cartesian(ylim=c(0, 35)) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5), cex = 0.2) + 
  scale_color_manual(values = colors) + 
  theme(legend.position = "none")  


pct_hisp_bar2 <- all_new[which(all_new$var == "Pct. Hispanic or Latino" & all_new$period == "T2: Jun. 15 to Aug. 14"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("b. Pct. Hispanic/Latino/a: \n Jun. 15 - Aug. 14") +
  labs(x = "Trajectory group", y = "Mean percentage (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period) + 
  coord_cartesian(ylim=c(0, 35)) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5), cex = 0.2) + 
  scale_color_manual(values = colors) + 
  theme(legend.position = "none")  


pct_hisp_bar3 <- all_new[which(all_new$var == "Pct. Hispanic or Latino" & all_new$period == "T3: Aug. 15 to Nov. 2"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("c. Pct. Hispanic/Latino/a: \n Aug. 15 - Nov. 2") +
  labs(x = "Trajectory group", y = "Mean percentage (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period) + 
  coord_cartesian(ylim=c(0, 35)) + 
  geom_vline(xintercept = c(1.5, 2.5, 3.5), cex = 0.2) + 
  scale_color_manual(values = colors) + 
  theme(legend.position = "none")


## 

age_bar1 <- all_new[which(all_new$var == "Median age" & all_new$period == "T1: Mar. 15 to Jun. 14"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("d. Median Age: \n Mar. 15 - Jun. 14") +
  labs(x = "Trajectory group", y = "Mean years (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period) +
  coord_cartesian(ylim=c(35, 55)) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5), cex = 0.2) + 
  scale_color_manual(values = colors) + 
  theme(legend.position = "none")  


age_bar2 <- all_new[which(all_new$var == "Median age" & all_new$period == "T2: Jun. 15 to Aug. 14"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("e. Median Age: \n Jun. 15 - Aug. 14") +
  labs(x = "Trajectory group", y = "Mean years (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period) +
  coord_cartesian(ylim=c(35, 55)) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), cex = 0.2) + 
  scale_color_manual(values = colors) + 
  theme(legend.position = "none")  


age_bar3 <- all_new[which(all_new$var == "Median age" & all_new$period == "T3: Aug. 15 to Nov. 2"),] %>%
  ggplot(aes(x = as.factor(trajGroup), y = pct, group = as.factor(sdoh2_quant), color = as.factor(sdoh2_quant))) +
  geom_point(position=position_dodge(width=0.9), stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position=position_dodge(width=0.9)) +
  #theme_light() +
  #theme(panel.grid.minor = element_blank()) + 
  ggtitle("f. Median Age: \n Aug. 15 - Nov. 2") +
  labs(x = "Trajectory group", y = "Mean years (95% CI)", color = "SDOHi \n Group") + 
  #facet_wrap(.~period) +
  coord_cartesian(ylim=c(35, 55)) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), cex = 0.2) + 
  scale_color_manual(values = colors) + 
  theme(legend.position = "none")  


                        
legend <- get_legend(income_bar1_legend)


###################################################################################################################################################################################
#### FIGURE 3                                                                                                                                                            ##########
###################################################################################################################################################################################


# png(paste0(out_path,"Table3_graphic_bars_inText.png"), units="in", width=12, height=12, res=600)

# png(paste0(out_path,"Table3_graphic_bars_inText_rural.png"), units="in", width=12, height=12, res=600)

png(paste0(out_path,"Table3_graphic_bars_inText_urban.png"), units="in", width=12, height=12, res=600)

grid.arrange(pct_black_bar1, pct_black_bar2, pct_black_bar3, legend,
             age_bar1, age_bar2, age_bar3, legend, 
             ncol = 4, widths = c(2.3, 2.3, 2.3, 0.8))

dev.off()


###################################################################################################################################################################################
#### Figure S2. Hispanic or Latino/a Population and Household Income in U.S. Counties by COVID-19 Trajectory Group and SDOH Group, Across Three Study Periods (n=3,142)  ##########
###################################################################################################################################################################################

# png(paste0(out_path,"Table3_graphic_bars_appendix.png"), units="in", width=12, height=12, res=600)

# png(paste0(out_path,"Table3_graphic_bars_appendix_rural.png"), units="in", width=12, height=12, res=600)

png(paste0(out_path,"Table3_graphic_bars_appendix_urban.png"), units="in", width=12, height=12, res=600)

grid.arrange(pct_hisp_bar1, pct_hisp_bar2, pct_hisp_bar3, legend,
             income_bar1, income_bar2, income_bar3, legend, 
             ncol = 4, widths = c(2.3, 2.3, 2.3, 0.8))

dev.off()
                        
                        
                        
                        
                        