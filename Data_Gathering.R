library(dplyr)
library(ggplot2)
library(rvest)
library(XML)
library(htmltab)


###### For Loop ######

years <- (seq(2004, 2020, 1))

# val <- 2005

Full_Dataset <- data.frame()



 for (val in years){
  
  ################ SG_APR, SG_ARG, SG_OTT  ############
  
  players_i <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.02674.y', val, '.html', sep = "")),
                                                  '.player-name')))
  
  hidden_medium <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.02674.y', val, '.html', sep = "")),
                                                      '.hidden-medium')))
  
  hidden_medium <- as.data.frame(hidden_medium[-1,])
  
  names(hidden_medium) <- 'col_1'
  
  out <- as.data.frame(matrix(hidden_medium$col_1[hidden_medium$col_1!=""], ncol=6, byrow=TRUE))
  
  blah <- cbind(players_i, out)
  
  blah <- blah[,-c(2,3,7)]
  
  names(blah) <- c("Player_Name", "SG_Off_The_Tee", "SG_Approach", "SG_Around_Green")
  
  done <- blah[-1,]
  
  done$Player_Name <- as.character(done$Player_Name)
  
  done$Player_Name <- trimws(done$Player_Name)
  
  
  
  ##################### Driving Distance #######################
  
  distance_table <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/stats/stat.101.y', val, '.html', sep = "")),
                                                       'td')))
  
  distance_table <- as.data.frame(distance_table[-c(1,2,3),])
  
  names(distance_table) <- 'col_1'
  
  distance_table <- as.data.frame(matrix(distance_table$col_1[distance_table$col_1!=""], ncol=7, byrow=TRUE))
  
  distance_table <- as.data.frame(distance_table[c(3,5)])
  
  names(distance_table) <- c("Player_Name", "Driving_Distance")
  
  distance_table$Player_Name <- as.character(distance_table$Player_Name)
  
  distance_table$Player_Name <- trimws(distance_table$Player_Name)
  
  done <- left_join(done, distance_table, by = "Player_Name")
  
  done$Player_Name <- as.character(done$Player_Name)
  

  
  ######################## Driving Accuracy Percentage #####################
  
  accuracy_table <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.102.y', val, '.html', sep = "")),
                                                       'td')))
  
  accuracy_table <- as.data.frame(accuracy_table[-c(1,2,3),])
  
  names(accuracy_table) <- 'col_1'
  
  accuracy_table <- as.data.frame(matrix(accuracy_table$col_1[accuracy_table$col_1!=""], ncol=7, byrow=TRUE))
  
  accuracy_table <- as.data.frame(accuracy_table[c(3,5)])
  
  names(accuracy_table) <- c("Player_Name", "Driving_Accuracy")
  
 #  accuracy_table$Driving_Accuracy <- as.numeric(accuracy_table$Driving_Accuracy)
  
  accuracy_table$Player_Name <- as.character(accuracy_table$Player_Name)
  
  accuracy_table$Player_Name <- trimws(accuracy_table$Player_Name)
  
  done <- left_join(done, accuracy_table, by = "Player_Name")
  
  done$Player_Name <- as.character(done$Player_Name)
  

  
  ############################ Greens in Regulation Percentage #########################
  
  GIR_table <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.103.y', val, '.html', sep = "")),
                                                       'td')))
  
  GIR_table <- as.data.frame(GIR_table[-c(1,2,3),])
  
  names(GIR_table) <- 'col_1'
  
  GIR_table <- as.data.frame(matrix(GIR_table$col_1[GIR_table$col_1!=""], ncol=8, byrow=TRUE))
  
  GIR_table <- as.data.frame(GIR_table[c(3,5)])
  
  names(GIR_table) <- c("Player_Name", "Greens_In_Regulation")
  
  GIR_table$Player_Name <- as.character(GIR_table$Player_Name)
  
  GIR_table$Player_Name <- trimws(GIR_table$Player_Name)
  
  done <- left_join(done, GIR_table, by = "Player_Name")
  
  done$Player_Name <- as.character(done$Player_Name)
  

  
  ################################## Scrambling Percentage #########################
  
  Scrambling_PCT <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.130.y', val, '.html', sep = "")),
                                                  'td')))
  
  Scrambling_PCT <- as.data.frame(Scrambling_PCT[-c(1,2,3),])
  
  names(Scrambling_PCT) <- 'col_1'
  
  Scrambling_PCT <- as.data.frame(matrix(Scrambling_PCT$col_1[Scrambling_PCT$col_1!=""], ncol=7, byrow=TRUE))
  
  Scrambling_PCT <- as.data.frame(Scrambling_PCT[c(3,5)])
  
  names(Scrambling_PCT) <- c("Player_Name", "Scrambling_Percentage")
  
  Scrambling_PCT$Player_Name <- as.character(Scrambling_PCT$Player_Name)
  
  Scrambling_PCT$Player_Name <- trimws(Scrambling_PCT$Player_Name)
  
  done <- left_join(done, Scrambling_PCT, by = "Player_Name")
  
  done$Player_Name <- as.character(done$Player_Name)
  

  ####################### SG_PUtting #################
  
  
  putting  <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/stats/stat.02564.y', val, '.html', sep = "")),
                                                 'td')))
  putting <- as.data.frame(putting[-c(1,2,3),])
  
  names(putting) <- 'col_1'
  
  putting_fixed <- as.data.frame(matrix(putting$col_1[putting$col_1!=""], ncol=7, byrow=TRUE))
  
  putting <- as.data.frame(putting_fixed[c(3,5)])
  
  names(putting) <- c("Player_Name", "SG_PUTT")
  
  putting$Player_Name <- as.character(putting$Player_Name)
  
  putting$Player_Name <- trimws(putting$Player_Name)
  
  done <- left_join(done, putting, by = "Player_Name")
  
  done$Player_Name <- as.character(done$Player_Name)
  

  ################################# Putting Inside 5 feet #########################

  putting_5  <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.403.y', val, '.html', sep = "")),
                                                 'td')))
  putting_5 <- as.data.frame(putting_5[-c(1,2,3),])

  names(putting_5) <- 'col_1'

  putting_5 <- as.data.frame(matrix(putting_5$col_1[putting_5$col_1!=""], ncol=7, byrow=TRUE))

  putting_5 <- as.data.frame(putting_5[c(3,5)])

  names(putting_5) <- c("Player_Name", "Putting_Inside_5")

  putting_5$Player_Name <- as.character(putting_5$Player_Name)

  putting_5$Player_Name <- trimws(putting_5$Player_Name)

  done <- left_join(done, putting_5, by = "Player_Name")

  done$Player_Name <- as.character(done$Player_Name)

  done$Player_Name <- trimws(done$Player_Name)




  # ################################### Putting from 5 to 10 feet #####################

  putting_5_10  <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.404.y', val, '.html', sep = "")),
                                                   'td')))
  putting_5_10 <- as.data.frame(putting_5_10[-c(1,2,3),])

  names(putting_5_10) <- 'col_1'

  putting_5_10 <- as.data.frame(matrix(putting_5_10$col_1[putting_5_10$col_1!=""], ncol=7, byrow=TRUE))

  putting_5_10 <- as.data.frame(putting_5_10[c(3,5)])

  names(putting_5_10) <- c("Player_Name", "Putting_5_10")

  putting_5_10$Player_Name <- as.character(putting_5_10$Player_Name)

  putting_5_10$Player_Name <- trimws(putting_5_10$Player_Name)

  done <- left_join(done, putting_5_10, by = "Player_Name")

  done$Player_Name <- as.character(done$Player_Name)

  done$Player_Name <- trimws(done$Player_Name)






  # ##################################### Putting from 10 to 15 feet #########################
  putting_10_15  <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.405.y', val, '.html', sep = "")),
                                                      'td')))
  putting_10_15 <- as.data.frame(putting_10_15[-c(1,2,3),])

  names(putting_10_15) <- 'col_1'

  putting_10_15 <- as.data.frame(matrix(putting_10_15$col_1[putting_10_15$col_1!=""], ncol=7, byrow=TRUE))

  putting_10_15 <- as.data.frame(putting_10_15[c(3,5)])

  names(putting_10_15) <- c("Player_Name", "putting_10_15")

  putting_10_15$Player_Name <- as.character(putting_10_15$Player_Name)

  putting_10_15$Player_Name <- trimws(putting_10_15$Player_Name)

  done <- left_join(done, putting_10_15, by = "Player_Name")

  done$Player_Name <- as.character(done$Player_Name)








  # ####################################### 15 to 20 feet ######################

  putting_15_20  <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.406.y', val, '.html', sep = "")),
                                                      'td')))
  putting_15_20 <- as.data.frame(putting_15_20[-c(1,2,3),])

  names(putting_15_20) <- 'col_1'

  putting_15_20 <- as.data.frame(matrix(putting_15_20$col_1[putting_15_20$col_1!=""], ncol=7, byrow=TRUE))

  putting_15_20 <- as.data.frame(putting_15_20[c(3,5)])

  names(putting_15_20) <- c("Player_Name", "putting_15_20")

  putting_15_20$Player_Name <- as.character(putting_15_20$Player_Name)

  putting_15_20$Player_Name <- trimws(putting_15_20$Player_Name)

  done <- left_join(done, putting_15_20, by = "Player_Name")

  done$Player_Name <- as.character(done$Player_Name)




  ################################## Putting from 20 to 25 feet  ###################


  putting_20_25  <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.407.y', val, '.html', sep = "")),
                                                       'td')))
  putting_20_25 <- as.data.frame(putting_20_25[-c(1,2,3),])

  names(putting_20_25) <- 'col_1'

  putting_20_25 <- as.data.frame(matrix(putting_20_25$col_1[putting_20_25$col_1!=""], ncol=7, byrow=TRUE))

  putting_20_25 <- as.data.frame(putting_20_25[c(3,5)])

  names(putting_20_25) <- c("Player_Name", "putting_20_25")

  putting_20_25$Player_Name <- as.character(putting_20_25$Player_Name)

  putting_20_25$Player_Name <- trimws(putting_20_25$Player_Name)

  done <- left_join(done, putting_20_25, by = "Player_Name")

  done$Player_Name <- as.character(done$Player_Name)



  ######################### Putting from outside 25 feet #######################

  putting_Outside_25  <- as.data.frame(html_text(html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.408.y', val, '.html', sep = "")),
                                                      'td')))
  putting_Outside_25 <- as.data.frame(putting_Outside_25[-c(1,2,3),])

  names(putting_Outside_25) <- 'col_1'

  putting_Outside_25 <- as.data.frame(matrix(putting_Outside_25$col_1[putting_Outside_25$col_1!=""], ncol=7, byrow=TRUE))

  putting_Outside_25 <- as.data.frame(putting_Outside_25[c(3,5)])

  names(putting_Outside_25) <- c("Player_Name", "putting_Outside_25")

  putting_Outside_25$Player_Name <- as.character(putting_Outside_25$Player_Name)

  putting_Outside_25$Player_Name <- trimws(putting_Outside_25$Player_Name)

  done <- left_join(done, putting_Outside_25, by = "Player_Name")

  done$Player_Name <- as.character(done$Player_Name)




  
  ################# Money Earnings ###################################
  
  
  money <-  as.data.frame(html_text
                          (html_nodes(read_html(paste('https://www.pgatour.com/content/pgatour/stats/stat.109.y', val, '.html', sep = "")),'td')))
  
  money <- as.data.frame(money[-c(1,2,3),])
  
  names(money) <- 'col_1'
  
  money <- as.data.frame(money)
  
  money[money==""] <- "0"
  
  money <- as.data.frame(matrix(money$col_1[money$col_1!=""], ncol=6, byrow=TRUE))
  
  money <- as.data.frame(money[c(3,5)])
  
  names(money) <- c("Player_Name", "Yearly_Earnings")
  
  money$Player_Name <- (trimws(money$Player_Name, which = "both"))
  
  money$Player_Name <- as.character(money$Player_Name)
  
  done$Player_Name <- trimws(done$Player_Name)
  
  Year_i <- left_join(done, money, by = "Player_Name")
  
  Year_i$Year <- val
  
  Year_i$Yearly_Earnings <- as.numeric(gsub('[$,]', '', Year_i$Yearly_Earnings))
  
  Year_i$Player_Name <- trimws(Year_i$Player_Name)
  
  Year_i <- Year_i %>%
    filter(Player_Name != "Richard Johnson")
  
  
  ############## Bind everything together ###################

  
  Full_Dataset <- rbind(Full_Dataset,
                        Year_i)
  
 }

Full_Dataset <- Full_Dataset %>%
  mutate(Yearly_Earnings = case_when(Player_Name == "Jay Williamson" & Year == 2007 ~ 835515, TRUE ~ Yearly_Earnings))

Full_Dataset <- Full_Dataset %>%
  mutate(Yearly_Earnings = case_when(Player_Name == "Ryo Ishikawa" & Year == 2012 ~ 727051, TRUE ~ Yearly_Earnings))

Full_Dataset <- Full_Dataset %>%
  mutate(Yearly_Earnings = case_when(Player_Name == "Brooks Koepka" & Year == 2014 ~ 1043115, TRUE ~ Yearly_Earnings))

Full_Dataset <- Full_Dataset %>%
  mutate(Yearly_Earnings = case_when(Player_Name == "Patrick Rodgers" & Year == 2015 ~ 120282, TRUE ~ Yearly_Earnings))

Full_Dataset <- Full_Dataset %>%
  mutate(Yearly_Earnings = case_when(Player_Name == "Kiradech Aphibarnrat" & Year == 2018 ~ 101120, TRUE ~ Yearly_Earnings))

Full_Dataset <- Full_Dataset %>%
  mutate(Yearly_Earnings = case_when(Player_Name == "Matthew Fitzpatrick" & Year == 2019 ~ 1553750, TRUE ~ Yearly_Earnings))

Full_Dataset <- Full_Dataset %>%
  mutate(Yearly_Earnings = case_when(Player_Name == "Will Gordon" & Year == 2020 ~ 773220, TRUE ~ Yearly_Earnings))




  
  
write.csv(Full_Dataset ,"C:/Users/dyson/OneDrive/Documents/School/2020 Fall/Predictive Analytics/Golf_Individual_Project/Career_Data.csv", row.names = FALSE)


