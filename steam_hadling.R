####################################################################################################################
##### read_csv로 무조건 steamid를 character로 읽을 것. 안하면 id값이 커서 numeric 값이 이상함#######################
####################################################################################################################

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(reshape2)
library(RevoUtilsMath)
setMKLthreads(4)
options(scipen = 100)
#### 행변경 함수 #####
arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

##### #####
setwd("D:/Steam_Project/steam_data")

####################################################################################################################
# 
# setwd("C:/Users/pokat/Desktop/steam_data/steam_data")
# appid_tag <- read_csv("tag_2018-04-16.csv", col_types = cols(
#   appid = col_character(),
#   tag = col_character(),
#   order = col_double()
# ))
# 
# #### 어떤 태그들이 많은지 세보기
# appid_tag_unique <- unique(appid_tag$appid)
# appid_tag$tag <- as.factor(appid_tag$tag)
# 
# tag_count <- as.data.frame(table(appid_tag$tag))
# tag_count <- arrange(tag_count, desc(Freq))
# 
# ####################################################################################################################
# 
# appid_tag <- filter(appid_tag, tag != "Early Access" & tag != "Indie")
# for (i in 1:NROW(appid_tag_unique)) {
#   n <- NROW(appid_tag[appid_tag$appid == appid_tag_unique[i], ])
#   appid_tag[which(appid_tag$appid == appid_tag_unique[i]), 3] <- seq(1, n)
#   cat(i, " / ", NROW(appid_tag_unique),  "\n")
# }
# 
# appid_tag_unique <- unique(appid_tag$appid) 
# app_genre <- c()
# app_genre <- data.frame(appid_tag_unique)
# app_genre$tag <- c(0)
# names(app_genre) <- c("appid", "tag")
# 
# appid_tag$tag <- as.character(appid_tag$tag)
# for (i in 1:NROW(appid_tag_unique)) {
#   x <- unlist(which(appid_tag$appid == appid_tag_unique[i]))
#   for (j in 1:NROW(x)) {
#     if (appid_tag[x[j], 2] == "Action" | appid_tag[x[j], 2] == "Adventure" | appid_tag[x[j], 2] == "Puzzle" | 
#         appid_tag[x[j], 2] == "Racing" | appid_tag[x[j], 2] == "Rhythm" | appid_tag[x[j], 2] == "RPG" | 
#         appid_tag[x[j], 2] == "Shooter" | appid_tag[x[j], 2] == "Simulation" | appid_tag[x[j], 2] == "Strategy" | 
#         appid_tag[x[j], 2] == "Sports") {
#       app_genre[app_genre$appid == appid_tag_unique[i], 2] <- appid_tag[x[j], 2]
#     }
#     else {
#       next
#     }
#     break
#   }
#   cat(i, " / ", NROW(appid_tag_unique),  "\n")  
# }
# app_genre <- app_genre[app_genre$tag != 0, ]
# 
# write.csv(app_genre, "app_genre_first_pick_within10.csv", row.names = FALSE)
# 
# setwd("D:/Steam_Project/steam_data")
# app_genre <- read_csv("app_genre_first_pick_within10.csv", col_types = cols(
#   appid = col_character(),
#   tag = col_character()
# ))
# 

##### 태그순서들중 변수로 쓸 10개의 변수중 어느하나가 있으면 그 장르만 남기는 작업

##### 장르별 average price 구하기
# game_data <- read_csv("app_genre_first_pick_within10.csv", col_types = cols(appid = col_character()))
# store_data <- read_csv("D:/Steam_Project/steam_data/store/store/1.steamstore/2018-04-26_steamstore.csv", 
#                        col_types = cols(steam_appid = col_character()))
# store_data <- store_data[!is.na(store_data$original_price),]
# store_data_sep <- separate_rows(store_data, steam_appid, sep = ",")
# names(store_data_sep) <- c("appid", "name", "released_date", "discount_percent", "free_or_not", "original_price",
#                            "final_price", "review_level", "review_score", "review_people", "date")
# merged_data <- merge(game_data, store_data_sep, by = "appid")
# merged_data <- merged_data[ , c(1, 2, 7)]
# merged_data_agg <- aggregate(merged_data[ , 3], by = list(merged_data$appid), mean)
# names(merged_data_agg) <- c("appid", "original_price")
# merged_data_agg <- merge(merged_data_agg, game_data, by = "appid")
# write.csv(merged_data_agg, "app_genre_price_10.csv", row.names = FALSE)

####################################################################################################################
# 2018-03-10 ~ 2018-04-09 기간 이니까 폴더는 0309까지

setwd("D:/Steam_Project/steam_data")
app_genre <- read_csv("app_genre_first_pick_within10.csv", col_types = cols(
  appid = col_character(),
  tag = col_character()
))
app_genre <- as.data.table(app_genre)
setkey(app_genre, appid)

#####  크롤링 시점 폴더들 리스트화
dir_1 <- ("D:/Steam_Project/steam_data/raw")
folder_list <- list.dirs(dir_1)
folder_list2 <- c()
j = 5
for (i in 1:(NROW(folder_list)%/%8)) {
  folder_list2[i] <- folder_list[j]
  j = j + 8
}
folder_list <- folder_list2
folder_list <- folder_list[1:25]
remove(folder_list2)

##### 날짜 리스트 # 2018-03-10 부터 2018-04-09 까지
date <- c()
for (i in 1:31) {
  d <- ymd("2018-03-09") + i
  date[i] <- as_date(d)
}
date <- as_date(date)

##### 저장할 경로 리스트화
save_directory <- list.dirs("D:/Steam_Project/steam_data/gathering")
save_directory <- save_directory[-1]


##### 저장할 파일이름 리스트화
result_file <- c()
for(i in 1:NROW(date)) { 
  result <- paste("playtime_by_genre_", date[i], sep = "")
  result_file[i] <- paste(result, ".csv", sep="")
}

##### 중복ID 처리하기 위한 ID저장 데이터프레임
survived_id_list <- c()
# OR
setwd("D:/Steam_Project/steam_data")
survived_id_list <- read_csv("survival_id_unique.csv", col_types = cols(
  steamid = col_character()
))
#
#
#
gc()

##### START ##### 
system.time(
  # NROW(folder_list)
  for (z in 1:NROW(folder_list)) {
    
    cat(as.character(ymd("2018-02-12") + z), "start.", "\n")
    
    dir_2 <- (folder_list[z])
    file_list <- list.files(dir_2)
    file_list <- file_list[c((26-z):(NROW(file_list)-17))]
    
    setwd(folder_list[z])
    
    survived_id_z <- read_csv(file_list[1], col_types = cols(
      steamid = col_character(),
      appid = col_character(),
      playtime_forever = col_double(),
      playtime_2weeks = col_double(),
      recordtime = col_datetime(format = "")
    ))
    survived_id_z <- as.data.table(unique(survived_id_z$steamid))
    names(survived_id_z) <- c("steamid")
    setkey(survived_id_z, steamid)
    
    for (i in 2:NROW(file_list)) {
      r <- read_csv(file_list[i], col_types = cols(
        steamid = col_character(),
        appid = col_character(),
        playtime_forever = col_double(),
        playtime_2weeks = col_double(),
        recordtime = col_datetime(format = "")
      ))
      r <- as.data.table(unique(r$steamid))
      names(r) <- c("steamid")
      setkey(r, steamid)
      survived_id_z <- survived_id_z[r, nomatch = 0]
      r <- c()
    }
    survived_id_z <- as.data.frame(survived_id_z)
    survived_id_z$steamid <- as.character(survived_id_z$steamid)
    
    if (z == 1) {
      
      survived_id_list <- survived_id_z
      
    } else {
      
      survived_id_z <- survived_id_z[!(survived_id_z$steamid %in% survived_id_list$steamid), ]
      survived_id_z <- as.data.frame(survived_id_z)
      names(survived_id_z) <- c("steamid")
      survived_id_z$steamid <- as.character(survived_id_z$steamid)
      id_list <- list(survived_id_list, survived_id_z)
      survived_id_list <- rbindlist(id_list)
    }
    
    surv_id <- unlist(strsplit(as.character(survived_id_z[, 1]), ", "))
    
    system.time(
      
      for (i in 1:NROW(file_list)) {
        
        setwd(folder_list[z])
        cat(result_file[i], "-> start.", "\n")
        
        data <- read_csv(file_list[i], col_types = cols(
          steamid = col_character(),
          appid = col_character(),
          playtime_forever = col_double(),
          playtime_2weeks = col_double(),
          recordtime = col_datetime(format = "")
        ))
        
        data_surv <- c()
        data_surv <- as.data.frame(data_surv)
        
        df <- list()
        for (j in 1:NROW(surv_id)) {
          df[[j]] <- data[which(data$steamid == surv_id[j]), ]
          cat(j, " / ", NROW(surv_id),  "\n")
        }
        data_surv <- rbindlist(df)
        data_surv <- as.data.table(data_surv)
        setkey(data_surv, appid)
        data_tagged <- data_surv[app_genre, on = "appid", nomatch = 0]
        data_tagged <- as.data.frame(data_tagged)
        data_tagged <- arrange(data_tagged, steamid, -playtime_forever)
        ##### 이부분에서 app정보가 없는 게임만 가지고 있는 id는 삭제되기 때문에 survival_id_z랑 달라짐
        
        data_tagged_spread <- spread(data_tagged, tag, playtime_2weeks)
        names(data_tagged_spread)[names(data_tagged_spread) == "Action"] <- ("Action_2W")
        names(data_tagged_spread)[names(data_tagged_spread) == "Adventure"] <- ("Adventure_2W")
        names(data_tagged_spread)[names(data_tagged_spread) == "Puzzle"] <- ("Puzzle_2W")
        names(data_tagged_spread)[names(data_tagged_spread) == "Racing"] <- ("Racing_2W")
        names(data_tagged_spread)[names(data_tagged_spread) == "Rhythm"] <- ("Rhythm_2W")
        names(data_tagged_spread)[names(data_tagged_spread) == "RPG"] <- ("RPG_2W")
        names(data_tagged_spread)[names(data_tagged_spread) == "Shooter"] <- ("Shooter_2W")
        names(data_tagged_spread)[names(data_tagged_spread) == "Simulation"] <- ("Simulation_2W")
        names(data_tagged_spread)[names(data_tagged_spread) == "Sports"] <- ("Sports_2W")
        names(data_tagged_spread)[names(data_tagged_spread) == "Strategy"] <- ("Strategy_2W")
        
        data_tagged_spread <- as.data.table(data_tagged_spread)
        setkey(data_tagged_spread, appid)
        data_tagged_spread_2 <- data_tagged_spread[app_genre, nomatch = 0]
        data_tagged_spread_2 <- as.data.frame(data_tagged_spread_2)
        data_tagged_spread_2 <- spread(data_tagged_spread_2, tag, playtime_forever)
        data_tagged_spread_2[is.na(data_tagged_spread_2)] <- 0
        data_tagged_spread_2 <- data_tagged_spread_2[, -15]
        
        data_tagged_spread_2 <- arrange.vars(data_tagged_spread_2, c("steamid" = 1))
        data_tagged_spread_2 <- arrange.vars(data_tagged_spread_2, c("Action" = 5))
        data_tagged_spread_2 <- arrange.vars(data_tagged_spread_2, c("Adventure" = 6))
        data_tagged_spread_2 <- arrange.vars(data_tagged_spread_2, c("Puzzle" = 7))
        data_tagged_spread_2 <- arrange.vars(data_tagged_spread_2, c("Racing" = 8))
        data_tagged_spread_2 <- arrange.vars(data_tagged_spread_2, c("Rhythm" = 9))
        data_tagged_spread_2 <- arrange.vars(data_tagged_spread_2, c("RPG" = 10))
        data_tagged_spread_2 <- arrange.vars(data_tagged_spread_2, c("Shooter" = 11))
        data_tagged_spread_2 <- arrange.vars(data_tagged_spread_2, c("Simulation" = 12))
        data_tagged_spread_2 <- arrange.vars(data_tagged_spread_2, c("Sports" = 13))
        data_tagged_spread_2 <- arrange.vars(data_tagged_spread_2, c("Strategy" = 14))
        
        data_tagged_spread_2 <- arrange(data_tagged_spread_2, steamid, appid)
        
        genre_len <- names(data_tagged_spread_2[4:13])
        genre_len2 <- names(data_tagged_spread_2[14:23])
        steamid <- c()
        steamid <- unique(data_tagged_spread_2$steamid)
        steamid <- data.frame(steamid)
        genre_pt <- steamid
        genre_pt$steamid <- as.character(genre_pt$steamid)
        
        system.time(for (j in 1:NROW(genre_len)) {
          gen <- aggregate(data_tagged_spread_2[ , genre_len[j]], by = list(data_tagged_spread_2$steamid), sum)
          names(gen) <- c("steamid", genre_len[j])
          genre_pt <- merge(genre_pt, gen, by = "steamid")
        })
        
        system.time(for (j in 1:NROW(genre_len2)) {
          gen <- aggregate(data_tagged_spread_2[ , genre_len2[j]], by = list(data_tagged_spread_2$steamid), sum)
          names(gen) <- c("steamid", genre_len2[j])
          genre_pt <- merge(genre_pt, gen, by = "steamid")
        })
        
        genre_pt$date <- date[i]
        genre_pt <- arrange.vars(genre_pt, c("date" = 2))
        
        setwd(save_directory[z])
        write.csv(genre_pt, result_file[i], row.names = FALSE)
        
        cat(result_file[i], "-> done.", "\n")
      }
      
    )
    
    setwd(save_directory[z])
    
    d_0403 <- read_csv(result_file[25], col_types = cols(steamid = col_character()))
    d_0404 <- read_csv(result_file[26], col_types = cols(steamid = col_character()))
    d_0405 <- read_csv(result_file[27], col_types = cols(steamid = col_character()))
    
    for (i in 1:NROW(d_0404)) {
      if (d_0404[i,3] == 0 & d_0404[i,4] == 0 & d_0404[i,5] == 0 &
          d_0404[i,6] == 0 & d_0404[i,7] == 0 & d_0404[i,8] == 0 & 
          d_0404[i,9] == 0 & d_0404[i,10] == 0 & d_0404[i,11] == 0 & d_0404[i,12] == 0) {
        d_0404[i,c(3:22)] <- round((d_0403[i,c(3:22)]+d_0405[i,c(3:22)])/2)
      }
      cat(i, "/", NROW(d_0404), "\n")
    }
    write.csv(d_0404, result_file[26], row.names = FALSE)
    
    cat(as.character(ymd("2018-02-12") + z), "is done.", "\n")
  }
  
)

setwd("D:/Steam_Project/steam_data")
write.csv(survived_id_list, "survival_id_unique.csv", row.names = FALSE)

####################################################################################################################
##### 행 갯수 확인하는 데이터프레임 생성 ~9 10~
row_check_list <- list()
row_check <- c()
folder_date <- c()
for (i in 1:NROW(save_directory)) {
  d <- ymd("2018-02-12") + i
  folder_date[i] <- as_date(d)
}
folder_date <- as_date(folder_date)

for (i in 1:NROW(save_directory)) {
  setwd(save_directory[i])
  for (j in 1:NROW(result_file)){
    row_check[j] <- as.character(NROW(read_csv(result_file[j], col_types = cols(steamid = col_character()))))
  }
  row_check_list[[i]] <- row_check
  cat(i, "/", NROW(save_directory), "is done", "\n")
}
row_check_list <- as.data.frame(row_check_list)
names(row_check_list) <- c(folder_date[1:NROW(save_directory)])
dimnames(row_check_list) <- list(date, col=names(row_check_list))

setwd("D:/Steam_Project/steam_data")
write.csv(row_check_list, "number_of_id.csv")
row_check_list <- read_csv("number_of_id.csv")
row_check_list <- row_check_list_0[,-1]

####################################################################################################################
##### 이상치 제거(수동으로 찾음)

not_uniq_list <- c()
for (i in 1:NROW(save_directory)) {
  if (NROW(unique(row_check_list[ , i])) != 1){
    not_uniq_list[i] <- as.character(folder_date[i])
  } else {
    not_uniq_list[i] <- 0
  }
}
not_uniq_list #2, 6, 11, 13, 15, 16, 17, 19(2개있음), 22

setwd(save_directory[19])
d_1 <- read_csv(result_file[1], col_types = cols(steamid = col_character()))
d_2 <- read_csv(result_file[28], col_types = cols(steamid = col_character()))
arranged_0 <- d_2[!(d_2$steamid %in% d_1$steamid), ]
arranged_id <- as.vector(arranged_0[1,1])
# d_2_a <- d_2[!(d_2$steamid %in% arranged_id$steamid), ]

setwd(save_directory[19])
for (i in 1:NROW(result_file)) {
  before <- read_csv(result_file[i], col_types = cols(steamid = col_character()))
  after <- before[!(before$steamid %in% arranged_id$steamid), ]
  write.csv(after, result_file[i], row.names = FALSE)
  cat(i, "/", NROW(result_file), "is done", "\n")
}

####################################################################################################################
##### 이제 합치기
steam_panel_data <- list()
for (i in 1:NROW(save_directory)) {
  setwd(save_directory[i])
  if (i == 1) {
    for (j in 1:NROW(result_file)) {
      steam_panel_data[[j]] <- read_csv(result_file[j], col_types = cols(steamid = col_character()))
    }
  } else{
    for (j in 1:NROW(result_file)) {
      steam_panel_data[[j + (31*(i-1))]] <- read_csv(result_file[j], col_types = cols(steamid = col_character()))
    }
  }
  cat(i, "/", NROW(save_directory), "is done", "\n")
}
steam_panel_data_1 <- rbindlist(steam_panel_data)
steam_panel_data_1 <- arrange(steam_panel_data_1, date)

##### uno, sero
steam_panel_data_1$uno <- c(1)
steam_panel_data_1$sero <- c(0)

steam_panel_data_1 <- arrange.vars(steam_panel_data_1, c("uno" = 2))
steam_panel_data_1 <- arrange.vars(steam_panel_data_1, c("sero" = 3))
steam_panel_data_1 <- arrange.vars(steam_panel_data_1, c("date" = 24))

##### id숫자가 너무 크니까 새로운 id 생성
id <- unique(steam_panel_data_1$steamid)
NROW(unique(steam_panel_data_1$steamid))
id <- as.data.frame(id)
names(id) <- ("steamid")
id$id <- seq(1, NROW(id))

steam_panel_data_1_1 <- merge(steam_panel_data_1, id, by = "steamid")
steam_panel_data_1_1 <- arrange.vars(steam_panel_data_1_1, c("id" = 2))
steam_panel_data_1_1 <- arrange(steam_panel_data_1_1, id, date)

##### 0404 평균처리 안된 id 76561198814649461
# steam_panel_data_1_1 <- read_csv("playtime_panel_data.csv", col_types = cols(steamid = col_character()))
which(steam_panel_data_1_1$steamid == "76561198814649461")
steam_panel_data_1_1[558584,25]
steam_panel_data_1_1[558584,c(5:24)] <- round((steam_panel_data_1_1[558583,c(5:24)]+steam_panel_data_1_1[558585,c(5:24)])/2)


##### 2주 데이터 없앤 df
steam_panel_data_2 <- steam_panel_data_1_1[ , -c(15:24)]


setwd("D:/Steam_Project/steam_data")
system.time(write.csv(steam_panel_data_1_1, "playtime_panel_data.csv", row.names = FALSE))
system.time(write.csv(steam_panel_data_2, "playtime_panel_data_no2W.csv", row.names = FALSE))

##### STATA에서 차분한 후, 0 이하로 나오는 플레이타임 전처리
diff_data <- read_csv("diff_panel_data.csv", col_types = cols(steamid = col_character()))
diff_data$steamid <- gsub('"', '', diff_data$steamid)
diff_data <- diff_data[diff_data$date != as_date(ymd("2018-03-10")), ]
diff_data <- arrange.vars(diff_data, c("date" = 25))

system.time(write.csv(diff_data, "diff_panel_data.csv", row.names = FALSE))
# diff_data <- read_csv("diff_panel_data.csv", col_types = cols(steamid = col_character(), id = col_character()))

under_zero <- diff_data[diff_data$action_D < 0 | diff_data$adventure_D < 0 | diff_data$puzzle_D < 0 |
                          diff_data$racing_D < 0 | diff_data$rhythm_D < 0 | diff_data$rpg_D < 0 |
                          diff_data$shooter_D < 0 | diff_data$simulation_D < 0 | diff_data$sports_D < 0 |
                          diff_data$strategy_D < 0, ]
under_z_id <- unique(under_zero$steamid)
diff_data_over0 <- diff_data[!(diff_data$steamid %in% under_z_id), ]
system.time(write.csv(diff_data_over0, "steam_diff_data.csv", row.names = FALSE))

##### 너무 많은 이상치 제거
diff_data_over0 <- read_csv("steam_diff_data.csv", col_types = cols(steamid = col_character(), id = col_character()))
data_1440 <- filter(diff_data_over0, 
                    (action_D + adventure_D + puzzle_D + racing_D + rhythm_D + rpg_D + 
                       shooter_D + simulation_D + sports_D + strategy_D) > 1440)
over_1440_id <- unique(data_1440$steamid)
diff_data_0to1440 <- diff_data_over0[!(diff_data_over0$steamid %in% over_1440_id), ]

##### 기간동안 아예 게임을 하지 않은 유저 제거
afk <- diff_data_0to1440[, -25]
afk <- unique(afk)
afk <- afk[ , 1]
afk_id <- unique(afk$steamid)
NROW(afk[afk$steamid %in% afk_id[2], ])
remove(over_1440_id)

zzz_id <- c()
system.time(
  for (i in 1:NROW(afk_id)) {
    if (NROW(afk[afk$steamid %in% afk_id[i], ]) == 1) {
      zzz_id[i] <- afk_id[i]
    } else {
      zzz_id[i] <- NA
    }
    cat(i, "/", NROW(afk_id), "\n")
  }
)
zzz_id <- zzz_id[!is.na(zzz_id)] # 이 아이디들이 기간동안 게임 안한 유저들
diff_data_cleansed <- diff_data_0to1440[!(diff_data_0to1440$steamid %in% zzz_id), ]
write.csv(diff_data_cleansed, "steam_data_fin.csv", row.names = FALSE)

##### 시계열 안따지고 agrregate 한 데이터
diff_data_cleansed <- read_csv("steam_data_fin.csv", col_types = cols(steamid = col_character(), id = col_character()))

data_id <- diff_data_cleansed[diff_data_cleansed$date == ymd("2018-04-09"),]
data_id <- data_id[, -c(15:25)]
diff_data_cleansed <- diff_data_cleansed[, -25]
genre_len <- names(diff_data_cleansed[15:24])
diff_data_cleansed_noTS <- data_id

system.time(for (j in 1:NROW(genre_len)) {
  agg_sum <- aggregate(diff_data_cleansed[ , genre_len[j]], by = list(diff_data_cleansed$steamid), sum)
  names(agg_sum) <- c("steamid", genre_len[j])
  diff_data_cleansed_noTS <- merge(diff_data_cleansed_noTS, agg_sum, by = "steamid")
})
write.csv(diff_data_cleansed_noTS, "steam_data_fin_noTS.csv", row.names = FALSE)
##### 평균
system.time(for (j in 1:NROW(genre_len)) {
  agg_sum <- aggregate(diff_data_cleansed[ , genre_len[j]], by = list(diff_data_cleansed$steamid), mean)
  names(agg_sum) <- c("steamid", genre_len[j])
  diff_data_cleansed_noTS <- merge(diff_data_cleansed_noTS, agg_sum, by = "steamid")
})
write.csv(diff_data_cleansed_noTS, "steam_data_fin_noTS_avg.csv", row.names = FALSE)


##### sample file #####
diff_data_id <- unique(diff_data_cleansed$steamid)
diff_data_id <- as.data.frame(diff_data_id)
names(diff_data_id) <- ("steamid")
diff_data_id$steamid <- as.character(diff_data_id$steamid)
set.seed(123)
diff_data_id <- diff_data_id[sample(NROW(diff_data_id)), ]
diff_data_id <- diff_data_id[c(1:100)]
data_sample <- diff_data_cleansed[diff_data_cleansed$steamid %in% diff_data_id, ]
write.csv(data_sample, "steamdata_sample.csv", row.names = FALSE)

####################################################################################################################
####################################################################################################################
####################################                                            ####################################
###################################  #####      ####    ##   ##  #######  ###  #####################################
##################################  ##  ##    ##  ##   ###  ##  ##       ###  ######################################
#################################  ##   ##  ##    ##  #### ##  ##       ###  #######################################
################################  ##   ##  ##    ##  #######  #######   #   ########################################
###############################  ##   ##  ##    ##  ## ####  ##            #########################################
##############################  ##  ##    ##  ##   ##  ###  ##        #   ##########################################
#############################  #####      ####    ##   ##  #######  ###  ###########################################
############################                                            ############################################
####################################################################################################################
####################################################################################################################


