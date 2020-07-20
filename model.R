setwd('D:/JISUNG/git/prediction-of-movie-audience-numbers')

################################################
#install.packages('dplyr')
library(dplyr)
#install.packages('outliers')
library(outliers)
#install.packages("stringr")
library(stringr)
#install.packages('plyr')
library(plyr)
#install.packages('caret')
library(caret)
#install.packages('randomForest')
library(randomForest)
#install.packages('xgboost')
library(xgboost)
#install.packages('car')
library(car)
#install.packages('irr')
library(irr)
options(scipen=100)
################################################


movie <- read.csv('movie.csv', header=T, stringsAsFactors=T, na.strings=c('', NA), fileEncoding='UTF-8')
head(movie)
summary(movie)
str(movie)


################################################ 전처리

# 결측치 확인
colSums(is.na(movie))
# audience 결측치 제거
movie = movie %>% filter(!is.na(audience))
# release_date 결측치 제거
movie = movie %>% filter(!is.na(release_date))


# audience 10만 이상
#hist(movie$audience)
#movie <- movie[movie$audience > 100000, ]
#hist(movie$audience)

# audience 로그 변환
hist(movie$audience)
movie$audience <- log(movie$audience, base=10)
hist(movie$audience)
movie <- movie[!movie$audience < 0.5, ]
hist(movie$audience)


# release_date 날짜 데이터로 변경
movie$release_date <- as.Date(movie$release_date)
# release_date가 한달 이내인 영화 제외(현재 상영중)
movie <- movie[movie$release_date < Sys.Date() - 30, ]
# release_date 2000년 이전 영화 제외
movie_order <- order(movie$release_date)
movie <- movie[movie_order, ][movie[movie_order, ]$release_date > as.Date('2000-01-01'), ]


# country one-hot encoding
# 영화 수가 100개 미만이면 기타로 변경
for(i in 1: length(movie$movie_id)) {
  if(movie$country[i] %in% c('한국', '미국')) {
    movie$country_e[i] <- as.character(movie$country[i])
  } else {
    movie$country_e[i] <- '기타'
  }
}
movie$country_e <- as.factor(movie$country_e)

for(i in 1: length(movie$movie_id)) {
  if(movie$country[i] == '한국') {
    movie$country_ko[i] <- 1
    movie$country_us[i] <- 0
    movie$country_etc[i] <- 0
  } else if(movie$country[i] == '미국') {
    movie$country_ko[i] <- 0
    movie$country_us[i] <- 1
    movie$country_etc[i] <- 0
  }else {
    movie$country_ko[i] <- 0
    movie$country_us[i] <- 0
    movie$country_etc[i] <- 1
  }
}


# director 평균 관객 수
movie$director_m <- 0
for(i in movie$movie_id) {
  temp <- movie[!is.na(movie$director), ][movie[!is.na(movie$director), ]$director == movie[movie$movie_id == i, ]$director, ]
  movie[movie$movie_id == i, ]$director_m <- mean(temp[temp$release_date < movie[movie$movie_id == i, ]$release_date, ]$audience)
}
#movie$director_m[is.na(movie$director_m)] <- mean(movie$director_m, na.rm=T)
#movie$director_m[is.na(movie$director_m)] <- median(movie$director_m, na.rm=T)
movie$director_m[is.na(movie$director_m)] <- 0


# producer 평균 관객 수
movie$producer_m <- 0
for(i in movie$movie_id) {
  temp <- movie[!is.na(movie$producer), ][movie[!is.na(movie$producer), ]$producer == movie[movie$movie_id == i, ]$producer, ]
  movie[movie$movie_id == i, ]$producer_m <- mean(temp[temp$release_date < movie[movie$movie_id == i, ]$release_date, ]$audience)
}
#movie$producer_m[is.na(movie$producer_m)] <- mean(movie$producer_m, na.rm=T)
#movie$producer_m[is.na(movie$producer_m)] <- median(movie$producer_m, na.rm=T)
movie$producer_m[is.na(movie$producer_m)] <- 0


# actor1 평균 관객 수
movie$actor1_m <- 0
for(i in movie$movie_id) {
  temp1 <- movie[!is.na(movie$actor1), ][as.character(movie[!is.na(movie$actor1), ]$actor1) == as.character(movie[movie$movie_id == i, ]$actor1), ]
  temp2 <- movie[!is.na(movie$actor2), ][as.character(movie[!is.na(movie$actor2), ]$actor2) == as.character(movie[movie$movie_id == i, ]$actor1), ]
  temp3 <- movie[!is.na(movie$actor3), ][as.character(movie[!is.na(movie$actor3), ]$actor3) == as.character(movie[movie$movie_id == i, ]$actor1), ]
  temp1_m <- temp1[temp1$release_date < movie[movie$movie_id == i, ]$release_date, ]$audience
  temp2_m <- temp2[temp2$release_date < movie[movie$movie_id == i, ]$release_date, ]$audience
  temp3_m <- temp3[temp3$release_date < movie[movie$movie_id == i, ]$release_date, ]$audience
  movie[movie$movie_id == i, ]$actor1_m <- mean(c(temp1_m, temp2_m, temp3_m))
}
#movie$actor1_m[is.na(movie$actor1_m)] <- mean(movie$actor1_m, na.rm=T)
#movie$actor1_m[is.na(movie$actor1_m)] <- median(movie$actor1_m, na.rm=T)
movie$actor1_m[is.na(movie$actor1_m)] <- 0

# actor2 평균 관객 수
movie$actor2_m <- 0
for(i in movie$movie_id) {
  temp1 <- movie[!is.na(movie$actor1), ][as.character(movie[!is.na(movie$actor1), ]$actor1) == as.character(movie[movie$movie_id == i, ]$actor2), ]
  temp2 <- movie[!is.na(movie$actor2), ][as.character(movie[!is.na(movie$actor2), ]$actor2) == as.character(movie[movie$movie_id == i, ]$actor2), ]
  temp3 <- movie[!is.na(movie$actor3), ][as.character(movie[!is.na(movie$actor3), ]$actor3) == as.character(movie[movie$movie_id == i, ]$actor2), ]
  temp1_m <- temp1[temp1$release_date < movie[movie$movie_id == i, ]$release_date, ]$audience
  temp2_m <- temp2[temp2$release_date < movie[movie$movie_id == i, ]$release_date, ]$audience
  temp3_m <- temp3[temp3$release_date < movie[movie$movie_id == i, ]$release_date, ]$audience
  movie[movie$movie_id == i, ]$actor2_m <- mean(c(temp1_m, temp2_m, temp3_m))
}
#movie$actor2_m[is.na(movie$actor2_m)] <- mean(movie$actor2_m, na.rm=T)
#movie$actor2_m[is.na(movie$actor2_m)] <- median(movie$actor2_m, na.rm=T)
movie$actor2_m[is.na(movie$actor2_m)] <- 0

# actor3 평균 관객 수
movie$actor3_m <- 0
for(i in movie$movie_id) {
  temp1 <- movie[!is.na(movie$actor1), ][as.character(movie[!is.na(movie$actor1), ]$actor1) == as.character(movie[movie$movie_id == i, ]$actor3), ]
  temp2 <- movie[!is.na(movie$actor2), ][as.character(movie[!is.na(movie$actor2), ]$actor2) == as.character(movie[movie$movie_id == i, ]$actor3), ]
  temp3 <- movie[!is.na(movie$actor3), ][as.character(movie[!is.na(movie$actor3), ]$actor3) == as.character(movie[movie$movie_id == i, ]$actor3), ]
  temp1_m <- temp1[temp1$release_date < movie[movie$movie_id == i, ]$release_date, ]$audience
  temp2_m <- temp2[temp2$release_date < movie[movie$movie_id == i, ]$release_date, ]$audience
  temp3_m <- temp3[temp3$release_date < movie[movie$movie_id == i, ]$release_date, ]$audience
  movie[movie$movie_id == i, ]$actor3_m <- mean(c(temp1_m, temp2_m, temp3_m))
}
#movie$actor3_m[is.na(movie$actor3_m)] <- mean(movie$actor3_m, na.rm=T)
#movie$actor3_m[is.na(movie$actor3_m)] <- median(movie$actor3_m, na.rm=T)
movie$actor3_m[is.na(movie$actor3_m)] <- 0

# actor_all 주연 배우들의 평균 관객수 합
movie$actor_all <- movie$actor1_m + movie$actor2_m + movie$actor3_m


# genre 성인 영화 제외
movie <- movie[!grepl('성인물', movie$genre), ]
# genre 드라마|멜로/로맨스|액션|기타로 분리
# genre one-hot encoding
unique(movie$genre)
genre_split <- strsplit(as.character(movie$genre), '\\|')
for(i in 1:length(movie$movie_id)) {
  if(genre_split[[i]][1] %in% c('드라마', '멜로/로맨스', '액션')) {
    movie$genre_f[i] <- genre_split[[i]][1]
  } else {
    movie$genre_f[i] <- '기타'
  }
}
movie$genre_f <- as.factor(movie$genre_f)

for(i in 1:length(movie$movie_id)) {
  if(genre_split[[i]][1] == '드라마') {
    movie$genre_drama[i] <- 1
    movie$genre_romance[i] <- 0
    movie$genre_action[i] <- 0
    movie$genre_etc[i] <- 0
  } else if(genre_split[[i]][1] == '멜로/로맨스') {
    movie$genre_drama[i] <- 0
    movie$genre_romance[i] <- 1
    movie$genre_action[i] <- 0
    movie$genre_etc[i] <- 0
  } else if(genre_split[[i]][1] == '액션') {
    movie$genre_drama[i] <- 0
    movie$genre_romance[i] <- 0
    movie$genre_action[i] <- 1
    movie$genre_etc[i] <- 0
  } else {
    movie$genre_drama[i] <- 0
    movie$genre_romance[i] <- 0
    movie$genre_action[i] <- 0
    movie$genre_etc[i] <- 1
  }
}
movie$

# rating one-hot encoding
# 연소자관람가, 모든 관람객이 관람할 수 있는 등급, 미성년자관람가 -> 전체관람가
# 12세이상관람가, 중학생이상관람가, 국민학생관람불가, 12세 미만인 자는 관람할 수 없는 등급 -> 12세관람가
# 15세이상관람가, 고등학생이상관람가, 15세 미만인 자는 관람할 수 없는 등급 -> 15세관람가
# 18세관람가, 18세 미만인 자는 관람할 수 없는 등급, 연소자관람불가, 미성년자관람불가 -> 청소년관람불가
unique(movie$rating)
movie$rating <- gsub('(연소자관람가|모든 관람객이 관람할 수 있는 등급|미성년자관람가)', '전체관람가', movie$rating)
movie$rating <- gsub('(12세이상관람가|중학생이상관람가|국민학생관람불가|12세 미만인 자는 관람할 수 없는 등급)', '12세관람가', movie$rating)
movie$rating <- gsub('(15세이상관람가|고등학생이상관람가|15세 미만인 자는 관람할 수 없는 등급)', '15세관람가', movie$rating)
movie$rating <- gsub('(18세관람가|18세 미만인 자는 관람할 수 없는 등급|연소자관람불가|미성년자관람불가)', '청소년관람불가', movie$rating)
unique(movie$rating)
movie$rating <- as.factor(movie$rating)

for(i in 1: length(movie$movie_id)) {
  if(movie$rating[i] == '전체관람가') {
    movie$rating_all[i] <- 1
    movie$rating_12[i] <- 0
    movie$rating_15[i] <- 0
    movie$rating_adult[i] <- 0
  } else if(movie$rating[i] == '12세관람가') {
    movie$rating_all[i] <- 0
    movie$rating_12[i] <- 1
    movie$rating_15[i] <- 0
    movie$rating_adult[i] <- 0
  } else if(movie$rating[i] == '15세관람가') {
    movie$rating_all[i] <- 0
    movie$rating_12[i] <- 0
    movie$rating_15[i] <- 1
    movie$rating_adult[i] <- 0
  }else {
    movie$rating_all[i] <- 0
    movie$rating_12[i] <- 0
    movie$rating_15[i] <- 0
    movie$rating_adult[i] <- 1
  }
}


################################################ XGBoost
movie_train <- movie[1:8972, ]
movie_test <- movie[8973:9969, ]

mat_train <- model.matrix(
  audience~
  country_ko+country_us+country_etc+director_m+producer_m+actor_all
  +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
  +rating_all+rating_12+rating_15+rating_adult,
  movie_train
)
mat_test <- model.matrix(
  audience~
    country_ko+country_us+country_etc+director_m+producer_m+actor_all
  +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
  +rating_all+rating_12+rating_15+rating_adult,
  movie_test
)

model_xg <- xgb.train(
  verbose=0,
  eta=0.025,
  booster='gbtree',
  max_depth=3,
  nround=2500,
  eval_metric='rmse',
  data=xgb.DMatrix(mat_train, label=movie_train$audience)
)
summary(model_xg)

result_xg <- predict(model_xg, mat_test)
cbind(result_xg, movie_test$audience)

# 오차율 937426.7
RMSE(10^result_xg, 10^movie_test$audience)
################################################


################################################ XGBoost / k-fold
set.seed(1024)
random_idx <- order(runif(8972))
movie_train <- movie[random_idx[1:6729], ]
movie_validate <- movie[random_idx[6730:8972], ]
movie_test <- movie[8973:9969, ]

folds <- createFolds(movie$audience, k=10)
str(folds)

cv_results <- lapply(folds, function(x) {
  temp_train <- movie[-x, ]
  temp_test <- movie[x, ]
  
  temp_mat_train <- model.matrix(
    audience~
      country_ko+country_us+country_etc+director_m+producer_m+actor_all
    +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
    +rating_all+rating_12+rating_15+rating_adult,
    temp_train
  )
  temp_mat_test <- model.matrix(
    audience~
      country_ko+country_us+country_etc+director_m+producer_m+actor_all
    +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
    +rating_all+rating_12+rating_15+rating_adult,
    temp_test
  )
  
  model_fold <- xgb.train(
    verbose=0,
    eta=0.025,
    booster='gbtree',
    max_depth=3,
    nround=2500,
    eval_metric='rmse',
    data=xgb.DMatrix(temp_mat_train, label=temp_train$audience)
  )
  
  result_fold <- predict(model_fold, temp_mat_test)
  fold_rmse <- RMSE(10^result_fold, 10^temp_test$audience)
  return(fold_rmse)
})

# 오차율 869847.8
sqrt(sum(as.data.frame(cv_results)^2)/10)
################################################


################################################ XGBoost / k-fold / holiday
set.seed(1024)
random_idx <- order(runif(8972))
movie_train <- movie[random_idx[1:6729], ]
movie_validate <- movie[random_idx[6730:8972], ]
movie_test <- movie[8973:9969, ]

folds <- createFolds(movie$audience, k=10)
str(folds)

cv_results <- lapply(folds, function(x) {
  temp_train <- movie[-x, ]
  temp_test <- movie[x, ]
  
  temp_mat_train <- model.matrix(
    audience~
      country_ko+country_us+country_etc+director_m+producer_m+actor_all
    +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
    +rating_all+rating_12+rating_15+rating_adult+holiday,
    temp_train
  )
  temp_mat_test <- model.matrix(
    audience~
      country_ko+country_us+country_etc+director_m+producer_m+actor_all
    +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
    +rating_all+rating_12+rating_15+rating_adult+holiday,
    temp_test
  )
  
  model_fold <- xgb.train(
    verbose=0,
    eta=0.025,
    booster='gbtree',
    max_depth=3,
    nround=2500,
    eval_metric='rmse',
    data=xgb.DMatrix(temp_mat_train, label=temp_train$audience)
  )
  
  result_fold <- predict(model_fold, temp_mat_test)
  fold_rmse <- RMSE(10^result_fold, 10^temp_test$audience)
  return(fold_rmse)
})

# 오차율 859288.8
sqrt(sum(as.data.frame(cv_results)^2)/10)
################################################


################################################ XGBoost / k-fold / series
series_split <- strsplit(as.character(movie$series), ',')
for(i in 1:length(movie$movie_id)) {
  empty_vec <- vector()
  if(!is.na(series_split[[i]][1])) {
    for(j in 1:length(series_split[[i]])) {
      if(series_split[[i]][j] %in% movie$title) {
        if(movie$release_date[i] > movie[movie$title == series_split[[i]][j], ]$release_date) {
          empty_vec <- c(empty_vec, movie[movie$title == series_split[[i]][j], ]$audience)
        }
      }
    }
    if(length(empty_vec) == 0) {
      movie$series_m[i] <- NA
    } else {
      movie$series_m[i] <- mean(empty_vec)
    }
  } else {
    movie$series_m[i] <- NA
  }
}
#movie$series_m[is.na(movie$series_m)] <- mean(movie$series_m, na.rm=T)
#movie$series_m[is.na(movie$series_m)] <- median(movie$series_m, na.rm=T)
movie$series_m[is.na(movie$series_m)] <- 0

set.seed(1024)
random_idx <- order(runif(8972))
movie_train <- movie[random_idx[1:6729], ]
movie_validate <- movie[random_idx[6730:8972], ]
movie_test <- movie[8973:9969, ]

folds <- createFolds(movie$audience, k=10)
str(folds)

cv_results <- lapply(folds, function(x) {
  temp_train <- movie[-x, ]
  temp_test <- movie[x, ]
  
  temp_mat_train <- model.matrix(
    audience~
      country_ko+country_us+country_etc+director_m+producer_m+actor_all
    +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
    +rating_all+rating_12+rating_15+rating_adult+series_m,
    temp_train
  )
  temp_mat_test <- model.matrix(
    audience~
      country_ko+country_us+country_etc+director_m+producer_m+actor_all
    +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
    +rating_all+rating_12+rating_15+rating_adult+series_m,
    temp_test
  )
  
  model_fold <- xgb.train(
    verbose=0,
    eta=0.025,
    booster='gbtree',
    max_depth=3,
    nround=2500,
    eval_metric='rmse',
    data=xgb.DMatrix(temp_mat_train, label=temp_train$audience)
  )
  
  result_fold <- predict(model_fold, temp_mat_test)
  fold_rmse <- RMSE(10^result_fold, 10^temp_test$audience)
  return(fold_rmse)
})

# 오차율 877192
sqrt(sum(as.data.frame(cv_results)^2)/10)
################################################


################################################ XGBoost / k-fold / holiday / series
series_split <- strsplit(as.character(movie$series), ',')
for(i in 1:length(movie$movie_id)) {
  empty_vec <- vector()
  if(!is.na(series_split[[i]][1])) {
    for(j in 1:length(series_split[[i]])) {
      if(series_split[[i]][j] %in% movie$title) {
        if(movie$release_date[i] > movie[movie$title == series_split[[i]][j], ]$release_date) {
          empty_vec <- c(empty_vec, movie[movie$title == series_split[[i]][j], ]$audience)
        }
      }
    }
    if(length(empty_vec) == 0) {
      movie$series_m[i] <- NA
    } else {
      movie$series_m[i] <- mean(empty_vec)
    }
  } else {
    movie$series_m[i] <- NA
  }
}
#movie$series_m[is.na(movie$series_m)] <- mean(movie$series_m, na.rm=T)
#movie$series_m[is.na(movie$series_m)] <- median(movie$series_m, na.rm=T)
movie$series_m[is.na(movie$series_m)] <- 0

set.seed(1024)
random_idx <- order(runif(8972))
movie_train <- movie[random_idx[1:6729], ]
movie_validate <- movie[random_idx[6730:8972], ]
movie_test <- movie[8973:9969, ]

folds <- createFolds(movie$audience, k=10)
str(folds)

cv_results <- lapply(folds, function(x) {
  temp_train <- movie[-x, ]
  temp_test <- movie[x, ]
  
  temp_mat_train <- model.matrix(
    audience~
      country_ko+country_us+country_etc+director_m+producer_m+actor_all
    +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
    +rating_all+rating_12+rating_15+rating_adult+holiday+series_m,
    temp_train
  )
  temp_mat_test <- model.matrix(
    audience~
      country_ko+country_us+country_etc+director_m+producer_m+actor_all
    +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
    +rating_all+rating_12+rating_15+rating_adult+holiday+series_m,
    temp_test
  )
  
  model_fold <- xgb.train(
    verbose=0,
    eta=0.025,
    booster='gbtree',
    max_depth=3,
    nround=2500,
    eval_metric='rmse',
    data=xgb.DMatrix(temp_mat_train, label=temp_train$audience)
  )
  
  result_fold <- predict(model_fold, temp_mat_test)
  fold_rmse <- RMSE(10^result_fold, 10^temp_test$audience)
  return(fold_rmse)
})

# 오차율 870308.5
sqrt(sum(as.data.frame(cv_results)^2)/10)
################################################


################################################ XGBoost / k-fold / holiday / grid search
set.seed(1024)
random_idx <- order(runif(8972))
movie_train <- movie[random_idx[1:8972], ]

mat_train <- model.matrix(
  audience~
    country_ko+country_us+country_etc+director_m+producer_m+actor_all
  +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
  +rating_all+rating_12+rating_15+rating_adult,
  movie_train
)

search_grid_sub_col  <- expand.grid(
  subsample = c(1, 0.5),
  colsample_bytree = c(1, 0.5),
  max_depth = c(1, 3),
  min_child = seq(1),
  eta = c(0.01, 0.025)
)

system.time(
  rmse_errors_hyper_parameters <- apply(search_grid_sub_col, 1, function(parameter_list) {
    current_sub_sample_rate <- parameter_list[["subsample"]]
    current_col_sample_rate <- parameter_list[["colsample_bytree"]]
    current_depth <- parameter_list[["max_depth"]]
    current_eta <- parameter_list[["eta"]]
    current_min_child <- parameter_list[["min_child"]]
    
    model_xg_fold <- xgb.cv(
      data=xgb.DMatrix(mat_train, label=movie_train$audience),
      nrounds=2500, nfold=10, showsd=TRUE,
      metrics="rmse", verbose=TRUE, "eval_metric"="rmse",
      "objective"="reg:linear", "max.depth"=current_depth, "eta"=current_eta,                               
      "subsample"=current_sub_sample_rate, "colsample_bytree"=current_col_sample_rate,
      print_every_n=10, "min_child_weight"=current_min_child, booster="gbtree",
      early_stopping_rounds=0
    )
    
    x_validation_scores <- as.data.frame(model_xg_fold$evaluation_log)
    model_xg_fold$test_rmse_mean
    rmse <- tail(model_xg_fold$test_rmse_mean, 1)
    trmse <- tail(model_xg_fold$train_rmse_mean,1)
    output <- return(c(rmse, trmse, current_sub_sample_rate, current_col_sample_rate, current_depth, current_eta, current_min_child))
  })
)

output <- as.data.frame(t(rmse_errors_hyper_parameters))
varnames <- c("sub_samp_rate", "col_samp_rate", "depth", "eta", "current_min_child")
names(output) <- varnames
head(output)
#  sub_samp_rate col_samp_rate depth  eta current_min_child
#1           1.0           1.0     1 0.01                 1
#2           0.5           1.0     1 0.01                 1
#3           1.0           0.5     1 0.01                 1
#4           0.5           0.5     1 0.01                 1
#5           1.0           1.0     3 0.01                 1
#6           0.5           1.0     3 0.01                 1


set.seed(1024)
random_idx <- order(runif(8972))
movie_train <- movie[random_idx[1:6729], ]
movie_validate <- movie[random_idx[6730:8972], ]
movie_test <- movie[8973:9969, ]

folds <- createFolds(movie$audience, k=10)
str(folds)

cv_results <- lapply(folds, function(x) {
  temp_train <- movie[-x, ]
  temp_test <- movie[x, ]
  
  temp_mat_train <- model.matrix(
    audience~
      country_ko+country_us+country_etc+director_m+producer_m+actor_all
    +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
    +rating_all+rating_12+rating_15+rating_adult+holiday,
    temp_train
  )
  temp_mat_test <- model.matrix(
    audience~
      country_ko+country_us+country_etc+director_m+producer_m+actor_all
    +genre_drama+genre_romance+genre_action+genre_etc+screen+running_time
    +rating_all+rating_12+rating_15+rating_adult+holiday,
    temp_test
  )
  
  model_fold <- xgb.train(
    verbose=0,
    eta=0.01,
    booster='gbtree',
    max_depth=3,
    subsample=1,
    colsample_bytree=1,
    nround=2500,
    eval_metric='rmse',
    data=xgb.DMatrix(temp_mat_train, label=temp_train$audience)
  )
  
  result_fold <- predict(model_fold, temp_mat_test)
  fold_rmse <- RMSE(10^result_fold, 10^temp_test$audience)
  return(fold_rmse)
})

# 오차율 854425.7
sqrt(sum(as.data.frame(cv_results)^2)/10)
################################################