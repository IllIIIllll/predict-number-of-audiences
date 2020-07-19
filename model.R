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
options(scipen=100)
################################################

#movie <- read.csv('movie_test.csv', header=T, stringsAsFactors=T, na.strings=c('', NA))
movie <- read.csv('movie.csv', header=T, stringsAsFactors=T, na.strings=c('', NA))
head(movie)
summary(movie)
str(movie)

################################################ 전처리

# 결측치 확인
colSums(is.na(movie))
# movie_id 결측치 제거
movie = movie %>% filter(!is.na(movie_id))
# audience 결측치 제거
movie = movie %>% filter(!is.na(audience))
# release_date 결측치 제거
movie = movie %>% filter(!is.na(release_date))


# audience 10만 이상
hist(movie$audience)
movie <- movie[movie$audience > 1000000, ]
hist(movie$audience)
# audience 로그 변환
#hist(movie$audience)
#movie$audience <- log(movie$audience, base=10)
#hist(movie$audience)
#movie <- movie[!movie$audience < 0.5, ]
#hist(movie$audience)


# release_date 날짜 데이터로 변경
movie$release_date <- as.Date(movie$release_date)
# release_date가 한달 이내인 영화 제외(현재 상영중)
movie <- movie[movie$release_date < Sys.Date() - 30, ]


# country 변경
# 영화 수가 100개 미만이면 기타로 변경
for(i in 1: length(movie$movie_id)) {
  if(movie$country[i] %in% c('한국', '미국')) {
    movie$country_e[i] <- as.character(movie$country[i])
  } else {
    movie$country_e[i] <- '기타'
  }
}
movie$country_e <- as.factor(movie$country_e)


# director 평균 관객 수
for(i in 1:length(movie$movie_id)) {
  movie$director_m[i] <- mean(movie[movie$release_date < movie$release_date[i] & movie$director == movie$director[i], ]$audience, na.rm=T)
}
#movie$director_m[is.na(movie$director_m)] <- mean(movie$director_m, na.rm=T)
#movie$director_m[is.na(movie$director_m)] <- median(movie$director_m, na.rm=T)
movie$director_m[is.na(movie$director_m)] <- 0
head(movie$director_m)



# producer 평균 관객 수
for(i in 1:length(movie$movie_id)) {
  movie$producer_m[i] <- mean(movie[movie$release_date < movie$release_date[i] & movie$producer == movie$producer[i], ]$audience, na.rm=T)
}
#movie$producer_m[is.na(movie$producer_m)] <- mean(movie$producer_m, na.rm=T)
#movie$producer_m[is.na(movie$producer_m)] <- median(movie$producer_m, na.rm=T)
movie$producer_m[is.na(movie$producer_m)] <- 0
head(movie$producer_m)


# actor1 평균 관객 수
for(i in 1:length(movie$movie_id)) {
  actor <- movie$actor1
  movie$actor1_m[i] <- mean(movie[movie$release_date < movie$release_date[i] & (movie$actor1[i] == actor | movie$actor2[i] == actor | movie$actor3[i] == actor), ]$audience, na.rm=T)
}
#movie$actor1_m[is.na(movie$actor1_m)] <- mean(movie$actor1_m, na.rm=T)
#movie$actor1_m[is.na(movie$actor1_m)] <- median(movie$actor1_m, na.rm=T)
movie$actor1_m[is.na(movie$actor1_m)] <- 0
head(movie$actor1_m)
# actor2 평균 관객 수
for(i in 1:length(movie$movie_id)) {
  actor <- movie$actor2
  movie$actor2_m[i] <- mean(movie[movie$release_date < movie$release_date[i] & (movie$actor1[i] == actor | movie$actor2[i] == actor | movie$actor3[i] == actor), ]$audience, na.rm=T)
}
#movie$actor2_m[is.na(movie$actor2_m)] <- mean(movie$actor2_m, na.rm=T)
#movie$actor2_m[is.na(movie$actor2_m)] <- median(movie$actor2_m, na.rm=T)
movie$actor2_m[is.na(movie$actor2_m)] <- 0
head(movie$actor2_m)
# actor3 평균 관객 수
for(i in 1:length(movie$movie_id)) {
  actor <- movie$actor3
  movie$actor3_m[i] <- mean(movie[movie$release_date < movie$release_date[i] & (movie$actor1[i] == actor | movie$actor2[i] == actor | movie$actor3[i] == actor), ]$audience, na.rm=T)
}
#movie$actor3_m[is.na(movie$actor3_m)] <- mean(movie$actor3_m, na.rm=T)
#movie$actor3_m[is.na(movie$actor3_m)] <- median(movie$actor3_m, na.rm=T)
movie$actor3_m[is.na(movie$actor3_m)] <- 0
head(movie$actor3_m)


# genre 성인 영화 제외
movie <- movie[!grepl('성인물', movie$genre), ]
# genre 정렬
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
unique(movie$genre_f)
table(movie$genre_f)
as.character(genre_split[[1]][1])

# rating 변경
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


################################################
# 결정계수 확인 0.2231
set.seed(911101)
train_idx <- sample(1:nrow(movie), size=0.7*nrow(movie), replace=F)
test_idx <- (-train_idx)
movie_train <- movie[train_idx, ]
movie_test <- movie[test_idx, ]
model <- lm(audience~country_e+director_m+producer_m+actor1_m+actor2_m+actor3_m+genre_f+screen+running_time+rating, data=movie_train)
summary(model)

# 모델 성능 평가
result <- predict(model, movie_test)
cbind(result, movie_test$audience)

# 상관관계 확인 0.4125881
cor(result, movie_test$audience)

# 오차율 확인 0.3048356
RMSE(log(result, base=10), log(movie_test$audience, base=10))

# 다중공선성 확인
vif(model)
################################################


################################################ holiday 추가
# holiday
for(i in 1:length(movie$movie_id)) {
  movie$holiday_m[i] <- mean(movie[movie$holiday == movie$holiday[i], ]$audience, na.rm=T)
}

# 결정계수 확인 0.2247
set.seed(911101)
train_idx <- sample(1:nrow(movie), size=0.7*nrow(movie), replace=F)
test_idx <- (-train_idx)
movie_train <- movie[train_idx, ]
movie_test <- movie[test_idx, ]
model2 <- lm(audience~country_e+director_m+producer_m+actor1_m+actor2_m+actor3_m+genre_f+screen+running_time+rating+holiday_m, data=movie_train)
summary(model2)

# 모델 성능 평가
result2 <- predict(model2, movie_test)
cbind(result2, movie_test$audience)

# 상관관계 확인 0.4189987
cor(result2, movie_test$audience)

# 오차율 확인 0.2865084
RMSE(log(result2, base=10), log(movie_test$audience, base=10))

# 다중공선성 확인
vif(model2)
################################################


################################################ series 추가
# series
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


# 결정계수 확인 # 0.2357
set.seed(911101)
train_idx <- sample(1:nrow(movie), size=0.7*nrow(movie), replace=F)
test_idx <- (-train_idx)
movie_train <- movie[train_idx, ]
movie_test <- movie[test_idx, ]
model3 <- lm(audience~country_e+director_m+producer_m+actor1_m+actor2_m+actor3_m+genre_f+screen+running_time+rating+holiday_m+series_m, data=movie_train)
summary(model3)

# 모델 성능 평가
result3 <- predict(model3, movie_test)
cbind(result3, movie_test$audience)

# 상관관계 확인 0.4502496
cor(result3, movie_test$audience)

# 오차율 확인 0.2821156
RMSE(log(result3, base=10), log(movie_test$audience, base=10))

# 다중공선성 확인
vif(model3)
################################################


################################################ RF
# 회귀계수 확인
model_rf <- randomForest(audience~country_e+director_m+producer_m+actor1_m+actor2_m+actor3_m+genre_f+screen+running_time+rating, data=movie_train)
summary(model_rf)

result_rf <- predict(model_rf, movie_test)
cbind(result_rf, movie_test$audience)

# 상관관계 확인 0.4282098
cor(result_rf, movie_test$audience)

# 오차율 확인 0.2665464
RMSE(log(result_rf, base=10), log(movie_test$audience, base=10))
################################################


################################################ RF (holiday 추가)
# 회귀계수 확인
model2_rf <- randomForest(audience~country_e+director_m+producer_m+actor1_m+actor2_m+actor3_m+genre_f+screen+running_time+rating+holiday_m, data=movie_train)
summary(model2_rf)

result2_rf <- predict(model2_rf, movie_test)
cbind(result2_rf, movie_test$audience)

# 상관관계 확인 0.4267053
cor(result2_rf, movie_test$audience)

# 오차율 확인 0.2670851
RMSE(log(result2_rf, base=10), log(movie_test$audience, base=10))
################################################


################################################ RF (series 추가)
# 회귀계수 확인
model3_rf <- randomForest(audience~country_e+director_m+producer_m+actor1_m+actor2_m+actor3_m+genre_f+screen+running_time+rating+holiday_m+series_m, data=movie_train)
summary(model3_rf)

result3_rf <- predict(model3_rf, movie_test)
cbind(result3_rf, movie_test$audience)

# 상관관계 확인 0.4652451
cor(result3_rf, movie_test$audience)

# 오차율 확인 0.262455
RMSE(log(result3_rf, base=10), log(movie_test$audience, base=10))
################################################


################################################ XGBoost
xg <- c('country_e','director_m','producer_m','actor1_m','actor2_m','actor3_m','genre_f','screen','running_time','rating')
model_xg <- xgboost(
  data=data.matrix(movie_train[xg]),
  label=data.matrix(movie_train$audience),
  booster='gbtree',
  eta=0.025,
  depth=3,
  nrounds=2500,
  objective='reg:linear',
  eval_metric='rmse',
  verbose=0
)
summary(model_xg)

result_xg <- predict(model_xg, data.matrix(movie_test[xg]))
cbind(result_xg, movie_test$audience)

# 상관관계 확인 0.3714022
cor(result_xg, movie_test$audience)

# 오차율 확인 0.2860657
RMSE(log(result_xg, base=10), log(movie_test$audience, base=10))
################################################


################################################ XGBoost (holiday 추가)
xg2 <- c('country_e','director_m','producer_m','actor1_m','actor2_m','actor3_m','genre_f','screen','running_time','rating','holiday_m')
model2_xg <- xgboost(
  data=data.matrix(movie_train[xg2]),
  label=data.matrix(movie_train$audience),
  booster='gbtree',
  eta=0.025,
  depth=3,
  nrounds=2500,
  objective='reg:linear',
  eval_metric='rmse',
  verbose=0
)
summary(model2_xg)

result2_xg <- predict(model2_xg, data.matrix(movie_test[xg2]))
cbind(result2_xg, movie_test$audience)

# 상관관계 확인 0.3618663
cor(result2_xg, movie_test$audience)

# 오차율 확인 0.2886274
RMSE(log(result2_xg, base=10), log(movie_test$audience, base=10))
################################################


################################################ XGBoost (series 추가)
xg3 <- c('country_e','director_m','producer_m','actor1_m','actor2_m','actor3_m','genre_f','screen','running_time','rating','holiday_m','series_m')
model3_xg <- xgboost(
  data=data.matrix(movie_train[xg3]),
  label=data.matrix(movie_train$audience),
  booster='gbtree',
  eta=0.025,
  depth=3,
  nrounds=2500,
  objective='reg:linear',
  eval_metric='rmse',
  verbose=0
)
summary(model3_xg)

result3_xg <- predict(model3_xg, data.matrix(movie_test[xg3]))
cbind(result3_xg, movie_test$audience)

# 상관관계 확인 0.3734795
cor(result3_xg, movie_test$audience)

# 오차율 확인 0.279303
RMSE(log(result3_xg, base=10), log(movie_test$audience, base=10))
################################################