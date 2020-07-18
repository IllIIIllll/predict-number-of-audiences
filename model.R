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
################################################

#movie <- read.csv('movie_test.csv', header=T, stringsAsFactors=T, na.strings=c('', NA))
movie <- read.csv('movie_test.csv', header=T, stringsAsFactors=T, na.strings=c('해당정보없음', '', NA))
head(movie)
summary(movie)
str(movie)

###################### 전처리

# 결측치 확인
colSums(is.na(movie))
# audience 결측치 제거
movie = movie %>% filter(!is.na(audience))
# release_date 결측치 제거
movie = movie %>% filter(!is.na(release_date))


# 개봉일 날짜 데이터로 변경
movie$release_date <- as.Date(movie$release_date)


# 성인 영화 제외
movie <- movie[!grepl('성인물', movie$genre), ]


# 개봉일이 한달 이내인 영화 제외(현재 상영중)
movie <- movie[movie$release_date < Sys.Date() - 30, ]


# 상영 등급 변경
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


# audience 로그 변환
hist(movie$audience)
movie$audience <- log(movie$audience)
hist(movie$audience)
movie <- movie[!movie$audience < 1, ]
hist(movie$audience)


# genre 정렬
unique(movie$genre)
genre_split <- strsplit(as.character(movie$genre), '\\|')
for(i in 1:length(genre_split)) {
  movie$genre[i] <- paste(sort(unlist(genre_split[i])), collapse='|')
}
unique(movie$genre)


str(movie)
# 상관관계 분석
cor(movie[, c('audience', 'holiday')])




# 이상치 확인
grubbs.flag <- function(x) {
  outliers <- NULL
  test <- x
  grubbs.result <- grubbs.test(test)
  pv <- grubbs.result$p.value
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- x[!x %in% outliers]
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}
movie[grubbs.flag(movie$audience)]

