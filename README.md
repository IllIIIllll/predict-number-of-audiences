# Predict number of audiences

#### 1. 데이터 수집
- [KOBIS](http://www.kobis.or.kr/kobis/business/main/main.do)를 통해 영화 정보(제목, 관객수, 개봉일, 감독, 주연배우 등) 수집  
- [NAVER](https://www.naver.com/)를 통해 해당 영화의 시리즈 영화 제목과 영화 상영 기간의 휴일 수 수집

#### 2. 데이터 탐색(EDA)
- 결측치 : 관객수와 개봉일에 결측치가 존재하는 데이터 제거
- 정규화 : 왜도를 줄이기 위해 로그스케일로 정규화
- 이상치 : 관객수가 현저히 적은 영화와 성인영화(실제로 영화관에서 상영하지 않음)를 이상치로 판단하여 제거
- 장르 : 갯수가 많은 상위 3개 장르(드라마, 액션, 코미디)외에 나머지 장르는 기타로 변경
- 등급 : 영화 상영년도에 따라 등급명이 다른데 이를 4개 등급으로 변경
- 감독 평균 관객수 : 해당 감독이 이전에 제작한 영화들의 관객수 평균
- 배급사 평균 관객수 : 해당 배급사에서 이전에 배급한 영화들의 관객수 평균
- 주연 배우 평균 관객수 : 3명의 주연 배우에 대해 이전에 출연한 영화들의 관객수 평균의 합
- 시리즈 평균 관객수 : 이전 시리즈들의 관객수 평균
- 휴일 수 : 영화 상영 기간 동안의 휴일 수

#### 3. 모델링
**XGBoost**를 사용했으며, 평가지표로 **RMSE** 사용
1. 제작 국가, 장르, 스크린 수, 상영기간, 등급 사용 : 173052.4
2. 감독 평균 관객수 파생변수 추가 : 168598.5
3. 주연 배우 평균 관객수 파생변수 추가 : 157489.7
4. 시리즈 평균 관객수 파생변수 추가 : 135054.7

# Skills
- R
- Python3
- Selenium
- BeautifulSoup 4

<br>

---
  
<br>

#### Open Source License는 [이곳](NOTICE.md)에서 확인해주시고, 문의사항은 [Issue](https://github.com/vivaan-park/image-scrapper/issues) 페이지에 남겨주세요.
