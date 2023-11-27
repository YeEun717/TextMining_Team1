print('hi')


read.csv(../)

# ggplot2 패키지를 불러옵니다
library(ggplot2)

# mtcars 데이터셋을 이용해 연비(mpg)와 배기량(disp) 간의 관계를 산점도로 그립니다
ggplot(mtcars, aes(x = disp, y = mpg)) + 
  geom_point() + 
  theme_minimal() + 
  ggtitle("연비와 배기량의 관계") + 
  xlab("배기량") + 
  ylab("연비")

