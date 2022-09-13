#install.packages(tidyverse)
library(tidyverse)
#gls
library(nlme)
#install.packages("multcomp")
library(multcomp)


##0.데이터 불러오기
body.df <- read.csv('C:/Users/lsj70/OneDrive - dongguk.edu/바탕 화면/학교 수업/3학년 1학기/다변량해석/프로젝트/body/bodyPerformance.csv')

#gender : 성별

#age : 나이
#weight_kg : 몸무게
#body.fat_. : 체지방
#diastolic : 이완기 혈압
#systolic : 수축기 혈압

#gripForc : 악력
#sit.and.bend.forward_cm : 앉아서 몸 앞으로 숙이기
#sit.ups.counts : 윗몸 일으키기
#broad.jump_cm : 제자리 멀리뛰기

#class : 등급


##1.EDA

#결측치 확인
str(body.df)
colSums(is.na(body.df))
#결측치 없음

#이상치 확인
summary(body.df)

#체지방률,수축기, 이완기 혈압, 몸 앞으로 숙이기, 악력, 제자리 멀리뛰기 이상치 존재

head(sort(body.df$body.fat_.,decreasing = T),10)
body.df %>%
  filter(body.fat_. > 50) %>%
  arrange(-body.fat_.)
#체지방 78이 말이 안되며 다른 수행 능력에 비해 이상함.

head(sort(body.df$diastolic),10)
head(sort(body.df$systolic),10)
#혈압 이상함

head(sort(body.df$sit.and.bend.forward_cm,decreasing = T),10)
#185cm 213cm는 말이 안되니까 제외


head(sort(body.df$gripForce),10)
body.df %>% 
  filter(gripForce == 0)
#악력이 0인 것이 말이 안됨. 성인 여성 기준 나이 상관없이 최저 악력이 5.8이었음 -> KOSIS

head(sort(body.df$broad.jump_cm),10)
body.df %>% 
  filter(broad.jump_cm == 0)
#제자리 멀리 뛰기가 0인 것도 이상

#이상치 제거
body.df2 <- body.df %>% 
  filter(diastolic != 0, diastolic != 6, diastolic != 8,body.fat_. != 78.4,
         systolic != 0, systolic != 14,
         sit.and.bend.forward_cm != 185 , sit.and.bend.forward_cm != 213,
         gripForce != 0, broad.jump_cm!= 0)
summary(body.df2)

nrow(body.df)
nrow(body.df2)

#변수들 분포 그래프 시각화
#신체변수
par(mfrow=c(2,3), cex.lab = 1.5,cex.main = 2)
hist(body.df2[,1],freq=F, xlab = colnames(body.df2)[1],main = paste(colnames(body.df2)[1], " Histogram"))
lines(density(body.df2[,1]),col='red')
hist(body.df2[,3],freq=F, xlab = colnames(body.df2)[3],main = paste(colnames(body.df2)[3], " Histogram"))
lines(density(body.df2[,3]),col='red')
hist(body.df2[,4],freq=F, xlab = colnames(body.df2)[4],main = paste(colnames(body.df2)[4], " Histogram"))
lines(density(body.df2[,4]),col='red')
hist(body.df2[,5],freq=F, xlab = colnames(body.df2)[5],main = paste(colnames(body.df2)[5], " Histogram"))
lines(density(body.df2[,5]),col='red')
hist(body.df2[,6],freq=F, xlab = colnames(body.df2)[6],main = paste(colnames(body.df2)[6], " Histogram"))
lines(density(body.df2[,6]),col='red')
hist(body.df2[,7],freq=F, xlab = colnames(body.df2)[7],main = paste(colnames(body.df2)[7], " Histogram"))
lines(density(body.df2[,7]),col='red')


#PCA에서 정규성을 만족해야하는 이유는, 주성분들이 원 자료를 설명하는 설명력 때문
#우리는 설명력에 대한 내용이 필요가 없기 때문에 정규성 만족이 필수는 아니라고 판단.
par(mfrow=c(2,3), cex.lab = 1.5,cex.main = 2)
qqnorm(body.df2[,1], xlab = colnames(body.df2)[1])
qqline(body.df2[,1],distribution = qnorm, xlab = colnames(body.df2)[1])
qqnorm(body.df2[,3], xlab = colnames(body.df2)[3])
qqline(body.df2[,3],distribution = qnorm, xlab = colnames(body.df2)[3])
qqnorm(body.df2[,4], xlab = colnames(body.df2)[4])
qqline(body.df2[,4],distribution = qnorm, xlab = colnames(body.df2)[4])
qqnorm(body.df2[,5], xlab = colnames(body.df2)[5])
qqline(body.df2[,5],distribution = qnorm, xlab = colnames(body.df2)[5])
qqnorm(body.df2[,6], xlab = colnames(body.df2)[6])
qqline(body.df2[,6],distribution = qnorm, xlab = colnames(body.df2)[6])
qqnorm(body.df2[,7], xlab = colnames(body.df2)[7])
qqline(body.df2[,7],distribution = qnorm, xlab = colnames(body.df2)[7])


#운동변수들이 남녀에 대하여 어떠한 분포를 가지는지
N_f = sum(body.df2$gender == 'F')
N_m = sum(body.df2$gender == 'M')
N = nrow(body.df2)
par(mfrow=c(2,2))
hist(body.df2[,8],freq=F, xlab = colnames(body.df2)[8], main = paste(colnames(body.df2)[8],' Histogram'))
lines(density(body.df2[,8]),col='green',lwd = 3)
lines(density(body.df2[body.df2$gender == 'F',8])$x,
      density(body.df2[body.df2$gender == 'F',8])$y * N_f/N ,col='red',lwd = 2)
lines(density(body.df2[body.df2$gender == 'M',8])$x,
      density(body.df2[body.df2$gender == 'M',8])$y * N_m/N ,col='blue',lwd = 2)

hist(body.df2[,9],freq=F, xlab = colnames(body.df2)[9], main = paste(colnames(body.df2)[9],' Histogram'))
lines(density(body.df2[,9]),col='green',lwd = 3)
lines(density(body.df2[body.df2$gender == 'F',9])$x,
      density(body.df2[body.df2$gender == 'F',9])$y * N_f/N ,col='red',lwd = 2)
lines(density(body.df2[body.df2$gender == 'M',9])$x,
      density(body.df2[body.df2$gender == 'M',9])$y * N_m/N ,col='blue',lwd = 2)

hist(body.df2[,10],freq=F, xlab = colnames(body.df2)[10], main = paste(colnames(body.df2)[10],' Histogram'))
lines(density(body.df2[,10]),col='green',lwd = 3)
lines(density(body.df2[body.df2$gender == 'F',10])$x,
      density(body.df2[body.df2$gender == 'F',10])$y * N_f/N ,col='red',lwd = 2)
lines(density(body.df2[body.df2$gender == 'M',10])$x,
      density(body.df2[body.df2$gender == 'M',10])$y * N_m/N ,col='blue',lwd = 2)

hist(body.df2[,11],freq=F, xlab = colnames(body.df2)[11], main = paste(colnames(body.df2)[11],' Histogram'))
lines(density(body.df2[,11]),col='green',lwd = 3)
lines(density(body.df2[body.df2$gender == 'F',11])$x,
      density(body.df2[body.df2$gender == 'F',11])$y * N_f/N ,col='red',lwd = 2)
lines(density(body.df2[body.df2$gender == 'M',11])$x,
      density(body.df2[body.df2$gender == 'M',11])$y * N_m/N ,col='blue',lwd = 2)

#쌍봉 형태를 보이는 것들이 존재 -> 성별에 따라서 다르게 보이는 것으로 생각

#성별에 따른 운동 변수 시각화 - boxplot
par(mfrow = c(2,2))
boxplot(gripForce ~ gender, data = body.df2)
boxplot(sit.and.bend.forward_cm ~ gender, data = body.df2)
boxplot(sit.ups.counts ~ gender, data = body.df2)
boxplot(broad.jump_cm ~ gender, data = body.df2)
#outlier들을 아우를 수 있는 새로운 그룹 척도가 필요


#우선적으로 신체 변수들을 이용하기 위한 PCA 진행
##3.PCA
pca.body <- princomp(body.df2[,c(1,3,4,5,6,7)], cor = T)
pca.body$sdev^2 / sum(pca.body$sdev^2)
cumsum(pca.body$sdev^2 / sum(pca.body$sdev^2))
par(mfrow = c(1,1))
screeplot(pca.body, type = "lines")
#주성분 3개!

pca.body$loadings[,1:3]

#PC1 : 신체적 여성 성향
par(mfrow = c(1,3))
boxplot(diastolic ~ gender, data = body.df)
boxplot(systolic ~ gender, data = body.df)
boxplot(age ~ gender, data = body.df)
#주로 여성의 체지방이 더 높고, 키 몸무게가 낮다. 혈압도 여성이 더 낮고,
#이 데이터에서는 여성의 나이가 더 많았다.

#PC2 : 신체나이, 노화 정도
#나이가 늘어나면 체지방이 증가하고 혈압도 올라가기 때문에

#PC3 : 반(反)비만도

body.df2 %>%
  ggplot(aes(body.fat_. , diastolic)) +
  geom_point()
#생각보다 체지방률은 혈압과 연관이 없다.

par(mfrow = c(1,1))
PC1 <- pca.body$scores[,1];PC1
gender <- body.df2$gender;gender
boxplot(PC1 ~ gender, main = "")
#PC1의 boxplot 결과도 여성이 주로 더 높게 나옴!
#하지만, boxplot의 +-1.5*IQR 부분을 고려하였을 때 겹치는 부분이 생긴다.
#PC1: 여성 성향과 성별이 반드시 일치하는 것은 아니다.

pc1 <- pca.body$scores[,1]
body.df3 <- cbind(body.df2[,c(8:11,2)], pc1)
head(body.df3)

hist(body.df3$pc1,breaks = 50,freq = F)
lines(density(body.df3[body.df3$gender == 'M',]$pc1),col='red')
lines(density(body.df3[body.df3$gender == 'F',]$pc1),col='blue')
#PC1들이 남녀별로 정규분포를 따른다.


#남성 pc1 평균과, 여성 pc1 평균의 중간 값을 기준으로, 남성성(1), 여성성(0)으로 구분
cut.value = (mean(body.df3[body.df3$gender == 'M',]$pc1) + mean(body.df3[body.df3$gender == 'F',]$pc1))/2
gender.power = ifelse(body.df3$pc1 >= cut.value, 0, 1)
data.lst <- cbind(body.df3[,1:5],gender.power)
#실제 gender와 gender.power가 일치하는지 여부를 gender.test로 생성
gender.test = ifelse(data.lst$gender == 'M',
                     ifelse(data.lst$gender.power == '1',"Yes","No"),
                     ifelse(data.lst$gender.power == '0',"Yes","No"))
data.lst <- cbind(data.lst,gender.test)
data.lst$gender <- as.factor(data.lst$gender)
data.lst$gender.power <- as.factor(data.lst$gender.power)
data.lst$gender.test <- as.factor(data.lst$gender.test)


##4.MANOVA
#PC1 변수가 과연 group을 나누는 데에 있어서 도움이 되는지 평가

#정규성 테스트
par(mfrow=c(1,4))
for(i in 1:4){
hist(data.lst[data.lst$gender == 'F' & data.lst$gender.test == 'No',i],freq=F, xlab = colnames(data.lst)[i],main = 'F.No', ylab = '')
lines(density(data.lst[data.lst$gender == 'F' & data.lst$gender.test == 'No',i]),col = 'red')
}
for(i in 1:4){
  hist(data.lst[data.lst$gender == 'F' & data.lst$gender.test == 'Yes',i],freq=F, xlab = colnames(data.lst)[i],main = 'F.Yes', ylab = '')
  lines(density(data.lst[data.lst$gender == 'F' & data.lst$gender.test == 'Yes',i]),col = 'red')
}
for(i in 1:4){
  hist(data.lst[data.lst$gender == 'M' & data.lst$gender.test == 'No',i],freq=F, xlab = colnames(data.lst)[i],main = 'M.No', ylab = '')
  lines(density(data.lst[data.lst$gender == 'M' & data.lst$gender.test == 'No',i]),col = 'red')
}
for(i in 1:4){
  hist(data.lst[data.lst$gender == 'M' & data.lst$gender.test == 'Yes',i],freq=F, xlab = colnames(data.lst)[i],main = 'M.Yes', ylab = '')
  lines(density(data.lst[data.lst$gender == 'M' & data.lst$gender.test == 'Yes',i]),col = 'red')
}
##Q-Q Plot
par(mfrow=c(4,4), cex.lab = 2,cex.main = 2.5)
for(i in 1:4){
  qqnorm(data.lst[data.lst$gender == 'F' & data.lst$gender.test == 'No',i], xlab = colnames(data.lst)[i], main = "F.No Q-Q Normal")
  qqline(data.lst[data.lst$gender == 'F' & data.lst$gender.test == 'No',i],distribution = qnorm, xlab = colnames(data.lst)[i])
}
for(i in 1:4){
  qqnorm(data.lst[data.lst$gender == 'F' & data.lst$gender.test == 'Yes',i], xlab = colnames(data.lst)[i], main = "F.Yes Q-Q Normal")
  qqline(data.lst[data.lst$gender == 'F' & data.lst$gender.test == 'Yes',i],distribution = qnorm, xlab = colnames(data.lst)[i])
}
for(i in 1:4){
  qqnorm(data.lst[data.lst$gender == 'M' & data.lst$gender.test == 'No',i], xlab = colnames(data.lst)[i], main = "M.No Q-Q Normal")
  qqline(data.lst[data.lst$gender == 'M' & data.lst$gender.test == 'No',i],distribution = qnorm, xlab = colnames(data.lst)[i])
}
for(i in 1:4){
  qqnorm(data.lst[data.lst$gender == 'M' & data.lst$gender.test == 'Yes',i], xlab = colnames(data.lst)[i], main = "M.Yes Q-Q Normal")
  qqline(data.lst[data.lst$gender == 'M' & data.lst$gender.test == 'Yes',i],distribution = qnorm, xlab = colnames(data.lst)[i])
}

cov(data.lst[data.lst$gender == 'F' & data.lst$gender.test == 'No',c(1:4)])
cov(data.lst[data.lst$gender == 'F' & data.lst$gender.test == 'Yes',c(1:4)])
cov(data.lst[data.lst$gender == 'M' & data.lst$gender.test == 'No',c(1:4)])
cov(data.lst[data.lst$gender == 'M' & data.lst$gender.test == 'Yes',c(1:4)])




#개수 확인
table(data.lst[,c('gender.test','gender')])
#비율적으로 생각했을 때 말이 된다고 생각.


#MANOVA를 위해서 1자로 데이터 나열.
data.lst <- arrange(data.lst,gender,gender.test)
head(data.lst)
nn = nrow(data.lst)
pp = 4
y <- as.numeric(t(as.matrix(data.lst[,1:4])))
id = rep(1:nn, each=pp)
gi=c(rep("F",4917*pp),rep("M",8456*pp))
gk=c(rep("No",508*pp),rep("Yes",4409*pp),rep("No",1020*pp),rep("Yes",7436*pp))
gj=rep(c("1","2","3","4"),nn)
gikj = paste(gi,gk,gj,sep=".")
gij = paste(gi,gj,sep=".")
gkj = paste(gk,gj,sep=".")
gik = paste(gi,gk,sep=".")

data.lst2 = data.frame(y,id,gi,gk,gj,gikj,gij,gkj,gik)
head(data.lst2)

##가설검정1
#H0 : 성별에 따른 운동 수행 능력의 차이가 없다.
#Full Model : y{ij} = g{ij}+ e{ij}
#Reduced Model : y{j} = mu{j} + e{j}

gF.1 = gls(y ~ gij, cor = corSymm(form = ~1|id),method = "ML", data = data.lst2)
gR.1 = gls(y ~ gj, cor = corSymm(form = ~1|id),method = "ML", data = data.lst2)
chi.test1 = anova(gR.1,gF.1);chi.test1

##가설검정2
#H0 : PC1에 따라 성질을 구분하는 것이 성별만으로 운동 수행 능력을 구분하는 것과 차이가 없다.
#Full Model : y{ijk} = gi{ij} + gk{kj} + gi{ij}:gk{kj} + e{ikj}
#Reduced Model : y{ij} = g{ij}+ e{ij}

gF.2 = gls(y ~ gikj, cor = corSymm(form = ~1|id),method = "ML", data = data.lst2)
gR.2 = gls(y ~ gij, cor = corSymm(form = ~1|id),method = "ML", data = data.lst2)
chi.test2 = anova(gR.2,gF.2); chi.test2

##가설검정3
#H0 : 본인의 성별과 PC1의 결과 일치 여부(gk)는, 성별(gi)이 운동 수행 능력(y)에 주는 영향과 상관이 없다.
#Full Model : y{ijk} = gi{ij} + gk{kj} + gi{ij}:gk{kj} + e{ikj}
#Reduced Model : y{ijk} = gi{ij} + gk{kj} + e{ikj}
gF.3 = gls(y ~ gikj, cor = corSymm(form = ~1|id),method = "ML", data = data.lst2)
gR.3 = gls(y ~ (gi+gk)*gj , cor = corSymm(form = ~1|id),method = "ML", data = data.lst2)
chi.test3 = anova(gR.3,gF.3); chi.test3


#셋 다 p-value : <.0001  ==>  H0 기각
#즉, 운동 수행 변수들에 대하여 성별 집단을 구분 하는데 있어서, 
#PC1에 의해 구분한 성향과 실제 성별의 일치 여부도 고려해주어야 한다.
#최종 모델
gF = gls(y ~ gikj-1, cor = corSymm(form = ~1|id),method = "ML", data = data.lst2)
summary(gF)


#gender 와 test를 결합한 변수 추가
gender.lst = as.factor(paste(data.lst$gender, data.lst$gender.test, sep="."))
data.0 = cbind(data.lst,gender.lst)
str(data.0)


##분류에 의한 시각화 결과를 보여주며 마무리
data.0 %>%
  ggplot(aes(x=broad.jump_cm, y=sit.and.bend.forward_cm, col=gender, shape = gender.test)) +
  geom_point()

str(data.0)
N_fy = sum(data.0$gender.lst == "F.Yes")
N_fn = sum(data.0$gender.lst == "F.No")
N_my = sum(data.0$gender.lst == "M.Yes")
N_mn = sum(data.0$gender.lst == "M.No")
N = nrow(data.0)

par(mfrow=c(2,2))
for(i in 1:4){
  hist(data.0[,i],freq=F, xlab = colnames(data.0)[i], main = paste(colnames(data.0)[i],' Histogram'))
  lines(density(data.0[,i]),col='green',lwd = 3)
  lines(density(data.0[data.0$gender.lst == "F.Yes",i])$x,
        density(data.0[data.0$gender.lst == "F.Yes",i])$y * N_fy/N ,col='red',lwd = 2)
  lines(density(data.0[data.0$gender.lst == "F.No",i])$x,
        density(data.0[data.0$gender.lst == "F.No",i])$y * N_fn/N ,col='magenta',lwd = 2)
  lines(density(data.0[data.0$gender.lst == "M.Yes",i])$x,
        density(data.0[data.0$gender.lst == "M.Yes",i])$y * N_my/N ,col='blue',lwd = 2)
  lines(density(data.0[data.0$gender.lst == "M.No",i])$x,
        density(data.0[data.0$gender.lst == "M.No",i])$y * N_mn/N ,col='cyan',lwd = 2)
}


par(mfrow=c(2,4),cex.lab = 1.5, cex.main = 1.5)
for(i in 1:4){
  hist(data.0[data.0$gender.test=='Yes',i],freq=F, xlab = colnames(data.0)[i], main = paste(colnames(data.0)[i],' Yes Histogram'))
  legend("topright", c("Female", "Male", "Total"), 
         col = c( "red", "blue", "green"), pch = c("-","-","-"))
  lines(density(data.0[data.0$gender.test=='Yes',i]),col='green',lwd = 3)
  lines(density(data.0[data.0$gender.lst == "F.Yes",i])$x,
        density(data.0[data.0$gender.lst == "F.Yes",i])$y * N_fy/(N_my+N_fy) ,col='red',lwd = 2)
  lines(density(data.0[data.0$gender.lst == "M.Yes",i])$x,
        density(data.0[data.0$gender.lst == "M.Yes",i])$y * N_my/(N_my+N_fy) ,col='blue',lwd = 2)
  hist(data.0[data.0$gender.test=='No',i],freq=F, xlab = colnames(data.0)[i], main = paste(colnames(data.0)[i],' No Histogram'))
  legend("topright", c("Female", "Male", "Total"), 
         col = c( "red", "blue", "green"), pch = c("-","-","-"))
  lines(density(data.0[data.0$gender.test=='No',i]),col='green',lwd = 3)
  lines(density(data.0[data.0$gender.lst == "F.No",i])$x,
        density(data.0[data.0$gender.lst == "F.No",i])$y * N_fn/(N_mn+N_fn) ,col='red',lwd = 2)
  lines(density(data.0[data.0$gender.lst == "M.No",i])$x,
        density(data.0[data.0$gender.lst == "M.No",i])$y * N_mn/(N_mn+N_fn) ,col='blue',lwd = 2)
}



par(mfrow=c(2,4))
for(i in 1:4){
  hist(data.0[data.0$gender=='F',i],freq=F, xlab = colnames(data.0)[i], main = paste(colnames(data.0)[i],' Female Histogram'))
  legend("topright", c("Yes", "No", "Total"), 
         col = c( "cyan", "magenta", "green"), pch = c("-","-","-"))
  lines(density(data.0[data.0$gender=='F',i]),col='green',lwd = 3)
  lines(density(data.0[data.0$gender.lst == "F.Yes",i])$x,
        density(data.0[data.0$gender.lst == "F.Yes",i])$y * N_fy/(N_fn+N_fy) ,fill='cyan',lwd = 2)
  lines(density(data.0[data.0$gender.lst == "F.No",i])$x,
        density(data.0[data.0$gender.lst == "F.No",i])$y * N_fn/(N_fn+N_fy) ,fill='magenta',lwd = 2)
  
  hist(data.0[data.0$gender=='M',i],freq=F, xlab = colnames(data.0)[i], main = paste(colnames(data.0)[i],' No Histogram'))
  legend("topright", c("Female", "Male", "Total"), 
         col = c( "red", "blue", "green"), pch = c("-","-","-"))
  lines(density(data.0[data.0$gender=='M',i]),col='green',lwd = 3)
  lines(density(data.0[data.0$gender.lst == "M.Yes",i])$x,
        density(data.0[data.0$gender.lst == "M.Yes",i])$y * N_my/(N_my+N_mn) ,fill='cyan',lwd = 2)
  lines(density(data.0[data.0$gender.lst == "M.No",i])$x,
        density(data.0[data.0$gender.lst == "M.No",i])$y * N_mn/(N_my+N_mn) ,fill='magenta',lwd = 2)
}


par(mfrow=c(2,2))
for(i in 1:4){
  boxplot(data.0[,i] ~ data.0$gender.lst, xlab = "" ,ylab = colnames(data.0)[i], main = paste(colnames(data.0)[i],' Boxplot'))
}
?boxplot
plot(density(data.0[data.0$gender=='F',1]),col='blue',lwd = 2)
lines(density(data.0[data.0$gender.lst=='F.No',1]),col='red',lwd = 2)


par(mfrow=c(2,4))
for(i in 1:4){
  plot(density(data.0[data.0$gender=='F',i]),col='blue',lwd = 2, xlab = colnames(data.0)[i], main = paste(colnames(data.0)[i],' Female Density'))
  legend("topright", c("Male", "Male.No"), 
         col = c( "blue", "red"), pch = c("-","-"))
  lines(density(data.0[data.0$gender.lst=='F.No',i]),col='red',lwd = 2)
  
  plot(density(data.0[data.0$gender=='M',i]),col='blue',lwd = 2, xlab = colnames(data.0)[i], main = paste(colnames(data.0)[i],' Male Density'))
  legend("topright", c("Female", "Female.No"), 
         col = c( "blue", "red"), pch = c("-","-"))
  lines(density(data.0[data.0$gender.lst=='M.No',i]),col='red',lwd = 2)
}

