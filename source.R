drug <- read.csv("total_drug.csv")

# 국가별 데이터 추출
KOR_drug <- subset(drug, drug$ISO.Code == 'KOR')
CHN_drug <- subset(drug, drug$ISO.Code == 'CHN')
JPN_drug <- subset(drug, drug$ISO.Code == 'JPN')
USA_drug <- subset(drug, drug$ISO.Code == 'USA')
GBR_drug <- subset(drug, drug$ISO.Code == 'GBR')


# 국가 별 데이터프레임 생성
KOR_df <- data.frame(KOR_drug$Drug.Group, KOR_drug$KG.Equivalent, KOR_drug$Year)
CHN_df <- data.frame(CHN_drug$Drug.Group, CHN_drug$KG.Equivalent, CHN_drug$Year)
JPN_df <- data.frame(JPN_drug$Drug.Group, JPN_drug$KG.Equivalent, JPN_drug$Year)
USA_df <- data.frame(USA_drug$Drug.Group, USA_drug$KG.Equivalent, USA_drug$Year)
GBR_df <- data.frame(GBR_drug$Drug.Group, GBR_drug$KG.Equivalent, GBR_drug$Year)


# 엑셀 파일 생성
write.csv(KOR_df, 'KOR_drug.csv')
write.csv(CHN_df, 'CHN_drug.csv')
write.csv(JPN_df, 'JPN_drug.csv')
write.csv(USA_df, 'USA_drug.csv')
write.csv(GBR_df, 'GBR_drug.csv')


# 년도별 밀매 건수
KOR_drug_num <- table(KOR_drug$Year)
CHN_drug_num <- table(CHN_drug$Year)
JPN_drug_num <- table(JPN_drug$Year)
USA_drug_num <- table(USA_drug$Year)
GBR_drug_num <- table(GBR_drug$Year)



# 대한민국 마약 종류 파이 그래프
KOR_drug_group <- table(KOR_drug$Drug.Group)
label <- names(KOR_drug_group)
pct <- round(KOR_drug_group / sum(KOR_drug_group) * 100)
label <- paste(label, pct)
label <- paste(label, "%", sep="")
pie(KOR_drug_group, labels = label, col = rainbow(length(label)), main = "국내 마약 밀매 종류와 비율")



# 국가별 선 그래프 ( Y축 범위는 0~70 )
color = rainbow(5)

plot(levels(as.factor(KOR_drug$Year)), table(KOR_drug$Year), main = "나라 별 밀매 건수" , ylim = c(0, 70), lwd = 2, xlab = "년도", ylab = "마약 밀매 건수", type = 'l', col = color[1])
lines(levels(as.factor(CHN_drug$Year)), table(CHN_drug$Year), main = "나라 별 밀매 건수" , ylim = c(0, 70), lwd = 2, xlab = "년도", ylab = "마약 밀매 건수", type = 'l', col = color[2])
lines(levels(as.factor(JPN_drug$Year)), table(JPN_drug$Year), main = "나라 별 밀매 건수" , ylim = c(0, 70), lwd = 2, xlab = "년도", ylab = "마약 밀매 건수", type = 'l', col = color[3])
lines(levels(as.factor(USA_drug$Year)), table(USA_drug$Year), main = "나라 별 밀매 건수" , ylim = c(0, 70), lwd = 2, xlab = "년도", ylab = "마약 밀매 건수", type = 'l', col = color[4])
lines(levels(as.factor(GBR_drug$Year)), table(GBR_drug$Year), main = "나라 별 밀매 건수" , ylim = c(0, 70), lwd = 2, xlab = "년도", ylab = "마약 밀매 건수", type = 'l', col = color[5])


legend("topleft", legend = c("KOR", "CHN", "JPN", "USA", "GBA"), fill = color)



# 국가별 마약 밀매 건수 막대 그래프 ( Y축 범위는 0~70)
color = rainbow(5)
par(mfrow = c(1, 5))
barplot(KOR_drug_num, main = '한국 밀매 건수', 
        ylim = c(0, 70), xlab = "년도" , ylab = "밀매 건수", col = color[1])
barplot(CHN_drug_num, main = '중국 밀매 건수', 
        ylim = c(0, 70), xlab = "년도" , ylab = "밀매 건수", col = color[2])
barplot(JPN_drug_num, main = '일본 밀매 건수', 
        ylim = c(0, 70), xlab = "년도" , ylab = "밀매 건수", col = color[3])
barplot(USA_drug_num, main = '미국 밀매 건수', 
        ylim = c(0, 70), xlab = "년도" , ylab = "밀매 건수", col = color[4])
barplot(GBR_drug_num, main = '영국 밀매 건수', 
        ylim = c(0, 70), xlab = "년도" , ylab = "밀매 건수", col = color[5])


# 마약 밀매 건수 기각역
KOR_Z <- (KOR_drug_num[39] - mean(KOR_drug_num[1:38])) / (sd(KOR_drug_num) / sqrt(39))
CHN_Z <- (CHN_drug_num[36] - mean(CHN_drug_num[1:35])) / (sd(CHN_drug_num) / sqrt(36))
JPN_Z <- (JPN_drug_num[40] - mean(JPN_drug_num[1:39])) / (sd(JPN_drug_num) / sqrt(40))
USA_Z <- (USA_drug_num[40] - mean(USA_drug_num[1:39])) / (sd(USA_drug_num) / sqrt(40))
GBR_Z <- (GBR_drug_num[40] - mean(GBR_drug_num[1:39])) / (sd(GBR_drug_num) / sqrt(40))

# 우리나라 마약 밀매 건수 정규분포
KOR_m <- mean(KOR_drug_num)
KOR_std <- sd(KOR_drug_num)
x=seq((KOR_m-5*KOR_std),(KOR_m+5*KOR_std),0.01)
y=dnorm(x,mean=KOR_m,sd=KOR_std)
plot(x,y,xlim=c((KOR_m-5*KOR_std),(KOR_m+5*KOR_std)),type="l",ann=FALSE)
title(main="대한민국 마약 밀매 건수 정규분포",cex.main=1.6,xlab="x",ylab="f(x)",
      cex.lab=1.3)

