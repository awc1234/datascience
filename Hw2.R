
car <- read.table("C:/Users/GUser/Desktop/R/data/car.data",header=TRUE,sep=',')
#데이터 보기 
class(car); glimpse(car)
head(car); tail(car)
nlevels(car$doors)

#type변환 
car$doors = as.integer(gsub('5more','5',car$doors))
car$person = as.integer(gsub('more','5',car$person))

i=0
for( x in c('unacc','acc','good','v2') ){
  car$Evaluation = gsub(x,as.character(i),car$Evaluation)
  i=i+1
}
car$Evaluation = as.integer(car$Evaluation)



#수치형 - doors, person, Evaluation
old_par = par(mfrow=c(2,2))
hist(car$doors)
boxplot(car$doors)
qqnorm(car$doors)
qqline(car$doors)
par(old_par)

door = car$doors
n = length(door)
mu0 = 22.9
t.test(door, mu=mu0, alternative = "greater")

#범주형 - buying, maint, lug_boot, safety
table(car$safety)
prop.table(table(car$safety))
barplot(table(car$safety))
x = car$safety
binom.test(x=length(x[x=='low']), n = length(x), p = 0.5, alternative = "two.sided")


#수량x - person, 수량y - Evaluation
ggplot(car, aes(person, Evaluation)) + geom_jitter()
  #=> person 3일때 데이터가 없는 것을 보아 3인용 차를 고려하지 않았고 2인용차는 모두 Evaluation이 0인 것을 보아 차가 2인용이라면 평가가 좋지 않다. 
car$Evaluation[car$person==2]
plot(x = car$person, y = car$Evaluation)

cor(car$person, car$Evaluation)
with(car, cor(person, Evaluation))
with(car, cor(person, Evaluation), method = "kendall")

#범주x - safety, 수량y - Evaluation 
ggplot(car, aes(safety, Evaluation)) + geom_jitter() 
  #=> safety가 low인 것은 Evaluation이 0이므로 safety가 Evaluation에 많은 영향을 미친다.

car %>% ggplot(aes(safety, Evaluation)) + geom_boxplot()
(hwy_lm2 = lm(Evaluation ~ safety, data=car))
summary(hwy_lm2)
predict(hwy_lm2, newdata=data.frame(safety="med"))

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm2, las = 1)  	# Residuals, Fitted, ...
par(opar)

#수량x - Evaluation, 범주y - buying 

car %>% ggplot(aes(Evaluation, buying)) + geom_point()
car %>% ggplot(aes(factor(buying), Evaluation)) + geom_boxplot()
  #=> 가격이 매우 비쌀 때 대부분 평가가 0이고 간혹 1이 존재함으로 보아 가격이 매우 높을때 평가가 좋지 않다.
car$Evaluation[car$buying=='vhigh']


# CAR             car acceptability
# PRICE           overall price
# buying          buying price
# maint           price of the maintenance
# TECH            technical characteristics
# COMFORT         comfort
# doors           number of doors
# persons         capacity in terms of persons to carry
# lug_boot        the size of luggage boot
# safety          estimated safety of the car







car
doors, person, Evaluation
buying, maint, lug_boot, safety
수량x - person, 수량y - Evaluation
범주x - safety, 수량y - Evaluation 
수량x - Evaluation, 범주y - buying
