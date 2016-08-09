
car <- read.table("C:/Users/GUser/Desktop/R/data/car.data",header=TRUE,sep=',')
#������ ���� 
class(car); glimpse(car)
head(car); tail(car)
nlevels(car$doors)

#type��ȯ 
car$doors = as.integer(gsub('5more','5',car$doors))
car$person = as.integer(gsub('more','5',car$person))

i=0
for( x in c('unacc','acc','good','v2') ){
  car$Evaluation = gsub(x,as.character(i),car$Evaluation)
  i=i+1
}
car$Evaluation = as.integer(car$Evaluation)



#��ġ�� - doors, person, Evaluation
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

#������ - buying, maint, lug_boot, safety
table(car$safety)
prop.table(table(car$safety))
barplot(table(car$safety))
x = car$safety
binom.test(x=length(x[x=='low']), n = length(x), p = 0.5, alternative = "two.sided")


#����x - person, ����y - Evaluation
ggplot(car, aes(person, Evaluation)) + geom_jitter()
  #=> person 3�϶� �����Ͱ� ���� ���� ���� 3�ο� ���� �������� �ʾҰ� 2�ο����� ��� Evaluation�� 0�� ���� ���� ���� 2�ο��̶�� �򰡰� ���� �ʴ�. 
car$Evaluation[car$person==2]
plot(x = car$person, y = car$Evaluation)

cor(car$person, car$Evaluation)
with(car, cor(person, Evaluation))
with(car, cor(person, Evaluation), method = "kendall")

#����x - safety, ����y - Evaluation 
ggplot(car, aes(safety, Evaluation)) + geom_jitter() 
  #=> safety�� low�� ���� Evaluation�� 0�̹Ƿ� safety�� Evaluation�� ���� ������ ��ģ��.

car %>% ggplot(aes(safety, Evaluation)) + geom_boxplot()
(hwy_lm2 = lm(Evaluation ~ safety, data=car))
summary(hwy_lm2)
predict(hwy_lm2, newdata=data.frame(safety="med"))

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm2, las = 1)  	# Residuals, Fitted, ...
par(opar)

#����x - Evaluation, ����y - buying 

car %>% ggplot(aes(Evaluation, buying)) + geom_point()
car %>% ggplot(aes(factor(buying), Evaluation)) + geom_boxplot()
  #=> ������ �ſ� ��� �� ��κ� �򰡰� 0�̰� ��Ȥ 1�� ���������� ���� ������ �ſ� ������ �򰡰� ���� �ʴ�.
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
����x - person, ����y - Evaluation
����x - safety, ����y - Evaluation 
����x - Evaluation, ����y - buying