rm(list = ls())
library(openxlsx)
library(data.table)
library(stringr)
library(fPortfolio)
#чтение данных и создание ожидаемого ммесячного дохода
dan<-read.xlsx("/Users/imac5k/Desktop/R/AP_invest/data1.xlsx")
dan<-data.table(dan)
dan[,Dates:=convertToDate(dan$Dates)] #создаю даты 
for(i in 2:length(names(dan))){ #выдергиваю имена акций
  names(dan)[i]<-str_extract(names(dan)[i],"[A-Z]+")
}
ret<-c()
data<-data.table()
dan[,YNDX:=NULL] # delete unnecessary shares (zhora must say which)
for(j in 2:dim(dan)[2]){ #calculate returns on shares
  for(i in 2:dim(dan)[1]){
    ret[i-1]<-dan[[names(dan)[j]]][[i]]/dan[[names(dan)[j]]][[i-1]]-1
  }
  data<-data[,names(dan)[j]:=ret]
  ret<-c()
}
data<-data.table(Dates=dan$Dates[-1],data) #delete first row, because no inf is about on first date 

#Для двух акций: Газпром и Сбер 

##1.
ret_GAZP<-mean(data$GAZP) # expected return on gazprom
ret_SBER<-mean(data$SBER) # variance of gazprom 
var_GAZP<-var(data$GAZP) 
var_SBER<-var(data$SBER)

wa<-0.5 # веса на самом деле не нужны но в задании есть этот пункт. Мне кажется, что надо прописать просто в предпосылках то, что мы юзаем такой то пакет, где в весах нет надобности ибо он сам все считает. 
wb<-1-wa
cor(subset(data,select = c("GAZP","SBER")))
ret<-c(ret_GAZP,ret_SBER)


dt<- as.timeSeries(subset(data,select = c("GAZP","SBER"))) #convert to necessary format
frontier<-portfolioFrontier(dt) # calculating many different portfolios 
frontierPlot(frontier,frontier = "upper", risk = "Cov",xlab="Risk") #graph при желание можете покапаться и посмотреть как меняются подписи осей и масштаб (я снова влезал в сам пакет и там правил)



#2. Risk-free, беру офз со сроком погашения в один год на 30.12.19, что является 5.21% в годовом выражении
rf<-0.0521/12

dt<- as.timeSeries(subset(data,select = c("GAZP","SBER")))
frontier<-portfolioFrontier(dt)
tan<-tangencyPortfolio(dt,`setRiskFreeRate<-`(portfolioSpec(),rf)) # build tangency portfolio and specifying our risk-free rate. 

Capital_Market_Line<-function(sigma_i){ #опять формальность 
  sharpe<-(getTargetReturn(tan)[1]-rf)/getTargetRisk(tan)[2]
  return(rf+sharpe*sigma_i)
}

frontierPlot(frontier,frontier = "upper", risk = "Cov",xlab="Risk") 
tangencyLines(tan) #используем tangentline, так как функция cmlLine берет основу этой функции можно посмотреть забив trace(cmlLines, edit=TRUE) надо обязательно это прописать 




#3
capm<-subset(data,select = c("SBER","GAZP","IMOEX"))
beta_SBER<-coef(lm(SBER~IMOEX,data = capm))[2]
mrp<-mean(capm$IMOEX)-rf
return_SBER_capm<-rf+beta_SBER*mrp
c(ret_SBER,return_SBER_capm)
#Отсюда видим, что capm  ниже чем реальный показатель. Если считаем, что capm is true model, тогда сбер недооценен и надо быстренько вкладывать все бабосики в него
#Но как мы знаем, что сапм говно-модель и лучше этого не делать)0)0))

beta_GAZP<-coef(lm(GAZP~IMOEX,data = capm))[2]
return_GAZP_capm<-rf+beta_GAZP*mrp
c(ret_GAZP,return_GAZP_capm)
#А вот тут уже видим, что capm предсказывает доход побольше. Получается, что акции overpriced и их надо сливать))

capm<-capm[,SBER_P:=SBER-rf]
capm<-capm[,GAZP_P:=GAZP-rf]
capm<-capm[,IMOEX_P:=IMOEX-rf]
model_SBER<-lm(SBER_P~IMOEX_P,data = capm)
p_value<-summary(model_SBER)$coefficients[2,4]
p_value
#Меньше чем 0.05 не реджектим, следовательно, самп валидна 
model_GAZP<-lm(GAZP_P~IMOEX_P,data = capm)
p_value<-summary(model_GAZP)$coefficients[2,4]
p_value
#Меньше чем 0.05 не реджектим, следовательно, самп валидна 


###ДЛЯ ВСЕХ АКЦИЙ ЧАСТЬ 2


#1 НУЖНО ДОПИСАТЬ ВЕСА, до назначать их, опять же по сути нам это не надо, но в задание есть... Я бы положил бы болт и не прописывал ибо это запарно. 
data[,QIWI:=NULL]
imoex<-data$IMOEX
data1<-subset(data,select = names(data)[-c(which(names(data)=="IMOEX"),which(names(data)=="Dates"))])
ret_var<-data.table(Abbreviation=c("Return","Var"))
for(i in 1:dim(data1)[2]){
  ret_var[,names(data1)[i]:=c(mean(data1[[names(data1)[i]]]),var(data1[[names(data1)[i]]]))]
}
dt<- as.timeSeries(data1)
frontier<-portfolioFrontier(dt)
frontierPlot(frontier,frontier = "upper", risk = "Cov",xlab="Risk")

#2 
rf<-0.0521/12

tan<-tangencyPortfolio(dt,`setRiskFreeRate<-`(portfolioSpec(),rf))
Capital_Market_Line<-function(sigma_i){
  sharpe<-(getTargetReturn(tan)[1]-rf)/getTargetRisk(tan)[2]
  return(rf+sharpe*sigma_i)
}
frontierPlot(frontier,frontier = "upper", risk = "Cov",xlab="Risk")
tangencyLines(tan)

#3
data1<-data1[,IMOEX:=imoex]
betas<-data.table()
return_capm<-data.table()
difference<-c()
result<-data.table()
mrp<-mean(data1$IMOEX)-rf
for(i in 1:(length(names(data1))-1)){
  formula<-as.formula(paste(names(data1)[i],"~IMOEX",sep = ""))
  betas[,names(data1)[i]:=coef(lm(formula,data = data1))[2]]
  return_capm[,names(data1)[i]:=rf+coef(lm(formula,data = data1))[2]*mrp]
  difference[i]<-return_capm[[names(data1)[i]]]-ret_var[[names(data1)[i]]][1]
  if(difference[i]>0){
    result[,names(data1)[i]:="Overpriced"]
  }else{
    result[,names(data1)[i]:="Underpriced"]
  }
}
betas #беты каждой бумаги
return_capm # доход по capm 
result # что надо делать с бумагой

market_premium<-function(x){
  return(x-rf)
}
data_rp<-as.data.table(apply(data1,2,market_premium))

test_capm<-data.table()
p_value<-c()
for(i in 1:(length(names(data_rp))-1)){
  formula<-as.formula(paste(names(data_rp)[i],"~IMOEX",sep = ""))
  model<-lm(formula,data = data_rp)
  p_value[i]<-summary(model)$coefficients[2,4]
  if(p_value[i]>0.05){
    test_capm[,names(data_rp)[i]:="Insignificant"]
  }else{
    test_capm[,names(data_rp)[i]:="Significant"]
  }
}
test_capm #говорит где модель сработала а где нет. 
#КОНЕЦ ПРОЕКТА




#Руками вроде все сходится для 2 пункта
rt<-function(x){
  return(x*ret_GAZP+(1-x)*ret_SBER)
}
sd1<-function(x){
  return(sqrt(x^2*var_GAZP+(1-x)^2*var_SBER+2*x*(1-x)*cov(data$GAZP,data$SBER)))
}
front<-data.table(weight=seq(0,1,by=0.001))
front[,return:=rt(front$weight)]
front[,risk:=sd1(front$weight)]
plot(front$risk,front$return,type="l")
lines(front$risk,front$weight)
front$risk[[which.min(front$risk)]]

sharpe<-function(x){
  num<-x[1]*ret_GAZP+x[2]*ret_SBER-0.0521/12
  den<-sqrt(x[1]^2*var_GAZP+x[2]^2*var_SBER+2*x[1]*x[2]*cov(data$GAZP,data$SBER))
  return((num/den))
}

const<-function(x){
  x[1]+x[2]
}
x0=c(0.5,0.5)
w<-solnp(x0,sharpe,const,eqB = 1)

cml<-function(y){
  rf+sharpe(w$pars)*y
}
cml(seq(0,0.1,by=0.001))

plot(front$risk,front$return,type="l",xlim=c(0.065,0.1),ylim=c(0,0.02))
lines(seq(0,0.1,by=0.001),cml(seq(0,0.1,by=0.001)))

#развлекуха или для тех кто хочет стать инвестором))))
minvariancePortfolio(dt)
