library("ggplot2")
library("dplyr")
library("tidyverse")
library("corrplot")
library("ggcorrplot")
library("reldist")
library("gridExtra")
library("directlabels")
theme_set(theme_dark())

ploan<-read.csv("Bank_Personal_Loan.csv")
ploan1<-ploan
ploan2<-ploan
ploan2$Personal.Loan[ploan2$Personal.Loan==0]<-'Rejected';ploan2$Personal.Loan[ploan2$Personal.Loan==1]<-'Aceepted'
ploan2$Online[ploan2$Online==0]<-'No';ploan2$Online[ploan2$Online==1]='Yes'
ploan2$Securities.Account[ploan2$Securities.Account==0]<-'No';ploan2$Securities.Account[ploan2$Securities.Account==1]<-'Yes'
ploan2$CD.Account[ploan2$CD.Account==0]<-'No';ploan2$CD.Account[ploan2$CD.Account==1]<-'Yes'
ploan2$CreditCard[ploan2$CreditCard==0]<-'No';ploan2$CreditCard[ploan2$CreditCard==1]<-'Yes'
ploan2$Education[ploan$Education==1]<-'Undergraduate';ploan2$Education[ploan2$Education==2]<-'Graduate';ploan2$Education[ploan2$Education==3]<-'Advanced/Professional'

#################################################################
pl<-as.numeric(factor(ploan1$Personal.Loan))-1
pl0<-length(pl[pl==0]);pl1<-length(pl[pl==1])
lab<-paste0(c(pl0*100/(pl0+pl1),pl1*100/(pl0+pl1)),c("% Rejected","% Accepted"))
pie(c(pl0,pl1),labels =lab,col = c("Red","Cyan"),main = "Pie chart of Loan Acceptance")
##################################################################

ggplot(ploan2,aes(x=Income))+geom_density(alpha=0.7,aes(fill=factor(Personal.Loan)))+xlab("Income(in $1000)")+labs(title = 'Loan acceptance with Income')+scale_fill_discrete(name="Offered Loan",labels=c('Rejected','Accepted'))+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour","fill"))
ggplot(ploan2,aes(x=CCAvg))+geom_density(alpha=0.7,aes(fill=factor(Personal.Loan)))+xlab("Credit Card expenditure per month(in $1000)")+labs(title = 'Loan acceptance with Credit Card Expenditure')+scale_fill_discrete(name="Offered Loan",labels=c('Rejected','Accepted'))+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour","fill"))

#################################################################
## Checking the association of Mortgage with Income
ploan2%>%select(Income,Mortgage,Personal.Loan)%>%ggplot(aes(x=Income,y=Mortgage,col=factor(Personal.Loan)))+geom_point()+scale_fill_discrete(name="Offered Loan",labels=c('Rejected','Accepted'))+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour","fill"))+xlab("Income(in $1000)")+ylab("Mortgage(in $1000)")+labs(title ="Association of Income with Mortgage with loan acceptance")
gini(ploan$Mortgage)### Shows me there are large number of customers whose Mortgage amount is 0

###### To see the positive association between Income and Mortgage without the rows having 0 mortgage values
ploan2%>%select(Income,Mortgage,Personal.Loan)%>%filter(Mortgage!=0)%>%ggplot(aes(x=Income,y=Mortgage,col=factor(Personal.Loan)))+geom_point()+facet_wrap(~Personal.Loan)+geom_smooth(method = "lm")+scale_fill_discrete(name="Offered Loan",labels=c('Rejected','Accepted'))+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour","fill"))+xlab("Income(in $1000)")+ylab("Mortgage(in $1000)")+labs(title = "Association of Income and Mortgage(customers with 0 mortgages are excluded)")
###### CHECKING CORRELATION
ploan2%>%select(Income,Mortgage,Personal.Loan)%>%filter(Mortgage!=0)%>%summarise(cor(Income,Mortgage))
########################################################
########################################################

########################################################

########### Variation of the rejected Customers in Family size
ggplot(ploan1,aes(x=factor(Family),y=Income,fill=factor(Personal.Loan)))+geom_boxplot()+xlab("Family size")+ylab("Income(in $1000)")+labs(title = "Acceptance of Personal Loan with Income and Family Size")+scale_fill_manual(name="Personal Loan",labels=c("Rejected","Accepted"),values = c("Red","Cyan"),aesthetics = c("colour","fill"))

########################################################


###########Association of Income with Credit Card Expenditure
ggplot(ploan2,aes(x=Income,y=CCAvg))+geom_point(aes(col=factor(Personal.Loan)))+geom_smooth(method = lm,col="Green")+xlab("Income(in $1000)")+scale_fill_discrete(name="Offered Loan",labels=c('Rejected','Accepted'))+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour"))+ylab("Credit Card Expenditure per month(in $1000)")+labs(title = "Income vs Credit Card Expenditure with loan Acceptance")

ploan3<-ploan1[-c(1,5,8,10:14)]
ggcorrplot(cor(ploan3),lab = TRUE,type ="lower")+labs(title = "Correlation Matrix")+theme(plot.title = element_text(hjust = 0.5))

################################################

####### How loan Acceptance varies with Income and Credit Card Expenditure per month
######

ggplot(ploan2,aes(x=Income,y=CCAvg,col=factor(Personal.Loan)))+geom_point()+facet_wrap(~factor(Education))+xlab("Income(in $1000)")+ylab("Credit Card Expenditure per month")+labs(title = "Income vs Credit Card Expenditure with Educational qualification")+scale_fill_discrete(name="Offered Loan",labels=c('Rejected','Accepted'))+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour"))

ggplot(ploan2,aes(x=Income,y=CCAvg,col=factor(Personal.Loan)))+geom_point()+facet_wrap(~factor(Family))+xlab("Income(in $1000)")+ylab("Credit Card Expenditure per month")+labs(title = "Income vs Credit Card Expenditure with Family Size")+scale_fill_discrete(name="Offered Loan",labels=c('Rejected','Accepted'))+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour"))

################################################
#Proof of the claim that most of the undergraduate undergraduate customers who have family size 1 and 2 have rejected the loan
ggplot(ploan2,aes(x=Education,y=..count..,fill=factor(Personal.Loan)))+facet_wrap(~Family)+geom_bar(position = "dodge")+xlab('Education qualification')+labs(title = 'Educational qualification and Family Size with Loan acceptance')+geom_text(aes(label=..count..),stat = 'count',position = position_dodge(0.9),vjust=-0.2)+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour","fill"))




##############################################
################## Loan Acceptance with banking facilities
On<-ggplot(ploan2,aes(x=Online,y=..count..,fill=factor(Personal.Loan)))+geom_bar(position = 'dodge')+geom_text(aes(label=..count..),stat = 'count',position = position_dodge(0.9),vjust=-0.2)+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour","fill"))+theme(legend.position = 'none')
SA<-ggplot(ploan2,aes(x=Securities.Account,fill=factor(Personal.Loan)))+geom_bar(position = 'dodge')+geom_text(aes(label=..count..),stat = 'count',position = position_dodge(0.9),vjust=-0.2)+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour","fill"))
CA<-ggplot(ploan2,aes(x=CD.Account,fill=factor(Personal.Loan)))+geom_bar(position = 'dodge')+geom_text(aes(label=..count..),stat = 'count',position = position_dodge(0.9),vjust=-0.2)+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour","fill"))+theme(legend.position = 'none')
CC<-ggplot(ploan2,aes(x=CreditCard,fill=factor(Personal.Loan)))+geom_bar(position = 'dodge')+geom_text(aes(label=..count..),stat = 'count',position = position_dodge(0.9),vjust=-0.2)+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour","fill"))
grid.arrange(On,SA,CA,CC,top="Acceptance of loan with other banking facilities",bottom="Online=Customer uses Net Banking \nSecurities.Account=Customer have Securities Account \nCD.Account=Customer have Certificate of Deposit Account\nCreditCard=Customer have Credit Card issued by Universal Bank")
####### Most of the customers who uses Net Banking facility rejects the loan

##########################################################
EP<-ggplot(ploan2,aes(x=Education,y=..count..,fill=factor(Personal.Loan)))+facet_wrap(~Online)+geom_bar(position = "dodge")+xlab("Educational qualifications")+labs(title = "Acceptance of Personal loan with Educational qualification and Net banking",subtitle = "Customer uses Net Banking")+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour","fill"))
FP<-ggplot(ploan2,aes(x=Family,y=..count..,fill=factor(Personal.Loan)))+facet_wrap(~Online)+geom_bar(position = "dodge")+xlab("Family Size")+labs(title = "Acceptance of Personal loan with Family Size and Net banking",subtitle = "Customer uses Net Banking")+scale_fill_manual(name="Personal Loan",labels=c("Accepted","Rejected"),values = c("Cyan","Red"),aesthetics = c("colour","fill"))
grid.arrange(EP,FP)
### Checking if the customers who are rejecting the loan though they are using 
#Net banking falls under any specific category i.e., Educational qualifications and Family Size

