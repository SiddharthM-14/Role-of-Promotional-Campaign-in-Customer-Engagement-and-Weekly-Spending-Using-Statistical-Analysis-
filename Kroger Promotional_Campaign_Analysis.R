df=import("alldata.csv",sheet="alldata")
final_df=import("Downloads_dummy_2.xlsx",sheet="Sheet1")
df=final_df
colnames(df)=tolower(make.names(colnames(df)))
str(df)
dim(df)

library(dplyr)

#here we are creating recency variable.
week_since_last_purchase <- function(df) {
  
  df$recency <- NA           # create an empty vector to store recency
  week_since= 0           # initialise our weeks since counter to zero
  
  for(row in 1:nrow(df)){    # loop through our rows
    
    if(df$weeklyspend[row] == 0){  # if we did not purchase something this week
      
      week_since= week_since+ 1   # increment week_since
      df$recency[row] <- week_since  # set the recency to weeks since
      
    } else {                     # else if we did purchase something this month
      
      week_since= week_since+ 1   # increment week_since
      if(week_since== 1){   #     and if we purchased something last month as well
        df$recency[row] = NA   #         set the recency to NA
      }else{                   #     else we didn't purchase something last month
        df$recency[row] <- week_since   # set the recency to the week_since
      }
      week_since= 0         # reset the months since to zero
      
    }
  }
  df                           # return the modified dataframe
}
new_df <- week_since_last_purchase(df)

#converting the NA's to 0
rfm_df$recency[is.na(rfm_df$recency)] =0

#checking the number of 0's in the recency variable
table(rfm_df$recency)  # in this data det we have around 46k 0's records for recency so we may be having excess of zero problem.


#for creating frequency variable.
x <- length(unique(new_df$id))
y <- unique(df$id); y 
c = 0 
rfm_df = NULL 
for(i in 1:x)
{ 
  c = NULL
  c = new_df[new_df$id == y[i],]
  c$frequency = rollapplyr(c$purweek, 4, sum, partial = TRUE, na.rm = TRUE)
  rfm_df = rbind(rfm_df,c)
}



#creating D variable
#D:if the person enters in the campaign in week 8 it will be log of 1 for that then if the person 
#stays in the campaign till week 20 then till that it will be log of 13 for week 20 and if the 
#house hold leaves the campaign in week 21 so D will be 0 for that week 21 and if the household 
#again enters in week 25 so it will be log of 14

#I am creating 2 D variables 
#D_1 =D which will be log of number of spending by individual HH. It will consider if the HH have made 
#any spending in a week so for eg if the HH started buying first time in week 8 so for that week 8
#the D_1 will be log of 1 and if the HH keep on purchasing every week till 15 then D_1 will be
#incremental log and for week 15 it will be log (8) and if the HH didnt buy any thing for next 5 
#week for that D_1 will be 0 and when the HH again buys in week 21 the D_1 will be log(9) and so on

#D_2 : log of the number of weeks for each household since the first campaign.

count=0
x <- length(unique(practice$id))
y = unique(practice$id)
for(j in 1:x)
{
 for(i in 1:nrow(practice))
 {
  if(practice$id[i] == y[j])
    {
    
       if (practice$weeklyspend!=0)
        {
          practice$d[i]=count+1
          count=practice$d [i]
       }
    else{practice$d[i]=0}
    
  }
   else{count=0;
   break}
  }
}

Final_rfmd=rfm_df
########Final_rfmd=import("Final_rfmd.xlsx",sheet="Sheet1")


Final_rfmd <- Final_rfmd %>%                              # Create numbering variable
  group_by(id) %>%
  mutate(numbering = row_number())

table (Final_rfmd$numbering)


Final_rfmd$D=ifelse(Final_rfmd$numbering>0 ,log (Final_rfmd$numbering), 0)

#rounding the decimal places of D variable to 2 decimals.
Final_rfmd$D=round(Final_rfmd$D,2)

table (Final_rfmd$D)

#Creating the copy of the data set
copy_Final_rfmd=Final_rfmd
#Backup_dataset=Final_rfmd 
#Backup_dataset_2=copy_Final_rfmd

sequence = seq(1, nrow(Final_rfmd))
table(Final_rfmd$numall_now)
copy_Final_rfmd$sequence=sequence
copy_Final_rfmd$numall_now
copy_Final_rfmd_1=subset(copy_Final_rfmd,
                       numall_now>0)
copy_Final_rfmd_0=subset(copy_Final_rfmd,
                         numall_now==0)

#here I have created the new variable "numbering_2" which will be like if the HH has entered in 
#the campaign for the first time then the variable will be 1 and 
#then for the next week when the HH still in the campaign then it will be 2 and so on
#and in case if the HH is not in a campaign in a week so it will be 0 for that week
#The variable is basically the number of weeks for each household since the first campaign
copy_Final_rfmd_1 <- copy_Final_rfmd_1 %>%                 # Create numbering variable
  group_by(id) %>%
  mutate(numbering_2 = row_number())
copy_Final_rfmd_0$numbering_2=0
table(copy_Final_rfmd_0$numbering_2)
table(copy_Final_rfmd_1$numbering_2)

copy_Final_rfmd = rbind(copy_Final_rfmd_1,copy_Final_rfmd_0)
copy_Final_rfmd = copy_Final_rfmd[order(copy_Final_rfmd$sequence),]

#to get the D_2 variable which is the log of the number of weeks for 
#each household since the first campaign, I have taken log of numbering variable(which is number
#of weeks for each household since the first campaign).

copy_Final_rfmd$D_2=ifelse(copy_Final_rfmd$numbering_2>0 ,log (copy_Final_rfmd$numbering_2), 0)

#rounding the decimal places of D_2 variable to 2 decimals.
copy_Final_rfmd$D_2=round(copy_Final_rfmd$D_2,2)
 
table(copy_Final_rfmd$D_2)

#Copying the data set back to the main data set
Final_rfmd=copy_Final_rfmd


#' Data visualization
hist(Final_rfmd$recency)                #   Not normally distributed it looks righly skewed
hist(log(Final_rfmd$recency))           #   Even log function is goving righly skewed

hist(Final_rfmd$frequency)              # not noramally distributed it looks left skewed
hist(log(Final_rfmd$frequency))         # not noramally distributed it looks left skewed

hist(Final_rfmd$spendingavg)            #   Not normally distributed it looks righly skewed
hist(log(Final_rfmd$spendingavg))        # Much better

hist(Final_rfmd$D)
hist(log(Final_rfmd$D))

hist(Final_rfmd$D_2)
hist(log(Final_rfmd$D_2))

#dropping zipcode
Final_rfmd$zipcode=NULL


#combining the C dummy variables according to A_group, B_group and C_group

Final_rfmd$A_group= ifelse(Final_rfmd$c8==1 | Final_rfmd$c13==1 | Final_rfmd$c18==1 | Final_rfmd$c26==1 |
                         Final_rfmd$c30==1,1,0)

Final_rfmd$B_group= ifelse(Final_rfmd$c1==1 | Final_rfmd$c2==1 | Final_rfmd$c4==1 | Final_rfmd$c5==1 |
                         Final_rfmd$c7==1|Final_rfmd$c9==1|Final_rfmd$c10==1|Final_rfmd$c11==1|Final_rfmd$c12==1|
                         Final_rfmd$c16==1| Final_rfmd$c17==1| Final_rfmd$c19==1| Final_rfmd$c21==1|
                         Final_rfmd$c22==1| Final_rfmd$c23==1| Final_rfmd$c24==1| Final_rfmd$c25==1|
                         Final_rfmd$c28==1|Final_rfmd$c29==1,1,0)

Final_rfmd$C_group=ifelse(Final_rfmd$c3==1 | Final_rfmd$c6==1 | Final_rfmd$c14==1| Final_rfmd$c15==1|Final_rfmd$c20==1 
                      | Final_rfmd$c27==1,1,0)

#Backup_3=Final_rfmd
# Checking Correlation 
cor(Final_rfmd[c(8,9,71,72,73,76,77,78,79)])                                       
#chart.Correlation(Final_rfmd[c(8,9,71,72,73,76,77,78,79)])

fixed_model_id= lm(weeklyspend ~ as.factor(id)+frequency+recency+spendingavg+D+D_2+calendarweek+age+
                  store.shopped+married + income+ numall_now+hhsize+
                numkids+ A_group+B_group+ C_group+w_loydisc_fb+w_loydisc_cat+ 
                w_display+w_mailer, data= Final_rfmd)
summary(fixed_model_id)
#or
#with spendingavg
fixed_model_id_2 <- plm(weeklyspend ~ frequency+recency+spendingavg+D_2+calendarweek+age+
                          store.shopped+married +hhsize+income+
                          numkids+ A_group+B_group+ C_group+w_loydisc_fb+w_loydisc_cat+ 
                          w_display+w_mailer, data=Final_rfmd, index="id", model="within")
summary(fixed_model_id_2)

#with D variable instead of spendingavg and calendarweek
fixed_model_id_3 <- plm(weeklyspend ~ frequency+recency+D+D_2+age+
                          store.shopped+married +hhsize+income+
                          numkids+ A_group+B_group+ C_group+w_loydisc_fb+w_loydisc_cat+ 
                          w_display+w_mailer, data=Final_rfmd, index="id", model="within")
summary(fixed_model_id_3)

#with spendingavg
fixed_model_camp_1= lm(weeklyspend ~ id+frequency+recency+spendingavg+D_2+calendarweek+age+
                     store.shopped+married + income+hhsize+ numkids+ 
                    as.factor(A_group)+as.factor(B_group)+ as.factor(C_group)+w_loydisc_fb+
                    w_loydisc_cat+ w_display+w_mailer, data= Final_rfmd)
summary(fixed_model_camp_1)

#with D variable instead of spendingavg and calendarweek
fixed_model_camp_2= lm(weeklyspend ~ id+frequency+recency+D+D_2+age+
                         store.shopped+married + income+hhsize+ numkids+ 
                         as.factor(A_group)+as.factor(B_group)+ as.factor(C_group)+
                         w_loydisc_fb+w_loydisc_cat+ w_display+w_mailer, data= Final_rfmd)
summary(fixed_model_camp_2)

stargazer(fixed_model_id_2, fixed_model_id_3, fixed_model_camp_1, fixed_model_camp_2
          ,out="Final_rfmd.html",
          title= "Model (a) at the household level (b) at the campaign level (A, or B or C", 
          single.row=TRUE)

#stargazer(fixed_model_id_2,fixed_model_camp, type="text", single.row=TRUE)

#removing unwated variables
new_Final_rfmd=subset(Final_rfmd,select= -c(purweek,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,numa_now, numb_now, numc_now, numa_ever,numb_ever,numc_ever, t_acoup,tb_coup,t_ccoup,t_catcoup,t_favbrndcoup,t_loydisc_fb,t_loydisc_cat,t_display,t_mailer,sequence,numbering,numbering_2))
str(new_Final_rfmd)
dim(new_Final_rfmd)

library(bigmemory)
library(doSNOW)
library(biganalytics)
library(factoextra)

#unscaled dataset
#df_new_Final_rfmd is the unscaled dataset
df_new_Final_rfmd = as.data.frame(sapply(new_Final_rfmd, as.numeric))
str(df_new_Final_rfmd)
#dfc=scale(df_new_Final_rfmd)
#scaled dataset =cluster_rfmd
cluster_rfmd=scale(df_new_Final_rfmd)
#str(df_new_Final_rfmd)
#df_new_Final_rfmd$recency[is.na(df_new_Final_rfmd$recency)] =0

#new_Final_rfmd is the name of dataframe with the whole data
set.seed(123)
kmeans_1= bigkmeans(cluster_rfmd, centers = 3, iter.max = 100, nstart = 1, dist = "euclid")
table(kmeans_1$cluster)
df_new_Final_rfmd$index <-kmeans_1$cluster #adds a column with the cluster type for each row
table(df_new_Final_rfmd$index)
#plotting the clusters
kmeans_2 =  kmeans(dist(cluster_rfmd_small), 3)
plot(cluster_rfmd_small, col = mod3$cluster,pch=15)
parcoord(cluster_rfmd_small,lty=2,col=grey(.3))

fviz_cluster(kmeans_1, data = df_new_Final_rfmd, geom = c("point"),ellipse.type = "euclid")

## Define a function for the statistical model

Mode <- function(x, na.rm = FALSE) {if(na.rm){
  x = x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])}
###
df_new_Final_rfmd_Vec = aggregate(index~id,df_new_Final_rfmd,Mode)
table(df_new_Final_rfmd_Vec)

## find households id by cluster

m1 = subset(df_new_Final_rfmd_Vec,index==1)
m2 =subset(df_new_Final_rfmd_Vec,index==2)
m3 = subset(df_new_Final_rfmd_Vec,index==3)

##subset the original data bsed on the indeces above

data1 = subset(df_new_Final_rfmd,id %in% m1$id)
data2 =subset(df_new_Final_rfmd,id %in% m2$id)
data3 =subset(df_new_Final_rfmd,id %in% m3$id)

# running models for segment 3



segment_3_id_1=lm(weeklyspend~as.factor(id)+frequency+recency+spendingavg+D+D_2+calendarweek+age+
                    store.shopped+married + income+ numall_now+hhsize+
                    numkids+ A_group+B_group+ C_group+w_loydisc_fb+w_loydisc_cat+ 
                    w_display+w_mailer,data = data1)
summary(segment_3_id_1)
segment_3_id_2=lm(weeklyspend~as.factor(id)+frequency+recency+spendingavg+D+D_2+calendarweek+age+
                    store.shopped+married + income+ numall_now+hhsize+
                    numkids+ A_group+B_group+ C_group+w_loydisc_fb+w_loydisc_cat+ 
                    w_display+w_mailer,data = data2)
segment_3_id_3=lm(weeklyspend~as.factor(id)+frequency+recency+spendingavg+D+D_2+calendarweek+age+
                    store.shopped+married + income+ numall_now+hhsize+
                    numkids+ A_group+B_group+ C_group+w_loydisc_fb+w_loydisc_cat+ 
                    w_display+w_mailer,data = data3)
stargazer(segment_3_id_1,segment_3_id_2,segment_3_id_3,out = "segment_3_id.html",single.row = TRUE)



segment_3_camp_1=lm(weeklyspend~id+frequency+recency+spendingavg+D_2+calendarweek+age+
                store.shopped+married + income+hhsize+ numkids+ 
                  A_group+B_group+ C_group+w_loydisc_fb+
                w_loydisc_cat+ w_display+w_mailer, data=data1)
segment_3_camp_1=lm(weeklyspend~id+frequency+recency+spendingavg+D_2+calendarweek+age+
                store.shopped+married + income+hhsize+ numkids+ 
                  A_group+B_group+ C_group+w_loydisc_fb+
                w_loydisc_cat+ w_display+w_mailer, data=data2)
segment_3_camp_3=lm(weeklyspend~id+frequency+recency+spendingavg+D_2+calendarweek+age+
                store.shopped+married + income+hhsize+ numkids+ 
                A_group+B_group+ C_group+w_loydisc_fb+
                w_loydisc_cat+ w_display+w_mailer, data=data3)


stargazer(segment_3_camp_1,segment_3_camp_2,segment_3_camp_3,out = "segment3camp.html",single.row = TRUE)


#Kmeans for 4 clusters
set.seed(123)
kmeans_2 =bigkmeans(cluster_rfmd, 4,iter.max = 100,nstart = 1,dist = "euclid")
table(kmeans_2$cluster)
#plotting the clusters
fviz_cluster(kmeans_2, data = df_new_Final_rfmd, geom = c("point"),ellipse.type = "euclid")

df_new_Final_rfmd$index4=kmeans_2$cluster



Mode <- function(x, na.rm = FALSE) {if(na.rm){
  x = x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])}

clustervector = aggregate(index4~id,df_new_Final_rfmd,Mode)
table(clustervector)



householdVec = aggregate(index4~id,df_new_Final_rfmd,Mode)
table(householdVec)
n1 = subset(householdVec,index4==1)
n2 =subset(householdVec,index4==2)
n3 = subset(householdVec,index4==3)
n4 = subset(householdVec,index4==4)



data_4_1 = subset(df_new_Final_rfmd,id %in% n1$id)
data_4_2 =subset(df_new_Final_rfmd,id %in% n2$id)
data_4_3 =subset(df_new_Final_rfmd,id %in% n3$id)
data_4_4 =subset(df_new_Final_rfmd,id %in% n4$id)



segment_4_id_1=lm(weeklyspend~as.factor(id)+frequency+recency+spendingavg+D+D_2+calendarweek+age+
                    store.shopped+married + income+ numall_now+hhsize+
                    numkids+ A_group+B_group+ C_group+w_loydisc_fb+w_loydisc_cat+ 
                    w_display+w_mailer,data = data_4_1)
segment_4_id_2=lm(weeklyspend~as.factor(id)+frequency+recency+spendingavg+D+D_2+calendarweek+age+
                    store.shopped+married + income+ numall_now+hhsize+
                    numkids+ A_group+B_group+ C_group+w_loydisc_fb+w_loydisc_cat+ 
                    w_display+w_mailer,data = data_4_2)
segment_4_id_3=lm(weeklyspend~as.factor(id)+frequency+recency+spendingavg+D+D_2+calendarweek+age+
                    store.shopped+married + income+ numall_now+hhsize+
                    numkids+ A_group+B_group+ C_group+w_loydisc_fb+w_loydisc_cat+ 
                    w_display+w_mailer,data = data_4_3)
segment_4_id_4=lm(weeklyspend~id+frequency+recency+spendingavg+D+D_2+calendarweek+age+
                    store.shopped+married + income+ numall_now+hhsize+
                    numkids+ A_group+B_group+ C_group+w_loydisc_fb+w_loydisc_cat+ 
                    w_display+w_mailer,data = data_4_4)
stargazer(segment_4_id_1,segment_4_id_2,segment_4_id_3,segment_4_id_4,out = "segment4id.html",single.row = TRUE)



segment_4_camp_1=lm(weeklyspend~id+frequency+recency+spendingavg+D_2+calendarweek+age+
                      store.shopped+married + income+hhsize+ numkids+ 
                      A_group+B_group+ C_group+w_loydisc_fb+
                      w_loydisc_cat+ w_display+w_mailer, data=data_4_1)
segment_4_camp_2=lm(weeklyspend~id+frequency+recency+spendingavg+D_2+calendarweek+age+
                      store.shopped+married + income+hhsize+ numkids+ 
                      A_group+B_group+ C_group+w_loydisc_fb+
                      w_loydisc_cat+ w_display+w_mailer, data=data_4_2)
segment_4_camp_3=lm(weeklyspend~id+frequency+recency+spendingavg+D_2+calendarweek+age+
                      store.shopped+married + income+hhsize+ numkids+ 
                      A_group+B_group+ C_group+w_loydisc_fb+
                      w_loydisc_cat+ w_display+w_mailer, data=data_4_3)
segment_4_camp_4=lm(weeklyspend~id+frequency+recency+spendingavg+D_2+calendarweek+age+
                      store.shopped+married + income+hhsize+ numkids+ 
                      A_group+B_group+ C_group+w_loydisc_fb+
                      w_loydisc_cat+ w_display+w_mailer, data=data_4_4)
stargazer(segment_4_camp_1,segment_4_camp_2,segment_4_camp_3,segment_4_camp_4,out = "seg4camp.html",single.row = TRUE)


