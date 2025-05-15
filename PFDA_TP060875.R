# WONG_BIN_JIE
# TP060875

install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggalluvia")

library(ggplot2)
library(ggalluvia)
library(dplyr)

#Import data
pfc = read.csv("C:\\Users\\Asus\\Download\\Documents\\Notes\\PFDA\\Placement_Data_Full_Class.csv",header=TRUE)
pfc

# DATA EXPLORATION
summary(pfc)         # Summary of the data set
dim(pfc)             # Number of rols and cols
class(pfc)           # it is a data frame.
nrow(pfc)            # no of row (17007)
colSums(is.na(pfc))  # column that has missing value
sum(is.na(pfc))      # 8265 missing value
sum(duplicated(pfc)) # 0 duplicates data
str(pfc)             # basic info of data frame

#Assign Headers
names(pfc) = c("srl_no","sex","age","area","mum_edu","dad_edu","mum_job","dad_job","fam_supp",
               "tuition","curriculum","internet","sec_per","sec_boun","high_sec_per","high_sec_boun","high_sec_spec",
               "degree_per","degree_type","work_ex","emp_test_per","mba_spec","mba_per","placement_sta","salary")
View(pfc)

# DATA TRANSFORMATION
# convert categorical to numerical variable
pfc$sex <- replace(pfc$sex,pfc$sex == "F",1)
pfc$sex <- replace(pfc$sex,pfc$sex == "M",0)

#-----------------------------------------------------------------------------------------------------------------
# Question 1 Demographic
# Analysis: Number of get placed no get placed in job placement_stat.
# Calculate total of placed and not placed
total_placement_stat ‹- nrow(select(pfc,"placement_stat") %>% subset(placement_stat == "Placed")) total_no_placement_stat ‹- nrow(select(pfc,"placement_stat") %>% subset(placement_stat == "Not Placed"))

# Find the percentage
total ‹- total_placement_stat + total_no_placement_stat placement_stat <- total_placement_stat / total
no_placement_stat ‹- total_no_placement_stat / total

# Create data frame
placement_status ‹- data. frame(placement_status = c("Placed", "Not Placed"),
                                percentage = c(placement_stat, no_placement_stat))
# Draw bar chart
ggplot (placement_stat_stat, aes(x = placement_stat, y = percentage, fill=placement_stat)) +
  geom_bar (stat = "identity", color = "FOE8388"! +
              geom_text (aes (label = round (percentage, 3)), position = position_stack(vjust = 0.5) ,
                         size = 4, color = "white")+ ggtitle("Work placement_stat Status")

            
            
# Analysis 1.2 Are males or females more likely to get a placement_stat after completing the degree?
# Filter the data by sex (8443,8564)
total_male = nrow(select (pfc, "sex") %>% subset (sex = "0"))
total_female = nrow(select(pfc, "sex") %>% subset (sex == "1"))
totalstudent = total_male + total_female

# Calculate the proportion of males and females who got placed
male_placed_prop ‹- sum(pfc$sex = "0"&pfc$placement_stat == "Placed")
female_placed_prop <- sum(pfc$sex = "1"&pfcSplacement_stat = "Placed")
total_gender_placed ‹- male_placed_prop + female_placed_prop male_get ‹- male_placed_prop/total_gender_placed female_get‹- female_placed_prop/total_gender_placed

# Create data frame for pie chart
gender_stat <- data. frame(sex = c("Male", "Female"),Proportion = c(male_get, female_get))

# Create pie chart
ggplot (gender_stat, aes(x="''y=Proportion, fill=sex)) +
geom_bar (stat="identity", width=1, color="pink", alpha = 0.4) +
coord_polar (theta="y") +
ggtitle("Proportion of Students Who Got Placed by Gender")+ scale_fill_manual (values=c("yellow", 'green")) + theme_void +
              theme(plot. title = element_text(hjust=0.4, size=12, face="bold")) +
              geom_text (aes (label=Proportion*100), position = position_stack(vjust = 0.5) ,
                         size = 4, color ="black")
            


# Analysis 1.3 Compare the usability of internet access between rural area and urban area.
# Find the area that have internet access (4628,4089)
urban_yes = nrows(select(pfc,"area","internet") %>% subset(area == "U") %>% subset(internet == "yes")
urban_yes = nrows(select(pfc,"area","internet") %>% subset(area == "R") %>% subset(internet == "yes")
                                    
# Find the area that have no internet access (4166,4124)
urban_no = nrows(select(pfc,"area","internet") %>% subset(area == "U") %>% subset(internet == "no")
urban_no = nrows(select(pfc,"area","internet") %>% subset(area == "R") %>% subset(internet == "no")

# Find total
total-yes ‹- urban_yes + rural_yes  
total-no ‹- urban_no + rural_no

# Create data frame
df ‹- data. frame(
  internet urban urban eural, " Rural",
  count = c(urban_yes, urban_no, rural_yes, rural_no)
)

# Create dodged bar chart
ggplotdf, aes(x=area, y=count, fill=internet)) +
  geom_bar (stat="identity", position="dodge", width=0.8) + xlab("area") + ylab"Count") +
  ggtitle("internet Access by area") +
  scale_fill_manual (values=c("#1b9e77', "#d95f02"))+ 
  geom_text (aes (label=count),
    position=position_dodge(width=0.8), vjust=-0.5, size=3.5))



# Analysis 1.4 Find the percentage that has internet access in both Urban and Rural area. #8794#8213
# Total of internet access in both area
urban <- sum(pfcSarea == "U")
rural ‹- sum(pfcSarea == "R")

# Find the area that have internet access (4628,4089)
urban_yes = nrows(select(pfc,"area","internet") %>% subset(area == "U") %>% subset(internet == "yes")
urban_yes = nrows(select(pfc,"area","internet") %>% subset(area == "R") %>% subset(internet == "yes")
   
# Find percentage
internet_rural <- round(rural_yes / rural) * 100, 2)
internet_urban <- round (urban_yes / urran * 100, 2)

# Create data frame|
internet_access ‹- data. frame(internet_access = c("Rural", "Urban"), percentage = c(internet_rural,internet_urban))

# Draw bar chart
ggplot (internet_access, aes(x = internet_access, y = percentage,fill=internet_access)) +
ggtitle("internet Access in Rural Area and Urban Area") +
geom_bar (stat = "identity",color = "#0e8388") + xlab("Area"). + ylab("count") +
                       geom_text (aes (label = paste0(percentage, "%")),position = position_stack(vjust = 0.5),
                                  size = 4, color = "white")+ ggtitle("internet Access in Urban and Rural")
             

                     
# Analysis 1.5 Find the percentage that does not have internet in both Urban and Rural area.
urban ‹- sum(pfcSarea == "U")
rural ‹- sum(pfcSarea == "R")

# Find the area that have no internet access (4628,4089)
urban_no = nrows(select(pfc,"area","internet") %>% subset(area == "U") %>% subset(internet == "no")
urban_no = nrows(select(pfc,"area","internet") %>% subset(area == "R") %>% subset(internet == "no")
                                    
# Find percentage
internet_rural ‹- round(rural_no / rural * 100, 2) 
internet_urban ‹- round(urban_no / urban * 100, 2)

# Create data frame
internet_access <- data. frame(internet_access = c("Rural", "Urban"), percentage = c(internet_rural, internet_urban)
                               # Draw bar chart
                               ggplot (internet_access, aes(x = internet_access, y = percentage,fill=internet_access)) +
                                 ggtitle("No internet Access in Rural Area and Urban Area") +
                                 geom_bar (stat = "identity", color = "#0E8388") + xlab("Area") + ylab("count") +
geom_text (aes (label = paste (percentage, "%")) ,
position = position_stack(vjust = 0.5), size = 4, color = "white")+
ggtitle("No internet Access in Urban and Rural")

                               
                               
# Analysis 1.6 Will the students go for tuition if they have internet access?
# Subset the data to include only urban and rural area students data
rural ‹- subset(pfc, area == "R")

# Create contingency tables to cross-tabulate the "internet" and "tuition" variables for each area type
table_urban ‹- table(data_urbanSinternet, data_urbanStuition) 
table_rural ‹- table(data_ruralSinternet, data_ruralstuition)

# Calculate row and column percentages
row_perc_urban ‹- prop. table(table_urban, margin = 1)
col_perc_urban <- prop. table(table_urban, margin = 2)
rom_perc_rural ‹- prop. table(table_rural, margin = 1) 
col_perc_rural ‹- prop. table(table_rural, margin = 2)

# Print the contingency tables
cat ("Contingency table for urban area students:\n")
print (table_urban)
cat ("\nContingency table for rural area students:\n")
print (table_rural)                               

                               

# Analysis 1.7 Relationship between internet access and student score
# Find the student that has internet access and improvement
int_yes_imp = filter (pfc, internet = "yes") %*% select(internet, sec_per, high_sec_per)%>%
  mutate(mark_diff = high_sec_per - sec_per) %>%
  mutate(inty_improvement = (ifelse(mark_diff > 0, "yes", ifelse(mark_diff < 0, "no", "remain"))))
totaly_yes_imp = nrow(subset(int_yimp, inty_improvement == "yes"))
totaly_no_imp = nrow(subset(int_yimp, inty_improvement == "no"))
totaly_remain = nrow(subset(int_yimp, inty_improvement == "remain"))

# Find the students that have no internet access and improvement
int_no_imp = filter (pfc, internet = "no") %›% select(internet, sec_per,high_sec_per)%>%
  mutate(mark_diff = high_sec_per - sec_per) %>%
  mutate(intn_improvement = (ifelse(mark_diff » 0, "yes", ifelse(mark_diff < 0, "no", "remain"))))
totaln_yes_imp = nrow(subset(int_nimp, intn_improvement = "yes"))
totaln_no_nimp = nrow(subset(int_nimp, intn_improvement = "no"))
totaln_no_remain = nrow(subset (int_nimp, intn_improvement == "remain"))

a = round(totaly-yes_imp/nrow(pfc) * 100, 3)
b = round (totaly_no_imp/nrow(pfc) * 100, 3)
c = round (totaly_remain/nrow(pfc) * 100, 3)
d = round(totaln_yes_imp/nrow(pfc) * 100, 3)
e = round (totaln_no_nimp/nrow(pfc) * 100, 3)
f = round(totaln_no_remain/nrow(pfc) * 100, 3)
int_percentage = c(a, b, c, d, e, f)
int_def = c("Has internet, Has Improvement","Has internet, No Improvement","Has Internet, Result Remain",
            "No internet, Has Improvement","No internet, No Improvement","No Internet, Result Remain")
int_stat ‹- data.frame(int_percentage, Proportion = int_def)

# Create pie chart with percentage values
coord_polar (theta = "y") +
ggtitle("Relationship between internet access and student score") +
scale_fill_manual (values = c("#FF339E", "#3397FF", "#FF7533", "#86CA40","#CA7E40","#50736A"))+,
theme_void +
theme(plot.title=element_text(hjust=0.4,size=12,face='bold'))+
  geom_text(aes(label=paste0(int_percentage,"%")),position+position_stack(vjust=0.5),
size = 4, color = "black") +
labs (fill = "internet Access and Student Score")


# Analysis: Will the gender and age affect the employment status?
# Total number of male and female (8443,8564)
total_male <- nrow(subset(pfc, sex == "0"))
total_female <- nrow(subset (pfc, sex == "1"))

# Total number of male and female being placed (categorized by age)
# filter with sex and placement_stat, then select the column, count the row (4373,2193,2180)(4369,2192,2178)
totaly_mp = nrow(subset (pfc, sex == "0", select = C(sex, age, placement_stat))
                   %>% subset (placement_stat == "Placed"))
totaly_mp21 = nrow(subset (pfc, sex == "0", select = C(sex, age, placement_stat))
                   %>% subset (age >= 21) %>% subset (placement_stat == "Placed"))
totaly_mp20 = nrow(subset (pfc, sex == "0", select = C(sex, age, placement_stat))
                   %>% subset (age< 21) %>% subset (placement_stat == "Placed"))
totaly_fp = nrow(subset (pfc, sex == "1", select = c(sex, age, placement_stat)) %›% subset (placement_stat == "Placed"))
totaly_fp21 = nrow(subset (pfc, sex == "1", select = c(sex, age, placement_stat))
                   %>% subset (age >= 21) %>% subset (placement_stat == "Placed"))
totaly_fp20 = nrow(subset (pfc, sex == "1", select = c(sex, age, placement_stat))
                   %›% subset (age < 21)%›% subset (placement_stat = "Placed"))

# Total number of male and female not being placed (categorized by age)
# filter with sex and placement_stat, then select the column, count the row (4070,2032,2038)(4195,2049,2146)
totaln_mp = nrow(subset (pfc, sex == "0", select = c(sex, age, placement_stat))
                 %>% subset(placement_stat = "Not Placed"))
totaln_mp21 = nrow(subset (pfc, sex == "0", select = c(sex, age, placement_stat))
                   %>% subset (age >=21) %>% subset(placement_stat =='Not Placed"')
totaln_fp =   nrow(subset (pfc, sex == "1", select = c(sex, age, placement_stat))
                   %>% subset(placement_stat =='Not Placed"')                 
totaln_fp21 = nrow(subset (pfc, sex == "1", select = c(sex, age, placement_stat))
                   %>% subset (age >= 21) %*% subset (placement_stat == "Not Placed"))
totaln_fp20 = nrow(subset (pfc, sex == "1", select = c(sex, age, placement_stat))
                   %>% subset (age < 21)%>% subset (placement_stat == "Not Placed"))

df ‹- data. frame(
  sex = c("F","F","M","M"),
  age = c(">=21"，"<21",">=21","<21"），
  placement_stat = c("Placed", "Placed", "Placed" ,"Placed"),
  count = c(totaly_fp21, totaly_fp20,totaly_mp21, totaly_mp20)
)
df <- rbing(df,data.frame(
  sex = c("F","F","M","M"),
  age = c(">=21"，"<21",">=21","<21"），
          placement_stat = c("Not Placed", "Not Placed", "Not Placed" ,"Not Placed"),
          count = c(totaly_fp21, totaly_fp20,totaly_mp21, totaly_mp20)
  ))

# Create the stacked bar chart
ggplot(df, aes(x=age, y=count, fi11=placement_stat)) +
geom_bar (stat="identity", position="stack'") +
geom_text (aes (label = ifelse(count = 0, "", count)),
position = position_stack(vjust = 0.5))+
  facet_grid(.~sex) +
  xlab("Age Category") + ylab"Number of Students") +
  ggtitle("placement_stat Status by Age and Gender") + scale_fill_manual (values=c("#1EE3BA", "#6EFFE2")) # custom color palette


# Analysis 1.9: Relationship between gender and salary
data <- pfc %>%
  select (sex, sal) %›% na. omit() %>%
  mutate(sal_category = cut(sal, breaks = c(199999, 299999, 399999, 500000),
                            labels = c("200000-299999", "300000-399999", "400000-500000"))) %>%
  count (sex, sal_category)

ggplot(data,aes(x=sal_category,y=n,fill=sex)) + coord_flip()+
geom_text (aes (label=n), position = position_dodge(width=0.8))
labs (x = "Salary", y = "Count") +|
theme(
aspect. ratio = 0.75, # adjust aspect ratio
strip. text.y = element_text(size = rel(0.8))) # adjust font size of facet labels 





# Question 2 Education
# Analysis 2.1 Relationship between degree type and sal received by the student
type <- pfc %>% group_by(degree_type) %>% summarize()
df_summary <- pfc %>% select(degree_type,sal)%>%
  group_by(degree_type)%>%na.omit(sal)%>%
summarize(count=n())

# Create the box plot using ggplot2
ggplot(pfc, aes(x = degree_type, y = sal, fill = degree_type)) +
  geom_boxplot() + scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("#FFA500", "#00CED1", "#FF69B4")) +
  labs(title = "sal by Degree Type", x = "Degree Type",y = "sal")


# Analysis 2.2: Find the student that has been placed based on the degree type.
# Find student that has been placed
comm_yes <- nrow(select(pfc,"placement_stat","degree_type") %>% subset(placement_stat == "Placed")
                 %>% subset(degree_type == "Comm&Mgmt"))
sci_yes <- nrow(select(pfc,"placement_stat","degree_type") %>% subset(placement_stat == "Placed")
                %>% subset(degree_type == "Sci&Tech"))
others_yes <- nrow(select(pfc,"placement_stat","degree_type") %>% subset(placement_stat == "Placed")
                   %>% subset(degree_type == "Others"))

# Find student that has not been placed
comm_no <- nrow(select(pfc,"placement_stat","degree_type") %>% subset(placement_stat == "Not Placed")
                %>% subset(degree_type == "Comm&Mgmt"))
sci_no <- nrow(select(pfc,"placement_stat","degree_type") %>% subset(placement_stat == "Not Placed")
               %>% subset(degree_type == "Sci&Tech"))
others_no <- nrow(select(pfc,"placement_stat","degree_type") %>% subset(placement_stat == "Not Placed")
                  %>% subset(degree_type == "Others"))

# Find total of being placed and not placed
total_yes <- comm_yes+sci_yes+others_yes
total_no <- comm_no+sci_no+others_no
x <- pfc %>%
  select(degree_type,placement_stat) %>%
  group_by(degree_type,placement_stat) %>%
  count()

# Create dodged bar chart
ggplot(x, aes(x=degree_type, y=n, fill=placement_stat)) +
  geom_bar(stat="identity", position="dodge", width=0.9) +
  xlab("Degree Type") +
  ylab("Count") +
  ggtitle("placement_stat Status") +
  scale_fill_manual(values=c("#FFF176", "#d95f02","#1b9e77"))+
  geom_text(aes(label=n), position=position_dodge(width=0.8),vjust=-0.5,size=3.5)


# Analysis 2.3: Which course will the student take in their degree based on their high school?
pfc %>% select(high_sec_spec, degree_type) %>% count(high_sec_spec,degree_type)

# Count the number of students for each combination of HSC_S and degree_type
count_data <- pfc %>%
  group_by(high_sec_spec, degree_type) %>%
  summarise(n = n())
ggplot(count_data, aes(x = high_sec_spec, y = n, fill = degree_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Specialization in High School against Field of Degree Education",
       x = "High School", y = "Number of Students", fill = "Degree Type") +
  theme_minimal() + geom_text(aes(label=n), position=position_dodge(width=0.8),vjust=-0.5,size=3.5)


# Analysis 2.4: The student secondary and high school (percentage).
sec <- pfc %>% select(sec_per,high_sec_per) %>% arrange(sec_per)
high <- pfc %>% select(sec_per,high_sec_per) %>% arrange(high_sec_per)
high_h1 <- pfc %>% select (high_sec_per,degree_per) %>% arrange(high)
degree <- pfc %>% select (high_sec_per,degree_per) %>% arrange(degree_per)
ggplot(data = sec, aes(x = sec_per,y=high_sec_per)) +
  geom_area(fill = "blue", alpha = .3) +
  geom_line(color = "black") +
  scale_x_continuous(breaks = c(30, 40, 50, 60,70,80,90,100))+
  labs(title = "Relationship between Secondary vs High Secondary Percentage",
       x = "Secondary School Percentage",
       y = "High School Percentage")
ggplot(data = high_h1, aes(x = high_sec_per, y = degree_per)) +
  geom_area(fill = "skyblue", alpha = .3) +
  geom_line(color = "black") +
  scale_x_continuous(breaks = c(30, 40, 50, 60,70,80,90,100))+
  labs(title = "High Secondary vs Degree Percentage",
       x = "High School Percentage",
       y = "Degree Percentage")


# Analysis 2.5 Will the degree percentage affect the student salary?
data = select(pfc, c("degree_per", "sal")) %>% arrange(degree_per) %>% na.omit()
ggplot(data, aes(x = degree_per, y = sal)) + geom_point(aes(color = sal)) +
  scale_color_gradient(low = "blue", high = "skyblue")



# Analysis 2.6 Will the degree type and degree percentage affect the sal?
deg_per = which(colnames(pfc)=="degree_per" )
deg_type = which(colnames(pfc)=="degree_type")

# Comm&Mgmt
deg_t = pfc[(pfc$degree_type == "Comm&Mgmt"), c(deg_per, deg_per, which(colnames(pfc)=="sal" ))]
no_deg_t = deg_t[!is.na(deg_t$sal), ]

# Find the sal based on the percentage
# for-loop over columns
for(degree_per in no_deg_t) {
  cm_80 = ifelse(no_deg_t$degree_per >= 80, print(no_deg_t$sal) , print(NA) )
  cm_60 = ifelse(no_deg_t$degree_per < 80 & no_deg_t$degree_per >= 60, print(no_deg_t$sal) , print(NA))
  cm_40 = ifelse(no_deg_t$degree_per < 60 & no_deg_t$degree_per >= 40, print(no_deg_t$sal) , print(NA))
  cm_0 = ifelse(no_deg_t$degree_per < 40, print(no_deg_t$sal) , print(NA) )
}
summary(na.omit(cm_80))
summary(na.omit(cm_60))
summary(na.omit(cm_40))
summary(na.omit(cm_0))
# Create box plot
miny = 200000
maxy = 500000
par(mfrow = c(1, 4))
boxplot((na.omit(cm_80)), main = "80-100", ylab = "sal", col = "Red", ylim = c(miny, maxy))
boxplot((na.omit(cm_60)), main = "60-79", ylab = "sal", col = "Yellow", ylim = c(miny, maxy))
boxplot((na.omit(cm_40)), main = "40-59", ylab = "sal", col = "Green", ylim = c(miny, maxy))
boxplot((na.omit(cm_0)), main = "0-39", ylab = "sal", col = "Green", ylim = c(miny, maxy))

# Sci&Tech
deg_t = pfc[(pfc$degree_type == "Sci&Tech"), c(deg_per, deg_per, which(colnames(pfc)=="sal" ))]
no_deg_t = deg_t[!is.na(deg_t$sal), ]
# Find the sal based on the percentage
# for-loop over columns
for(degree_per in no_deg_t) {
  sm_80 = ifelse(no_deg_t$degree_per >= 80, print(no_deg_t$sal) , print(NA) )
  sm_60 = ifelse(no_deg_t$degree_per < 80 & no_deg_t$degree_per >= 60, print(no_deg_t$sal) , print(NA))
  sm_40 = ifelse(no_deg_t$degree_per < 60 & no_deg_t$degree_per >= 40, print(no_deg_t$sal) , print(NA))
  sm_0 = ifelse(no_deg_t$degree_per < 40, print(no_deg_t$sal) , print(NA) )
}
summary(na.omit(sm_80))
summary(na.omit(sm_60))
summary(na.omit(sm_40))
summary(na.omit(sm_0))

# Create box plot
miny = 200000
maxy = 500000
par(mfrow = c(1, 4))
boxplot((na.omit(sm_80)), main = "80-100", ylab = "sal", col = "Blue", ylim = c(miny, maxy))
boxplot((na.omit(sm_60)), main = "60-79", ylab = "sal", col = "Yellow", ylim = c(miny, maxy))
boxplot((na.omit(sm_40)), main = "40-59", ylab = "sal", col = "Green", ylim = c(miny, maxy))
boxplot((na.omit(sm_0)), main = "0-39", ylab = "sal", col = "Green", ylim = c(miny, maxy))

# Other
deg_t = pfc[(pfc$degree_type == "Others"), c(deg_per, deg_per, which(colnames(pfc)=="sal" ))]
no_deg_t = deg_t[!is.na(deg_t$sal), ]  #Drop the row with null value in the "sal" column.

# Find the sal based on the percentage
# for-loop over columns
for(degree_per in no_deg_t) {
  o_80 = ifelse(no_deg_t$degree_per >= 80, print(no_deg_t$sal) , print(NA) )
  o_60 = ifelse(no_deg_t$degree_per < 80 & no_deg_t$degree_per >= 60, print(no_deg_t$sal) , print(NA))
  o_40 = ifelse(no_deg_t$degree_per < 60 & no_deg_t$degree_per >= 40, print(no_deg_t$sal) , print(NA))
  o_0 = ifelse(no_deg_t$degree_per < 40, print(no_deg_t$sal) , print(NA) )
}
summary(na.omit(o_80))
summary(na.omit(o_60))
summary(na.omit(o_40))
summary(na.omit(o_0))

# Create box plot
miny = 200000
maxy = 500000
par(mfrow = c(1, 4))
boxplot((na.omit(o_80)), main = "80-100", ylab = "sal", col = "Blue", ylim = c(miny, maxy))
boxplot((na.omit(o_60)), main = "60-79", ylab = "sal", col = "Yellow", ylim = c(miny, maxy))
boxplot((na.omit(o_40)), main = "40-59", ylab = "sal", col = "Green", ylim = c(miny, maxy))
boxplot((na.omit(o_0)), main = "0-39", ylab = "sal", col = "Green", ylim = c(miny, maxy))


# Analysis 2.7 Relationship between MBA specialization and degree percentage against sal.
pfc %>% na.omit(sal) %>% sample_frac(.03) %>%
  ggplot(aes(x = degree_per, y = mba_per, size = sal, color = sal)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(1, 10)) +
  ggtitle("Bubble Diagram") + xlab("Degree Percentage") + ylab("MBA Percentage") +
  theme_bw()
pfc %>% select(degree_per, mba_per, sal) %>% arrange(degree_per)
pfc %>% select(degree_per, mba_per, sal) %>% arrange(mba_per)
pfc %>% select(degree_per, mba_per, sal) %>% arrange(sal)




# Question 3 Academic Performance
# Analysis 3.1: Relationship between Degree Type and curriculum
got_act_com <- nrow(select(pfc,"curriculum","degree_type")
                    %>% subset(curriculum == "yes")
                    %>% subset(degree_type =="Comm&Mgmt"))
got_act_sci <- nrow(select(pfc,"curriculum","degree_type")
                    %>% subset(curriculum == "yes")
                    %>% subset(degree_type =="Sci&Tech"))
got_act_oth <- nrow(select(pfc,"curriculum","degree_type")
                    %>% subset(curriculum == "yes")
                    %>% subset(degree_type =="Others"))
total_got <- got_act_com + got_act_sci + got_act_oth
no_act_com <- nrow(select(pfc,"curriculum","degree_type")
                   %>% subset(curriculum == "no") %>% subset(degree_type =="Comm&Mgmt"))
no_act_sci <- nrow(select(pfc,"curriculum","degree_type")
                   %>% subset(curriculum == "no") %>% subset(degree_type =="Sci&Tech"))
no_act_oth <- nrow(select(pfc,"curriculum","degree_type")
                   %>% subset(curriculum == "no") %>% subset(degree_type =="Others"))
total_no <- no_act_com + no_act_sci + no_act_oth
x <- pfc %>% select(degree_type,curriculum) %>% group_by(degree_type,curriculum) %>% count()
ggplot(x,aes(x=degree_type,y=n,fill=curriculum)) +
  geom_bar(stat= "identity", position = "dodge",width=0.9) +
  xlab("Degree Type") +
  ylab("Count") +
  ggtitle("Relationship between Degree Type and curriculum") +
  scale_fill_manual(values=c("#75e8e7","#f5487f")) +
  geom_text(aes(label=n),position=position_dodge(width=0.8),vjust=-0.5,size =3.5)



# Analysis 3.2: Relationship between High School and curriculum
got_act_com <- nrow(select(pfc,"curriculum","high_sec_spec")%>%
                      subset(curriculum == "yes") %>% subset(high_sec_spec =="Science"))
got_act_sci <- nrow(select(pfc,"curriculum","high_sec_spec")%>%
                      subset(curriculum == "yes") %>% subset(high_sec_spec =="Commerce"))
got_act_oth <- nrow(select(pfc,"curriculum","high_sec_spec")  %>%
                      subset(curriculum == "yes")%>% subset(high_sec_spec =="Arts"))
total_got <- got_act_com + got_act_sci + got_act_oth
no_act_com <- nrow(select(pfc,"curriculum","high_sec_spec")
                   %>% subset(curriculum == "no") %>% subset(high_sec_spec =="Science"))
no_act_sci <- nrow(select(pfc,"curriculum","high_sec_spec")
                   %>% subset(curriculum == "no") %>% subset(high_sec_spec =="Commerce"))
no_act_oth <- nrow(select(pfc,"curriculum","high_sec_spec")
                   %>% subset(curriculum == "no") %>% subset(high_sec_spec =="Arts"))
total_no <- no_act_com + no_act_sci + no_act_oth
x <- pfc %>% select(high_sec_spec,curriculum) %>% group_by(high_sec_spec,curriculum) %>% count()
ggplot(x,aes(x=high_sec_spec,y=n,fill=curriculum)) +
  geom_bar(stat= "identity", position = "dodge",width=0.7) +
  xlab("Hgih Secondary School Specialization") +
  ylab("Count") +
  ggtitle("Relationship between Specialization in High Secondary School and curriculum") +
  scale_fill_manual(values=c("#fdfa66","#f5487f") )+
  geom_text(aes(label=n),position=position_dodge(width=0.8),vjust=-0.5,size =3.5)


# Analysis 3.3: Find whether the student has improvement if he participates extra-curricular activities.
#Find the student that join extra curriculum and improvement
act_yes_imp = filter(pfc, curriculum == "yes") %>% select(curriculum,sec_per,high_sec_per) %>%
  mutate(act_Differences_y = high_sec_per - sec_per) %>%
  mutate(act_Improvement_y = (ifelse(act_Differences_y > 0, "yes",
                                     ifelse(act_Differences_y < 0, "no", "remain"))))
totaly_yes_imp = nrow(subset(act_yes_imp, act_Improvement_y == "yes"))
totaly_no_imp = nrow(subset(act_yes_imp, act_Improvement_y == "no"))
totaly_remain_imp = nrow(subset(act_yes_imp, act_Improvement_y == "remain"))

#Find the student that join extra curriculum but no improved
act_no_imp = filter(pfc, curriculum == "no") %>% select(curriculum,sec_per,high_sec_per) %>%
  mutate(act_Differences_n = high_sec_per - sec_per) %>%
  mutate(act_Improvement_n = (ifelse(act_Differences_n > 0, "yes",
                                     ifelse(act_Differences_n < 0, "no", "remain"))))
totaln_yes_imp = nrow(subset(act_no_imp, act_Improvement_n == "yes"))
totaln_no_imp = nrow(subset(act_no_imp, act_Improvement_n == "no"))
totaln_remain_imp = nrow(subset(act_no_imp, act_Improvement_n == "remain"))
a = round(totaly_yes_imp/nrow(pfc) * 100, 3)
b = round(totaly_no_imp/nrow(pfc) * 100, 3)
c = round(totaly_remain_imp/nrow(pfc) * 100, 3)
d = round(totaln_yes_imp/nrow(pfc) * 100, 3)
e = round(totaln_no_imp/nrow(pfc) * 100, 3)
f = round(totaln_remain_imp/nrow(pfc) * 100, 3)
act_percentage = c(a, b, c, d, e, f)
act_def = c("Curricular Yes, Improvement Yes", "Curricular Yes, Improvement No", "Curricular Yes, Result Maintain",
            "Curricular No, Improvement Yes", "Curricular No, Improvement No", "Curricular No, Result Maintain")
act_stat <- data.frame(act_percentage, Proportion = act_def)
# Create pie chart with percentage values
ggplot(act_stat, aes(x = "", y = act_percentage, fill = act_def)) +
  geom_bar(stat = "identity", width = 1.5, color = "white") +
  coord_polar(theta = "y") +
  ggtitle("Relationship between extra-curriculum and student score") +
  scale_fill_manual(values = c("#ffff4d", "#ffc14d", "#ea9e62", "#ff4d4d", "#ff6680","#ff4dff")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.4, size = 12, face = "bold")) +
  geom_text(aes(label = paste0(act_percentage, "%")), position = position_stack(vjust = 0.5),
            size = 4, color = "black") +
  labs(fill = "Extra Curricular Activities and Student Score")



# Analysis 3.4: Find whether student has improvement if he attends extra paid tuition.
#Find the student that go for tuition and his result
tui_yes_imp = filter(pfc, tuition == "yes") %>% select(tuition,sec_per,high_sec_per) %>%
  mutate(tui_Diff_y = high_sec_per - sec_per) %>%
  mutate(tui_Improvement_y = (ifelse(tui_Diff_y > 0, "yes",
                                     ifelse(tui_Diff_y < 0, "no", "remain"))))
totaly_yes_imp = nrow(subset(tui_yes_imp, tui_Improvement_y == "yes"))
totaly_no_imp = nrow(subset(tui_yes_imp, tui_Improvement_y == "no"))
totaly_remain_imp = nrow(subset(tui_yes_imp, tui_Improvement_y == "remain"))

#Find the student that no go tuition and his result
tui_no_imp = filter(pfc, tuition == "no") %>% select(tuition,sec_per,high_sec_per) %>%
  mutate(tui_Diff_n = high_sec_per - sec_per) %>%
  mutate(tui_Improvement_n = (ifelse(tui_Diff_n > 0, "yes",
                                     ifelse(tui_Diff_n < 0, "no", "remain"))))
totaln_yes_imp = nrow(subset(tui_no_imp, tui_Improvement_n == "yes"))
totaln_no_imp = nrow(subset(tui_no_imp, tui_Improvement_n == "no"))
totaln_remain_imp = nrow(subset(tui_no_imp, tui_Improvement_n == "remain"))
a = round(totaly_yes_imp/nrow(pfc) * 100, 3)
b = round(totaly_no_imp/nrow(pfc) * 100, 3)
c = round(totaly_remain_imp/nrow(pfc) * 100, 3)
d = round(totaln_yes_imp/nrow(pfc) * 100, 3)
e = round(totaln_no_nimp/nrow(pfc) * 100, 3)
f = round(totaln_remain_imp/nrow(pfc) * 100, 3)
tui_percentage = c(a, b, c, d, e, f)
tui_def = c("Has Tuition, Has Improvement", "Has Tuition, No Improvement", "Has Tuition, Result Remain",
            "No Tuition, Has Improvement", "No Tuition, No Improvement", "No Tuition, Remain Result")
tui_stat <- data.frame(tui_percentage, Proportion = tui_def)

# Create pie chart with percentage values
ggplot(act_stat, aes(x = "", y = tui_percentage, fill = tui_def)) +
  geom_bar(stat = "identity", width = 1.5, color = "white") +
  coord_polar(theta = "y") +
  ggtitle("Relationship between extra paid tuition and student score") +
  scale_fill_manual(values = c("#FF339E", "#3397FF", "#FF7533", "#86CA40", "#CA7E40","#50736A")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.4, size = 12, face = "bold")) +
  geom_text(aes(label = paste0(tui_percentage, "%")), position = position_stack(vjust = 0.5),
            size = 4, color = "black") +
  labs(fill = "Extra Paid Tuition and Student Score")


# Analysis 3.5: Relationship between family support and curriculum activities
fam_yes = nrow(select(pfc,"fam_supp","curriculum") %>%
                 subset(fam_supp == "yes") %>% subset(curriculum == "yes"))
cur_yes = nrow(select(pfc,"fam_supp","curriculum") %>%
                 subset(fam_supp == "no") %>% subset(curriculum == "yes"))
fam_no = nrow(select(pfc,"fam_supp","curriculum") %>%
                subset(fam_supp == "yes") %>% subset(curriculum == "no"))
cur_no = nrow(select(pfc,"fam_supp","curriculum") %>%
                subset(fam_supp == "no") %>% subset(curriculum == "no"))
total_yes <- fam_yes + cur_yes
total_no <- cur_yes + cur_no

# Create data frame
df <- data.frame(
  fam_supp = c("Support","Support","No Support", "No Support"),
  curriculum = c("Yes","No","Yes", "No"),
  count = c(fam_yes, cur_yes,cur_yes ,cur_no))

# Create dodged bar chart
ggplot(df, aes(x=fam_supp, y=count, fill=curriculum)) +
  geom_bar(stat="identity", position="dodge", width=0.8) +
  xlab("area") + ylab("Count") +
  ggtitle("Relationship between family support and curriculum acitivities") +
  scale_fill_manual(values=c("violet", "brown"))+
  geom_text(aes(label=count),
            position=position_dodge(width=0.8), vjust=-0.5,  size=3.5)



# Question 4 Experimental
# Analysis 4.1: Relationship between employment test and placement_stat
placed <-  pfc %>% select(emp_test_per,placement_stat) %>% subset(placement_stat == "Placed")
not_placed <- pfc %>% select(emp_test_per,placement_stat) %>% subset(placement_stat == "Not Placed")
group <-  rbind(placed,not_placed)
ggplot(group,aes(x=placement_stat,y=emp_test_per)) +
  geom_violin(fill='pink') +
  geom_boxplot(width = .2, fill = 'grey', outlier.color = 'red', outlier.size = .2) +
  labs(title = "placement_stat based on Employment Test")


# Analysis 4.2: Does the student with work experience easier to get a placement_stat?
  # find the sum of work experience based on the student placement_stat
  placement_stat <- pfc %>%
  select(work_ex, placement_stat) %>%
  group_by(placement_stat, work_ex) %>%
  count()
ggplot(placement_stat, aes(x = work_ex, y = n, fill = placement_stat)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency Count of placement_stat by Work Experience",
       x = "Work Experience",
       y = "Frequency Count",
       fill = "placement_stat")


# Analysis 4.3： Relationship between sex, degree type and placement_stat
# Alternative alluvial diagram
a = pfc%>%select(sex,degree_type,placement_stat) %>% group_by(sex,degree_type,placement_stat) %>% count()
ggplot(a, aes(axis1=sex, axis2=degree_type, axis3=placement_stat, y=n)) +
  geom_alluvium(aes(fill=sex)) +
  geom_stratum() +
  geom_text(stat="stratum", aes(label=after_stat(stratum)), size=3) +
  scale_x_discrete(limits = c("sex", "degree_type", "placement_stat"), expand = c(.1, .1)) +
  scale_fill_viridis_d() +
  labs(title = "Relationship between",
       subtitle = "sex, degree type and placement_stat status",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")





# Question 5 Education Background
# Analysis 5.1  How does the level of education of parents relate to the
# specializations that students choose?
a=pfc %>% select(mum_edu, dad_edu, degree_type) %>% filter(mum_edu == 4 | dad_edu == 4) %>%
  group_by(degree_type) %>% count()
ggplot(a,aes(x = degree_type, y = n, fill = degree_type)) +
  geom_col() +
  scale_fill_manual(values= c("#F73667", "#56B4E9", "#009E73")) +
  geom_text(aes(label=n),position = position_stack(vjust = 0.5),
            size = 4, color = "black") +
  ggtitle("Count of Students in Each Degree Type Based on Parents' Education Level") +
  xlab("Degree Type") + ylab("Count")


# Analysis 5.2 How does the job status of parents relate to the specializations
# that students choose?
a=pfc %>%
  select(mum_job, dad_job, degree_type) %>%
  filter(degree_type != "Others") %>%
  group_by(degree_type) %>%
  count( mum_job, dad_job) %>% arrange(n)
b=pfc %>%
  select(mum_job, dad_job, degree_type) %>%
  filter(degree_type != "Others") %>%
  group_by(degree_type) %>%
  count( mum_job, dad_job) %>% arrange(desc(n))
pfc %>%
  select(mum_job, dad_job, degree_type) %>%
  filter(degree_type != "Others") %>%
  group_by(mum_job, dad_job, degree_type) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = mum_job, y = dad_job, fill = count)) +
  geom_tile() +
  facet_wrap(~ degree_type, scales = "free") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Mother's job", y = "Father's job", fill = "Count",
       title = "Counts of Parental Jobs by Degree Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Analysis 5.3 Will mother job affect the student specialization when entering degree?
#categorized based on degree type
pfc %>%
  select(mum_job, degree_type) %>% filter(degree_type != 'Others') %>%
  group_by(degree_type, mum_job) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = degree_type, y = count, fill = mum_job)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width=0.8), vjust=3.0) +
  labs(x = "Degree Type", y = "Count", fill = "Mother's Job",
       title = "Counts of Mother's Job by Degree Type") +
  theme_bw()



# Analysis 5.4 Is family support play an important roles in student getting higher
# mark in high school?
pfc %>%
  select(fam_supp, high_sec_per) %>%
  ggplot(aes(x = high_sec_per, fill = fam_supp)) +
  geom_density(alpha = 0.5) +
  labs(x = "High School Percentage", y = "Density", fill = "Family Support",
       title = 'Family Support vs High School Percentage')


# Analysis 5.5 Will the parents with same job affect student high school result?
  high_sec_mean = mean(pfc$high_sec_per)
same_data = pfc %>% select(mum_job,dad_job,high_sec_per) %>%
  mutate(same = dad_job == mum_job) %>%
  subset(same == TRUE) %>% count(high_sec_per)
same_cat <- same_data %>%
  mutate(high_p_cat =
           cut(high_sec_per, breaks = c(0, 39, 44.99, 49.99, 54.99, 59.99, 64.99, 69.99, 74.99, 79.99, 84.99, 89.99, 94.99, 100),
               labels = c("0-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-100"))) %>%
  group_by(high_p_cat) %>% count()
ggplot(same_cat, aes(x = high_p_cat, y = n, group = 1)) +
  geom_line() +
  labs(x = "High School Percentage Category (%)", y = "Count", title = "Count of High School Percentage by Category (Same Parent Job)") +
  theme_bw()
