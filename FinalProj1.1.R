library(tidyverse)
library(readxl)
library(janitor)
library(easystats)
library(broom)

df <- read_xlsx("AnalysisFinalProj.xlsx","demographics ")
df1 <- read_xlsx("AnalysisFinalProj.xlsx","Geographical")
df2 <- read_xlsx("AnalysisFinalProj.xlsx","Illegal")
df3 <- read_xlsx("AnalysisFinalProj.xlsx","demographics2")
df4 <- read_xlsx("AnalysisFinalProj.xlsx","Geographical2")

##########################################################################
#cleaned geographic and socioeconomic sheet in thousands a.k.a Finalclean1
##########################################################################
df1<- df1 %>% 
  filter(`Geo/socieconomic` !="TOTAL") %>% 
  filter(str_detect(`Geo/socieconomic`,"or",negate = TRUE))
df1[df1 =="*"] <- NA

clean1 <- df1 %>% 
  pivot_longer(starts_with("12+"),
               names_to = "YearA",
               values_to = "12+" )%>% 
  mutate(YearA= str_replace_all(YearA,"12+...2", "2019")) %>% 
  mutate(YearA= str_replace_all(YearA,"12+...3", "2020")) %>% 
  pivot_longer(starts_with("12-17"),
               names_to = "YearB",
               values_to = "12-17")%>%  
  mutate(YearB = str_replace_all(YearB,"12-17...4","2019")) %>% 
  mutate(YearB = str_replace_all(YearB,"12-17...5","2020")) %>% 
  mutate(YearB = as.numeric(YearB)) %>% 
  pivot_longer(starts_with("18+"),
               names_to = "YearC",
               values_to = "18+") %>% 
  mutate(YearC = str_replace_all(YearC,"18+...6","2019")) %>% 
  mutate(YearC = str_replace_all(YearC,"18+...7","2020")) %>% 
  pivot_longer(starts_with("18-25"),
               names_to = "YearD",
               values_to = "18-25" )%>% 
  mutate(YearD= str_replace_all(YearD,"18-25...8", "2019")) %>% 
  mutate(YearD= str_replace_all(YearD,"18-25...9", "2020")) %>% 
  pivot_longer(starts_with("26+"),
               names_to = "YearE",
               values_to = "26+") %>% 
  mutate(YearE = str_replace_all(YearB,"26+...10","2019")) %>% 
  mutate(YearE = str_replace_all(YearB,"26+...11","2020")) %>% 
  mutate(YearE = as.numeric(YearE))

Finalclean1 <- clean1 %>% 
   mutate(YearA = case_when(YearA == "12+...2" ~ "2019",
                            YearA == "12+...3" ~ "2020")) %>%
   mutate(YearA = as.numeric(YearA)) %>% 
   mutate(YearC = case_when(YearC == "18+...6" ~ "2019",
                            YearC == "18+...7" ~ "2020")) %>% 
   mutate(YearC = as.numeric(YearC)) %>%
   mutate(YearD = as.numeric(YearD)) %>% 
   pivot_longer(starts_with("Year"), names_to = "YearX",values_to = "Year") %>% 
   pivot_longer(c("12+","12-17","18+","18-25","26+"),names_to = "AgeGroup",values_to = "Users") %>% 
   mutate(AgeGroup = str_replace_all(AgeGroup,"26\\+","26&up")) %>% 
   filter(str_detect(AgeGroup, "\\+",negate = TRUE)) %>% 
   select(-YearX) %>% 
   unique.data.frame()

##########################################################################
#cleaned geographic and socioeconomic sheet percentages a.k.a Finalclean4
##########################################################################
df4<- df4 %>% 
   filter(`Geo/socieconomic` !="TOTAL") %>% 
   filter(str_detect(`Geo/socieconomic`,"or",negate = TRUE))
df4[df4 =="*"] <- NA

clean4 <- df4 %>% 
   pivot_longer(starts_with("12+"),
                names_to = "YearA",
                values_to = "12+" )%>% 
   mutate(YearA= str_replace_all(YearA,"12+...2", "2019")) %>% 
   mutate(YearA= str_replace_all(YearA,"12+...3", "2020")) %>% 
   pivot_longer(starts_with("12-17"),
                names_to = "YearB",
                values_to = "12-17")%>%  
   mutate(YearB = str_replace_all(YearB,"12-17...4","2019")) %>% 
   mutate(YearB = str_replace_all(YearB,"12-17...5","2020")) %>% 
   mutate(YearB = as.numeric(YearB)) %>% 
   pivot_longer(starts_with("18+"),
                names_to = "YearC",
                values_to = "18+") %>% 
   mutate(YearC = str_replace_all(YearC,"18+...6","2019")) %>% 
   mutate(YearC = str_replace_all(YearC,"18+...7","2020")) %>% 
   pivot_longer(starts_with("18-25"),
                names_to = "YearD",
                values_to = "18-25" )%>% 
   mutate(YearD= str_replace_all(YearD,"18-25...8", "2019")) %>% 
   mutate(YearD= str_replace_all(YearD,"18-25...9", "2020")) %>% 
   pivot_longer(starts_with("26+"),
                names_to = "YearE",
                values_to = "26+") %>% 
   mutate(YearE = str_replace_all(YearB,"26+...10","2019")) %>% 
   mutate(YearE = str_replace_all(YearB,"26+...11","2020")) %>% 
   mutate(YearE = as.numeric(YearE))

Finalclean4 <- clean4 %>% 
   mutate(YearA = case_when(YearA == "12+...2" ~ "2019",
                            YearA == "12+...3" ~ "2020")) %>%
   mutate(YearA = as.numeric(YearA)) %>% 
   mutate(YearC = case_when(YearC == "18+...6" ~ "2019",
                            YearC == "18+...7" ~ "2020")) %>% 
   mutate(YearC = as.numeric(YearC)) %>%
   mutate(YearD = as.numeric(YearD)) %>% 
   pivot_longer(starts_with("Year"), names_to = "YearX",values_to = "Year") %>% 
   pivot_longer(c("12+","12-17","18+","18-25","26+"),names_to = "AgeGroup",values_to = "Users") %>% 
   mutate(AgeGroup = str_replace_all(AgeGroup,"26\\+","26&up")) %>% 
   filter(str_detect(AgeGroup, "\\+",negate = TRUE)) %>% 
   select(-YearX) %>% 
   unique.data.frame()


########################################
#cleaned ages broken down a.k.a Clean2
########################################
df2<- df2 %>% 
  filter(Age !="TOTAL") %>% 
  filter(str_detect(Age,"or",negate = TRUE))
df2[df2 =="*"] <- NA

 clean2 <- df2 %>% 
   pivot_longer(starts_with("Lifetime"),
                names_to = "Year",
                values_to = "Lifetime_use",
                names_prefix = "Lifetime_")%>% 
   mutate(Year= str_replace_all(Year,"Lifetime...2", "2019")) %>% 
   mutate(Year= str_replace_all(Year,"Lifetime...3", "2020")) %>% 
   pivot_longer(starts_with("Past Year"),
                names_to = "YearB",
                values_to = "Past_Year_Use") %>% 
   mutate(YearB = str_replace_all(YearB,"Past Year...4","2019")) %>% 
   mutate(YearB = str_replace_all(YearB,"Past Year...5","2020")) %>% 
   mutate(YearB = as.numeric(YearB)) %>% 
   pivot_longer(starts_with("Past Month"),
                names_to = "YearC",
                values_to = "Past_Month_Use") %>% 
   mutate(YearC = str_replace_all(YearC,"Past Month...6","2019")) %>% 
   mutate(YearC = str_replace_all(YearC,"Past Month...7","2020")) %>% 
   mutate(YearC = as.numeric(YearC))

 #####################################
 #cleaned demographic a.k.a Finalclean
 #####################################
 df<- df %>% 
   filter(Demograph !="TOTAL") %>% 
   filter(str_detect(Demograph,"or",negate = TRUE))
 df[df =="*"] <- NA
 
 clean <- df %>% 
   pivot_longer(starts_with("12+"),
                names_to = "YearA",
                values_to = "12+" )%>% 
   mutate(YearA= str_replace_all(YearA,"12+...2", "2019")) %>% 
   mutate(YearA= str_replace_all(YearA,"12+...3", "2020")) %>% 
   pivot_longer(starts_with("12-17"),
                names_to = "YearB",
                values_to = "12-17")%>%  
   mutate(YearB = str_replace_all(YearB,"12-17...4","2019")) %>% 
   mutate(YearB = str_replace_all(YearB,"12-17...5","2020")) %>% 
   mutate(YearB = as.numeric(YearB)) %>% 
   pivot_longer(starts_with("18+"),
                names_to = "YearC",
                values_to = "18+") %>% 
   mutate(YearC = str_replace_all(YearC,"18+...6","2019")) %>% 
   mutate(YearC = str_replace_all(YearC,"18+...7","2020")) %>% 
   pivot_longer(starts_with("18-25"),
                names_to = "YearD",
                values_to = "18-25" )%>% 
   mutate(YearD= str_replace_all(YearD,"18-25...8", "2019")) %>% 
   mutate(YearD= str_replace_all(YearD,"18-25...9", "2020")) %>% 
   pivot_longer(starts_with("26+"),
                names_to = "YearE",
                values_to = "26+") %>% 
   mutate(YearE = str_replace_all(YearB,"26+...10","2019")) %>% 
   mutate(YearE = str_replace_all(YearB,"26+...11","2020")) %>% 
   mutate(YearE = as.numeric(YearE)) 
 clean$Demograph
 Finalclean <- clean %>% 
    mutate(YearA = case_when(YearA == "12+...2" ~ "2019",
                             YearA == "12+...3" ~ "2020")) %>%
    mutate(YearA = as.numeric(YearA)) %>% 
    mutate(YearC = case_when(YearC == "18+...6" ~ "2019",
                             YearC == "18+...7" ~ "2020")) %>% 
    mutate(YearC = as.numeric(YearC)) %>%
    mutate(YearD = as.numeric(YearD)) %>% 
    pivot_longer(starts_with("Year"), names_to = "YearX",values_to = "Year") %>% 
    pivot_longer(c("12+","12-17","18+","18-25","26+"),names_to = "AgeGroup",values_to = "Users") %>% 
    mutate(AgeGroup = str_replace_all(AgeGroup,"26\\+","26&up")) %>% 
    filter(str_detect(AgeGroup, "\\+",negate = TRUE)) %>% 
    select(-YearX) %>% 
    unique.data.frame()
    
 ###################################################
 #cleaned demographic percentages a.k.a Finalclean3
 ###################################################
 df3<- df3 %>% 
    filter(Demograph !="TOTAL") 
 df3[df3 =="*"] <- NA
 
 clean3 <- df3 %>% 
    pivot_longer(starts_with("12+"),
                 names_to = "YearA",
                 values_to = "12+" )%>% 
    mutate(YearA= str_replace_all(YearA,"12+...2", "2019")) %>% 
    mutate(YearA= str_replace_all(YearA,"12+...3", "2020")) %>% 
    pivot_longer(starts_with("12-17"),
                 names_to = "YearB",
                 values_to = "12-17")%>%  
    mutate(YearB = str_replace_all(YearB,"12-17...4","2019")) %>% 
    mutate(YearB = str_replace_all(YearB,"12-17...5","2020")) %>% 
    mutate(YearB = as.numeric(YearB)) %>% 
    pivot_longer(starts_with("18+"),
                 names_to = "YearC",
                 values_to = "18+") %>% 
    mutate(YearC = str_replace_all(YearC,"18+...6","2019")) %>% 
    mutate(YearC = str_replace_all(YearC,"18+...7","2020")) %>% 
    pivot_longer(starts_with("18-25"),
                 names_to = "YearD",
                 values_to = "18-25" )%>% 
    mutate(YearD= str_replace_all(YearD,"18-25...8", "2019")) %>% 
    mutate(YearD= str_replace_all(YearD,"18-25...9", "2020")) %>% 
    pivot_longer(starts_with("26+"),
                 names_to = "YearE",
                 values_to = "26+") %>% 
    mutate(YearE = str_replace_all(YearB,"26+...10","2019")) %>% 
    mutate(YearE = str_replace_all(YearB,"26+...11","2020")) %>% 
    mutate(YearE = as.numeric(YearE)) 
 clean3$Demograph
 Finalclean3 <- clean3 %>% 
    mutate(YearA = case_when(YearA == "12+...2" ~ "2019",
                             YearA == "12+...3" ~ "2020")) %>%
    mutate(YearA = as.numeric(YearA)) %>% 
    mutate(YearC = case_when(YearC == "18+...6" ~ "2019",
                             YearC == "18+...7" ~ "2020")) %>% 
    mutate(YearC = as.numeric(YearC)) %>%
    mutate(YearD = as.numeric(YearD)) %>% 
    pivot_longer(starts_with("Year"), names_to = "YearX",values_to = "Year") %>% 
    pivot_longer(c("12+","12-17","18+","18-25","26+"),names_to = "AgeGroup",values_to = "Users") %>% 
    mutate(AgeGroup = str_replace_all(AgeGroup,"26\\+","26&up")) %>% 
    filter(str_detect(AgeGroup, "\\+",negate = TRUE)) %>% 
    select(-YearX) %>% 
    unique.data.frame()
 
 
 ######################
 #Numbers in thousands
 ######################
 Finalclean$Users <- as.numeric(Finalclean$Users)
 Finalclean1$Users <- as.numeric(Finalclean1$Users)
 Finalclean$Demograph
 Finalclean1$`Geo/socieconomic`
 
 lean_sex <- Finalclean %>% 
    filter(Demograph %in% c("Male","Female"))

 clean_education <- Finalclean %>% 
    filter(Demograph %in% c("Some College/Associate’s Degree","< High School","College Graduate",
                            "High School Graduate"))
 Finalclean$Demograph %>% 
    table()
 
 
 clean_Ethnicity <- Finalclean %>% 
    filter(Demograph %in% c("White","AIAN","NHOPI","Asian","Black or African American","Two or More Races","Hispanic or Lation"))
 
 clean_Work <- Finalclean %>% 
    filter(Demograph %in% c("Full-Time","Part-Time","Unemployed","Other1"))
 
 Clean_region <- Finalclean1 %>% 
    filter(`Geo/socieconomic` %in% c("Midwest","South","West"))
 
 Clean_Metro <- Finalclean1 %>% 
    filter(`Geo/socieconomic` %in% c("Large Metro", "Small Metro","Nonmetro","Urbanized","Less Urbanized","Completely Rural"))
 
Clean_Insurance <- Finalclean1 %>% 
   filter(`Geo/socieconomic` %in% c("Private","Medicaid/CHIP","Other3","No Coverage"))
 
Clean_Poverty <- Finalclean1 %>% 
   filter(`Geo/socieconomic` %in% c("Less Than 100%","100-199%","200% or More"))
 
 ######################
 #number in Percentages
 ######################
 Finalclean3$Users <- as.numeric(Finalclean3$Users)
Finalclean4$Users <- as.numeric(Finalclean4$Users)
 Finalclean3$Demograph
 Finalclean4$`Geo/socieconomic`
 
 lean_sex2 <- Finalclean3 %>% 
    filter(Demograph %in% c("Male","Female"))
 
 clean_education2 <- Finalclean3 %>% 
    filter(Demograph %in% c("Some College/Associate’s Degree","< High School","College Graduate",
                            "High School Graduate"))
 clean_Ethnicity2 <- Finalclean3 %>% 
    filter(Demograph %in% c("White","AIAN","NHOPI","Asian","Black or African American","Two or More Races","Hispanic or Lation"))
 
 clean_Work2 <- Finalclean3 %>% 
    filter(Demograph %in% c("Full-Time","Part-Time","Unemployed","Other1"))
 
 Clean_region2 <- Finalclean4 %>% 
    filter(`Geo/socieconomic` %in% c("Midwest","South","West"))
 
 Clean_Metro2 <- Finalclean4 %>% 
    filter(`Geo/socieconomic` %in% c("Large Metro", "Small Metro","Nonmetro","Urbanized","Less Urbanized","Completely Rural"))
 
 Clean_Insurance2 <- Finalclean4 %>% 
    filter(`Geo/socieconomic` %in% c("Private","Medicaid/CHIP","Other3","No Coverage"))
 
 Clean_Poverty2 <- Finalclean4 %>% 
    filter(`Geo/socieconomic` %in% c("Less Than 100%","100-199%","200% or More"))
 
 #removed 12-17 and 18-25 because by 25 and up youve proably completed the education
 #youll have for the rest of your life
 #Some college or graduated took way more drugs than highschool and less in all illegal 
 #catogories except meth
 clean_education %>% 
    filter(AgeGroup != "12-17") %>% 
    filter(AgeGroup !="18-25") %>% 
    filter(Time == "Lifetime") %>% 
    ggplot(aes(x=AgeGroup,y=Users,color=Demograph)) +
    geom_boxplot() +
    facet_wrap(~Drug)
 
 clean_education2 %>% 
    filter(AgeGroup != "12-17") %>% 
    filter(AgeGroup !="18-25") %>% 
    filter(Time == "Lifetime") %>% 
    ggplot(aes(x=AgeGroup,y=Users,color=Demograph)) +
    geom_boxplot() +
    facet_wrap(~Drug)
 
 
 clean_education %>% 
    filter(AgeGroup != "12-17") %>% 
    filter(Time == "Lifetime") %>%
    filter(Drug == "Marijuana") %>% 
    glm(data = ., formula = Users ~ AgeGroup + Demograph) %>% 
    report()

  #Prescription medications are typically more by women or close
 #between sex but males take significantly more illegal drugs than females
 #trends hold true for lifetime, Past year and month expect past month stimulants
 #has more men but its tight for all others so not to weird
 lean_sex %>% 
    ggplot(aes(x=AgeGroup,y=Users,color=Demograph))+
    geom_boxplot()+
    facet_wrap(~Drug,scales="free_y")
 
 
 lean_sex2 %>% 
    ggplot(aes(x=AgeGroup,y=Users,color=Demograph))+
    geom_boxplot()+
    facet_wrap(~Drug,scales="free_y")
 
      #only 26&up is significant
      lean_sex %>% 
         filter(AgeGroup != "12-17") %>% 
         filter(Time == "Lifetime") %>%
         glm(data = ., formula = Users ~ AgeGroup + Demograph) %>% 
         report()
      lean_sex %>% 
         glm(data = ., formula = Users ~ AgeGroup + Demograph) %>% 
         report()
 
 #White people use more of all illegal drugs by a much larger margin
 #see if can compare to population percentages to see if its dissporportionate
 #to their portion of the population
 clean_Ethnicity %>% 
    ggplot(aes(x=AgeGroup,y=Users,color=Demograph))+
    geom_boxplot()+
    facet_wrap(~Drug,scales="free_y")
 clean_Ethnicity$Demograph %>% 
    table()
 #all over the place
 clean_Ethnicity2 %>% 
    ggplot(aes(x=AgeGroup,y=Users,color=Demograph))+
    geom_boxplot()+
    facet_wrap(~Drug,scales="free_y")
 
 
       #Weak model r^2=0.11, 26+ and White are statisticly significant
       clean_Ethnicity %>% 
         glm(data = ., formula = Users ~ AgeGroup + Demograph) %>% 
         report()
 
       clean_Ethnicity2 %>% 
          glm(data = ., formula = Users ~ AgeGroup + Demograph) %>% 
          report()
 #removed 12-17 cause it doesnt apply, Remember free scales
 #Full-time definitly takes more drugs
clean_Work %>% 
   filter(AgeGroup != "12-17") %>%
   ggplot(aes(x=AgeGroup,y=Users, color=Demograph))+
   geom_boxplot()+
   facet_wrap(~Drug,scales="free_y")

#Unemployed is typically highest with ocasionally full time tying or leading
clean_Work2 %>% 
   filter(AgeGroup != "12-17") %>%
   ggplot(aes(x=AgeGroup,y=Users, color=Demograph))+
   geom_boxplot()+
   facet_wrap(~Drug,scales="free_y")
 

#week model r^2=0.09 but 26&up and part time is significant
clean_Work %>% 
   glm(data = ., formula = Users ~ AgeGroup + Demograph) %>% 
   report()
 
 #Typically the south with the exception of Meth which the west takes the cake
Clean_region%>% 
   ggplot(aes(x=AgeGroup,y=Users, color=`Geo/socieconomic`))+
   geom_boxplot()+
   facet_wrap(~Drug,scales="free_y")

#much closer, look at each drug and different regions are the highest 
#No overall patterns
Clean_region2%>% 
   ggplot(aes(x=AgeGroup,y=Users, color=`Geo/socieconomic`))+
   geom_boxplot()+
   facet_wrap(~Drug,scales="free_y")
 
 
 Clean_Metro %>% 
    ggplot(aes(x=AgeGroup,y=Users, color=`Geo/socieconomic`))+
    geom_boxplot()+
    facet_wrap(~Drug,scales="free_y")
 
    
 
 
 
 
 
 
 
