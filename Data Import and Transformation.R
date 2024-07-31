rm(list=ls())
library(dplyr)

#Bring in the data files to be cleaned
Inflation<-read.csv("C:/Users/Brian/OneDrive/Documents/Inflation.csv")
MidWest<-read.csv("C:/Users/Brian/OneDrive/Documents/Midwest.csv")
South<-read.csv("C:/Users/Brian/OneDrive/Documents/South.csv")
NorthEast<-read.csv("C:/Users/Brian/OneDrive/Documents/NorthEast.csv")
West<-read.csv("C:/Users/Brian/OneDrive/Documents/West.csv")
City<-read.csv("C:/Users/Brian/OneDrive/Documents/City.csv")


#Adds dummy variables in order to distinguish which region the data belongs to
MidWest$MW<-1
South$MW<-0
NorthEast$MW<-0
West$MW<-0

MidWest$S<-0
South$S<-1
NorthEast$S<-0
West$S<-0

MidWest$NE<-0
South$NE<-0
NorthEast$NE<-1
West$NE<-0

MidWest$W<-0
South$W<-0
NorthEast$W<-0
West$W<-1



#Combines the datasets together and creates a CSV file
combined_data1 <- rbind(MidWest, South)
Combined_data2 <- rbind(combined_data1, NorthEast)
Combined_Final <- rbind(Combined_data2, West)


write.csv(Combined_Final,"C:/Users/brian/OneDrive/Documents/Combined_Egg_Data.csv")


#The following code calculates the percentage change for the price of eggs between the current and prior year
percentage_changes <- Combined_Final %>%
  mutate(
    Jan_Percentage_Change = (Jan / lag(Jan) - 1) * 100,
    Feb_Percentage_Change = (Feb / lag(Feb) - 1) * 100,
    Mar_Percentage_Change = (Mar / lag(Mar) - 1) * 100,
    Apr_Percentage_Change = (Apr / lag(Apr) - 1) * 100,
    May_Percentage_Change = (May / lag(May) - 1) * 100,
    Jun_Percentage_Change = (Jun / lag(Jun) - 1) * 100,
    Jul_Percentage_Change = (Jul / lag(Jul) - 1) * 100,
    Aug_Percentage_Change = (Aug / lag(Aug) - 1) * 100,
    Sep_Percentage_Change = (Sep / lag(Sep) - 1) * 100,
    Oct_Percentage_Change = (Oct / lag(Oct) - 1) * 100,
    Nov_Percentage_Change = (Nov / lag(Nov) - 1) * 100,
    Dec_Percentage_Change = (Dec / lag(Dec) - 1) * 100
  )

#The following code calculates the percentage change for the price of eggs between the current and prior month
percentage_changes2 <- Combined_Final %>%
  mutate(
    Jan_Percentage_Change = (Jan / lag(Dec) - 1) * 100,
    Feb_Percentage_Change = (Feb / Jan - 1) * 100,
    Mar_Percentage_Change = (Mar / Feb - 1) * 100,
    Apr_Percentage_Change = (Apr / Mar - 1) * 100,
    May_Percentage_Change = (May / Apr - 1) * 100,
    Jun_Percentage_Change = (Jun / May - 1) * 100,
    Jul_Percentage_Change = (Jul / Jun - 1) * 100,
    Aug_Percentage_Change = (Aug / Jul - 1) * 100,
    Sep_Percentage_Change = (Sep / Aug - 1) * 100,
    Oct_Percentage_Change = (Oct / Sep - 1) * 100,
    Nov_Percentage_Change = (Nov / Oct - 1) * 100,
    Dec_Percentage_Change = (Dec / Nov - 1) * 100
  )

#remove extra rows
Col_to_Remove<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#Code calculates the difference between the percent change in price for eggs and the monthly inflation data
Only_Diffs<-percentage_changes2 %>%
  select(-one_of(Col_to_Remove))

Inflation_comp<-merge(Only_Diffs, Inflation, by = "Year")

Inflation_Final <- Inflation_comp %>%
  mutate(
    Jan_Egg_Inflation_Dif = Jan_Percentage_Change - Jan,
    Feb_Egg_Inflation_Dif = Feb_Percentage_Change - Feb,
    Mar_Egg_Inflation_Dif = Mar_Percentage_Change - Mar,
    Apr_Egg_Inflation_Dif = Apr_Percentage_Change - Apr,
    May_Egg_Inflation_Dif = May_Percentage_Change - May,
    Jun_Egg_Inflation_Dif = Jun_Percentage_Change - Jun,
    Jul_Egg_Inflation_Dif = Jul_Percentage_Change - Jul,
    Aug_Egg_Inflation_Dif = Aug_Percentage_Change - Aug,
    Sep_Egg_Inflation_Dif = Sep_Percentage_Change - Sep,
    Oct_Egg_Inflation_Dif = Oct_Percentage_Change - Oct,
    Nov_Egg_Inflation_Dif = Nov_Percentage_Change - Nov,
    Dec_Egg_Inflation_Dif = Dec_Percentage_Change - Dec 
  )

write.csv(Inflation_Final,"C:/Users/brian/OneDrive/Documents/Inflation_Differences.csv")

City_changes1 <- City %>%
  mutate(
    Jan_Percentage_Change = (Jan / lag(Dec) - 1) * 100,
    Feb_Percentage_Change = (Feb / Jan - 1) * 100,
    Mar_Percentage_Change = (Mar / Feb - 1) * 100,
    Apr_Percentage_Change = (Apr / Mar - 1) * 100,
    May_Percentage_Change = (May / Apr - 1) * 100,
    Jun_Percentage_Change = (Jun / May - 1) * 100,
    Jul_Percentage_Change = (Jul / Jun - 1) * 100,
    Aug_Percentage_Change = (Aug / Jul - 1) * 100,
    Sep_Percentage_Change = (Sep / Aug - 1) * 100,
    Oct_Percentage_Change = (Oct / Sep - 1) * 100,
    Nov_Percentage_Change = (Nov / Oct - 1) * 100,
    Dec_Percentage_Change = (Dec / Nov - 1) * 100
  )

Only_Diffs2<-City_changes1 %>%
  select(-one_of(Col_to_Remove))

Inflation_comp2<-merge(Only_Diffs2, Inflation, by = "Year")

City_Inflation_Final <- Inflation_comp2 %>%
  mutate(
    Jan_Egg_Inflation_Dif = Jan_Percentage_Change - Jan,
    Feb_Egg_Inflation_Dif = Feb_Percentage_Change - Feb,
    Mar_Egg_Inflation_Dif = Mar_Percentage_Change - Mar,
    Apr_Egg_Inflation_Dif = Apr_Percentage_Change - Apr,
    May_Egg_Inflation_Dif = May_Percentage_Change - May,
    Jun_Egg_Inflation_Dif = Jun_Percentage_Change - Jun,
    Jul_Egg_Inflation_Dif = Jul_Percentage_Change - Jul,
    Aug_Egg_Inflation_Dif = Aug_Percentage_Change - Aug,
    Sep_Egg_Inflation_Dif = Sep_Percentage_Change - Sep,
    Oct_Egg_Inflation_Dif = Oct_Percentage_Change - Oct,
    Nov_Egg_Inflation_Dif = Nov_Percentage_Change - Nov,
    Dec_Egg_Inflation_Dif = Dec_Percentage_Change - Dec 
  )

write.csv(City_Inflation_Final,"C:/Users/brian/OneDrive/Documents/City_Inflation_Differences.csv")
