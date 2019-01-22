library(dplyr)
library(datapkg)
library(acs)
library(data.table)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Disengaged Youth
# Created by Jenna Daly
# On 03/28/2018
#
##################################################################

# ACS B14005
# Get geography object for CT and subcounty divisions
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2017,
    table = "B14005"
)

options(scipen=9999)
dataset <- data.table()
for (data in acsdata) {
    year <- data@endyear
    year <- paste(as.numeric(year)-4, as.numeric(year), sep="-")
 # Totals for denominator    
    pop.total <- acsSum(data, 1, "Total")
    pop.total.m <- acsSum(data, 2, "Total Male")
    pop.total.f <- acsSum(data, 16, "Total Female")
 # aggregate values
    #first sum appropriately
    pop.male.engaged <- acsSum(data, c(3, 9, 13), "Engaged Male")
    pop.female.engaged <- acsSum(data, c(17, 23, 27), "Engaged Female")
    pop.total.engaged <- acsSum(data, c(3, 9, 13, 17, 23, 27), "Engaged Total")
    pop.male.disengaged <- acsSum(data, c(10, 11, 14, 15), "Disengaged Male")
    pop.female.disengaged <- acsSum(data, c(24, 25, 28, 29), "Disengaged Female")
    pop.total.disengaged <- acsSum(data, c(10, 11, 14, 15, 24, 25, 28, 29), "Disengaged Total")  
    #then divide appropriately 
    percent.male.engaged <- divide.acs(pop.male.engaged, pop.total.m, method = "ratio")
    acs.colnames(percent.male.engaged) <- "Percent Engaged Male"
    percent.female.engaged <- divide.acs(pop.female.engaged, pop.total.f, method = "ratio")
    acs.colnames(percent.female.engaged) <- "Percent Engaged Female"
    percent.total.engaged <- divide.acs(pop.total.engaged, pop.total, method = "ratio")
    acs.colnames(percent.total.engaged) <- "Percent Engaged Total" 
    percent.male.disengaged <- divide.acs(pop.male.disengaged, pop.total.m, method = "ratio")
    acs.colnames(percent.male.disengaged) <- "Percent Disengaged Male"
    percent.female.disengaged <- divide.acs(pop.female.disengaged, pop.total.f, method = "ratio")
    acs.colnames(percent.female.disengaged) <- "Percent Disengaged Female"
    percent.total.disengaged <- divide.acs(pop.total.disengaged, pop.total, method = "ratio")
    acs.colnames(percent.total.disengaged) <- "Percent Disengaged Total"
    
 # merge in fips
    datafips <- data.table(geography(data)[2])
    
 # Cast to separate data frames
    numberEstimates <- data.table(
        datafips$Id2,
        estimate(pop.total),
        estimate(pop.total.m),
        estimate(pop.total.f),
        estimate(pop.male.engaged),
        estimate(pop.female.engaged),
        estimate(pop.total.engaged),
        estimate(pop.male.disengaged),
        estimate(pop.female.disengaged),
        estimate(pop.total.disengaged),
        year,
        "Number",
        "Disengaged Youth"
    )
    numberMOES <- data.table(
        datafips$Id2,
        standard.error(pop.total) * 1.645,
        standard.error(pop.total.m) * 1.645,
        standard.error(pop.total.f) * 1.645,
        standard.error(pop.male.engaged) * 1.645,
        standard.error(pop.female.engaged) * 1.645,
        standard.error(pop.total.engaged) * 1.645,
        standard.error(pop.male.disengaged) * 1.645,
        standard.error(pop.female.disengaged) * 1.645,
        standard.error(pop.total.disengaged) * 1.645,
        year,
        "Number",
        "Margins of Error"
    )
    numberNames <- c(
            "FIPS",
            "Number:Total:Total",
            "Number:Total:Male",
            "Number:Total:Female",
            "Number:Engaged:Male",
            "Number:Engaged:Female",
            "Number:Engaged:Total",   
            "Number:Disengaged:Male",
            "Number:Disengaged:Female",
            "Number:Disengaged:Total",  
            "Year",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
        rbind(numberEstimates, numberMOES),
        id.vars = c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name = "Disengaged Youth",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
     )

    percentEstimates <- data.table(
        datafips$Id2,
        estimate(percent.male.engaged),
        estimate(percent.female.engaged),
        estimate(percent.total.engaged),
        estimate(percent.male.disengaged),
        estimate(percent.female.disengaged),
        estimate(percent.total.disengaged),
        year,
        "percent",
        "Disengaged Youth"
    )
    percentMOES <- data.table(
        datafips$Id2,
        standard.error(percent.male.engaged) * 1.645,
        standard.error(percent.female.engaged) * 1.645,
        standard.error(percent.total.engaged) * 1.645, 
        standard.error(percent.male.disengaged) * 1.645,
        standard.error(percent.female.disengaged) * 1.645,
        standard.error(percent.total.disengaged) * 1.645,
        year,
        "percent",
        "Margins of Error"
    )
    percentNames <- c(
        "FIPS",
        "Percent:Engaged:Male",
        "Percent:Engaged:Female",
        "Percent:Engaged:Total",
        "Percent:Disengaged:Male",
        "Percent:Disengaged:Female",
        "Percent:Disengaged:Total",
        "Year",
        "Measure Type",
        "Variable"
     )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)

    percentsData.melt <- melt(
        rbind(percentEstimates, percentMOES),
        id.vars = c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name = "Disengaged Youth",
        variable.factor = F,
        value.name = "Value",
        value.factor = F
     )

    dataset <- rbind(dataset, numbersData.melt, percentsData.melt)
}

# Parse gender, variable out of "var" column
dataset[,c("Measure Type", "Type", "Gender"):=do.call(Map, c(f = c, strsplit(`Disengaged Youth`, ":", fixed = T)))]

dataset$`Disengaged Youth` <- NULL

dataset$FIPS <- gsub("^", "0", dataset$FIPS)

#Merge in towns by FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

final_dataset <- merge(towns, dataset, by = "FIPS", all.x=T)

names(final_dataset)[names(final_dataset) == "Type"] <- "Engagement"

# Round Values according to MT/Variable
# Number, Variable: round to the nearest whole number
# Number MOE: round to the nearest whole number
# Percent, Variable: multiply by 100, round to nearest hundredth
# Percent, MOE: round to nearest hundredth

final_dataset <- final_dataset %>% 
  mutate(Value = ifelse(`Measure Type` == "Percent" & Variable == "Disengaged Youth", Value*100, Value))

final_dataset$Value <- round(final_dataset$Value, 2)

final_dataset$Value[final_dataset$Value == "NaN"] <- NA

final_dataset$Gender <- factor(final_dataset$Gender, levels = c("Total", "Female", "Male"))
final_dataset$Engagement <- factor(final_dataset$Engagement, levels = c("Total", "Engaged", "Disengaged"))

# Select and sort columns
final_dataset <- final_dataset %>% 
  select(Town, FIPS, Year, Gender, Engagement, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, Gender, Engagement, `Measure Type`, Variable)

# write to table
write.table(
    final_dataset,
    file.path(getwd(), "data", "disengaged-youth-2018.csv"),
    sep = ",",
    row.names = F, 
    na="-9999"
)
