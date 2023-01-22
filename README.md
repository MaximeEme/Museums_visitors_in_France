# MUSEUM'S VISITS IN FRANCE 

This is a personal project produced during my free time to demonstrate my personal skills in spreadsheet, R studio and Tableau. 

## Step 1: Ask 

### 1.Ministry of Cultural Affairs   

The Ministry of Cultural Affairs is the main governing body over the 1161 museums in France. 

### 2.Business task 

Analyzing historical data in museum's visits from the Ministry of Cultural Affairs to identify trends and connections between paid and free visits. 

### 3.Key Stakeholders

**Secretary of state**: She is the manager and the director of marketing. She is handling the development of campaigns and the ways to promote the bike sharing program.

**Ministry executive team**: The notoriously detail-oriented executive team will decide whether to approve the recommended analysis. 

## Step 2: Prepare

### 1.Information on data

The data has been shared publicly by the French's Ministry of Cultural Affairs under strict licence. It is composed of yearly spreadsheets files from 2008 to 2018. The data has been collected from the visits of Museums and it includes the OSM id, name of museum, street number, street, city, postal code, longitude, latitude, stats, label ...

### 2.Limitations of data.

The data obtained is from 2008 to 2018. I choose to work on ten years worth of data due to the huge amount of data, the analysis can only be judged as a multi-year analysis. As it is publicly shared on the ministry's website, I have no doubt on its integrity.

### 3.Is the data ROCC ?

A good data source is ROCCC which stands for Reliable, Original, Comprehensive, Current, and Cited.

Reliable - High - It has all the museums that are endorsed by the ministry.

Original - High - It is first-party data.

Comprehensive - Low - It has been difficult to understand some of the columns.

Current - Med - The data is 4 year old.

Cited - Med - It is first-party data. 

# Step 3: Process

I began the data cleaning on Gsheet to have a look at the data itself and see how I could prepare my work for analysis on R studio.

### 1. Gsheet

I took a look around the first file year 2008 :
- I erased several columns such as street number, street, label, website, telephone, fax and tag.
- I separated in two columns the stat column which had the data on the free tickets and paid tickets.
- I created a new department column based on the first 2 digits of the postal code.
- I looked for any inconsistencies in the city, department, free tickets and paid tickets columns.

I repeated these steps for all ten files and then proceeded to continue on R studio.
    
### 2.Installing packages and library in R studio. 

```{r}

install.packages("tidyverse")

install.packages("ggplot2")

install.packages("lubridate")

install.packages("janitor")

install.packages("ggpubr")

install.packages("skimr")

install.packages("here")

install.packages("ggrepel")

```

```{r}

library(tidyverse)

library(janitor)

library(here)

library(skimr)

library(ggplot2)

library(lubridate)

```

I chose these packages to help me with my analysis.

### 2.Importing and preparing the data

```{r}

Frequentation_musees_2008 <- read_csv("Frequentation_musees spreadsheet files /Frequentation_musees_2009.csv"),
Frequentation_musees_2009 <- read_csv("Frequentation_musees spreadsheet files /Frequentation_musees_2009.csv"),
Frequentation_musees_2010 <- read_csv("Frequentation_musees spreadsheet files /Frequentation_musees_2010.csv"),
Frequentation_musees_2011 <- read_csv("Frequentation_musees spreadsheet files /Frequentation_musees_2011.csv"),
Frequentation_musees_2012 <- read_csv("Frequentation_musees spreadsheet files /Frequentation_musees_2012.csv"),
Frequentation_musees_2013 <- read_csv("Frequentation_musees spreadsheet files /Frequentation_musees_2013.csv"),
Frequentation_musees_2014 <- read_csv("Frequentation_musees spreadsheet files /Frequentation_musees_2014.csv"),
Frequentation_musees_2015 <- read_csv("Frequentation_musees spreadsheet files /Frequentation_musees_2015.csv"),
Frequentation_musees_2016 <- read_csv("Frequentation_musees spreadsheet files /Frequentation_musees_2016.csv"),
Frequentation_musees_2017 <- read_csv("Frequentation_musees spreadsheet files /Frequentation_musees_2017.csv"),
Frequentation_musees_2018 <- read_csv("Frequentation_musees spreadsheet files /Frequentation_musees_2018.csv"),
```

### 3.Data cleaning part II

I checked for duplicated data in every tables. 

```{r}

sum(duplicated(Frequentation_musees_2008))

```

> [1] 0

I removed rows with NA values in every tables. 

```{r}

frequentation_musees_2008 <- frequentation_musees_2008 %>% drop_na()

```

I cleaned and formatted the columns names of every tables. 

```{r}

clean_names(frequentation_musees_2008)

frequentation_musees_2008 <- rename_with(frequentation_musees_2008, tolower)

```

I merged and renamed the file. 

```{r}
Frequentation_musees <- bind_rows(Frequentation_musees_2008, Frequentation_musees_2009, Frequentation_musees_2010, Frequentation_musees_2011, Frequentation_musees_2012, Frequentation_musees_2013, Frequentation_musees_2014, Frequentation_musees_2015, Fréquentation_musees_2016, Frequentation_musees_2017, Frequentation_musees_2018,)
```

I created a region column. 

```{r}
Frequentation_musees <- Frequentation_musees %>% mutate(region = case_when(no_departement %in% c("1", "3", "7", "15", "26", "38", "42", "43", "63", "69", "73", "74") ~ "Auvergne-Rhône-Alpes",no_departement %in% c("89", "21", "70", "90", "25", "39", "71", "58") ~ "Bourgogne-Franche-Comté",no_departement %in% c("8", "55", "54", "57", "67", "68", "88", "52", "10", "51") ~ "Grand Est",no_departement %in% c("62", "59", "2", "80", "60") ~ "Hauts-de-France",no_departement %in% c("75", "92", "93", "94", "95", "77", "91", "78") ~ "Ile-De-France",no_departement %in% c("50", "14", "61", "27", "76") ~ "Normandie",no_departement %in% c("53", "72", "44", "49", "85") ~ "Pays De La Loire",no_departement %in% c("29", "22", "35", "56") ~ "Bretagne",no_departement %in% c("62", "59", "2", "80", "28", "45", "41", "18", "37", "36") ~ "Centre-Val De Loire",no_departement %in% c("79", "86", "87", "23", "19", "24", "16", "17", "33", "47", "40", "64") ~ "Nouvelle Aquitaine",no_departement %in% c("46", "12", "48", "30", "34", "81", "82", "32", "31", "65", "9", "11", "66") ~ "Occitanie",no_departement %in% c("13", "84", "4", "5", "6", "83") ~ "Provence-Alpes-Côte d'Azur"))
```

# Step 4: Analyse

### 1. Determine the number of museums open vs total.

I wanted to know how many museums were open and the total (including the closed ones). 

```{r}
open_closed_year <- Frequentation_musees %>% group_by(year) %>% summarise(open = sum(status1))
musee_total <- Frequentation_musees %>% group_by(year) %>% summarise(total = sum(status2))
musee_open_closed_total <- merge(open_closed_year, musee_total, by=c("year"))
```

### 2. Number of tickets per year. 

Then, I wanted to see the number of tickets per year. 

```{r}
tickets_per_year <- Frequentation_musees %>% group_by(year) %>% summarise(free_tickets = sum(nb_free_tickets), paid_tickets = sum(nb_paid_tickets))
```

### 3. Number of tickets per year and department. 

I wondered about the number of tickets in each department assuming that 75 (Paris) was the first one in visitors. 

```{r}
tickets_per_departement_year <- Frequentation_musees %>% group_by(no_departement, year) %>% summarise(free_tickets = sum(nb_free_tickets), paid_tickets = sum(nb_paid_tickets))

```

### 4.Determine the N°1 museums per region and year. 

I wanted to know which museums were the most visited in the 13 regions by free tickets and paid tickets. 

```{r}
musees_free_tickets_per_region_year <- Frequentation_musees %>% select(name,year, nb_free_ticket, region) %>%  arrange(region, year, desc(nb_free_tickets)) %>% group_by(region, year) %>% slice(1:2) %>% ungroup()
musees_paid_tcikets_per_region_year <- Frequentation_musees %>% select(name,year, nb_paid_tickets, region) %>%  arrange(region, year, desc(nb_paid_tickets)) %>% group_by(region, year) %>% slice(1:2) %>% ungroup()
```

### 5.Getting the Top 10 departments with the most visited museums. 

I wanted to be more precise by having the top 10 departments with the most visited museums.  

```{r}
Total_visits_department <- Frequentation_musees %>% group_by(no_departement) %>% summarise(total_free_tickets = sum(nb_free_tickets), total_paid_tickets = sum(nb_paid_tickets))

```

### 6. Ranking the regions according to their total free tickets and paid tickets. 

I thought that ranking the regions according to their total free tickets and paid tickets would bring interesting results even if I knew that Ile-De-France would be first.

```{r}
Total_visits_region <- Frequentation_musees %>% group_by(region) %>% summarise(total_free_tickets = sum(nb_free_tickets), total_paid_tickets = sum(nb_paid_tickets))

```
# Step 5: Share phase

I created these two interactive dashboards on Tableau to expose my findings. 

[Museums & visitors in France](https://public.tableau.com/views/MuseumsvisitorsinFrance2008-2018/MuseumsvisitorsinFranceovera10yearperiod_?:language=fr-FR&:display_count=n&:origin=viz_share_link) 
[Maps of Museums & visitors in France](https://public.tableau.com/views/MapsofmuseumsvisitorsinFrance2008-2018/MapsofmuseumsvisitsinFranceovera10yearperiod_?:language=fr-FR&:display_count=n&:origin=viz_share_link)

# Step 6: Act phase 

1/ From 2008 to 2018, there's been a 4.7% decrease in museum's accreditation by the Ministry from 1219 museums in 2008 to 1161 in 2018. That represents 58 museums that disappeared from the Ministry's registry. The number of open museums has been stable during that 10 year period even though 21 museums closed doors, representing a 1.9% decrease.

2/ Museums have witnessed an important rise in visits either paid or free in those 10 years. Starting with the paid tickets, it represents a 50% increase in tickets sold between 2008 and 2018. 37% is the increase of free tickets from 2008 and 2018. It established France as a powerhouse of culture.

3/ Paris (75) is indeed the department with the most visitors while Haute-Loire (43) is the department with the least visitors.

4/ Musée du Louvre is the most visited museum in Paris, Ile-de-France and France and in both paid and free tickets. It is interesting to see that 6 regions have different museums in the two categories (Normandie, Pays de la Loire, PACA, Auvergne-Rhône-Alpes, Grand Est, Centre-Val-de-Loire).

5/ Those 10 tens are not surprising. They represent each one a major city in France except Haut-Rhin (68) and Côte-d'or (21).

6/ Ile-de-France is dominating each and every region in terms of tickets in museums. The second is PACA with only 21 715 898 millions which is x6 less visitors than Ile-de-France and the third region with most visitors is Auvergne-Rhône-Alpes with 18 129 211 millions visitors. 
