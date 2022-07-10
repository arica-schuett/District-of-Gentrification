---
title: "DCGentrificationDataCleaning"
author: "Arica Schuett"
date: "10/22/2021"
output: pdf_document
---

## Gentrification in DC Survey
## WA-PO Survey Data
#install.packages("ggplot2")
#install.packages("ggplot2", repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
#install.packages("purrr")
library(cowplot)
library(tidyverse)

###### 2000 ##############
#install.packages("haven")
library(haven)
DF00 = read_csv("/Users/aricaschuett/Documents/MiLO/Gentrification/31119631.csv")

# 21. As you may know, the district government is trying to redevelop parts of
# the city to attract new businesses and residents. Do you think this process of
# redevelopment is mainly good or mainly bad for (Whites in the District)?
DF00 <- mutate( DF00, 
                DevGood4Whts = ifelse(DF00$q21_1 == "Mainly good", 1, 0),
                DevBad4Whts = ifelse(DF00$q21_1  == "Mainly bad", 1, 0),
                DK_NoOpn4Whts = ifelse(DF00$q21_1  == "Don't know", 1, 0))

# 21. ... (Blacks in the District)?
DF00 <- mutate( DF00, 
                DevGood4Blks = ifelse(DF00$q21_2 == "Mainly good", 1, 0),
                DevBad4Blks = ifelse(DF00$q21_2  == "Mainly bad", 1, 0),
                DK_NoOpn4Blks = ifelse(DF00$q21_2  == "Don't know", 1, 0))


# 21.... is mainly good or mainly bad for (The Rich)?
DF00 <- mutate( DF00, 
                DevGood4Rich = ifelse(DF00$q21_3 == "Mainly good", 1, 0),
                DevBad4Rich = ifelse(DF00$q21_3  == "Mainly bad", 1, 0),
                DK_NoOpn4Rich = ifelse(DF00$q21_3  == "Don't know", 1, 0))


# 21. ... is mainly good or mainly bad for (The Poor)?
DF00 <- mutate( DF00, 
                DevGood4Poor = ifelse(DF00$q21_4 == "Mainly good", 1, 0),
                DevBad4Poor = ifelse(DF00$q21_4  == "Mainly bad", 1, 0),
                DK_NoOpn4Poor = ifelse(DF00$q21_4  == "Don't know", 1, 0))

# 21. ... is mainly good or mainly bad for (Your Neighborhood)?
DF00 <- mutate( DF00, 
                DevGood4Nhbd = ifelse(DF00$q21_6 == "Mainly good", 1, 0),
                DevBad4Nhbd = ifelse(DF00$q21_6  == "Mainly bad", 1, 0),
                DK_NoOpn4Nhbd = ifelse(DF00$q21_6  == "Don't know", 1, 0))

# 21. ... is mainly good or mainly bad for (People like You)?
DF00 <- mutate( DF00, 
                DevGood4PplYou = ifelse(DF00$q21_7 == "Mainly good", 1, 0),
                DevBad4PplYou = ifelse(DF00$q21_7  == "Mainly bad", 1, 0),
                DK_NoOpn4PplYou = ifelse(DF00$q21_7  == "Don't know", 1, 0))

# 21. ... is mainly good or mainly bad for (the city overall)?
DF00 <- mutate( DF00, 
                DevGood4City = ifelse(DF00$q21_8 == "Mainly good", 1, 0),
                DevBad4City = ifelse(DF00$q21_8  == "Mainly bad", 1, 0),
                DK_NoOpn4City = ifelse(DF00$q21_8  == "Don't know", 1, 0))

# Gender?
DF00 <- mutate( DF00, 
                Male = ifelse(DF00$sex == "Male", 1, 0),
                Female = ifelse(DF00$sex == "Female", 1, 0))

# race?
DF00 <- mutate( DF00, 
                White = ifelse(DF00$race== "Male", 1, 0),
                Black = ifelse(DF00$race  == "Female", 1, 0),
                WhiteHisp = ifelse(DF00$race== "White Hispanic", 1, 0),
                BlackHisp = ifelse(DF00$race== "Black Hispanic", 1, 0),
                HispNRG = ifelse(DF00$race== "Hispanic(no race given)", 1, 0),
                OtherRace = ifelse(DF00$race== "Other Race", 1, 0))

# own or rent? Not Included 2000

# years in DC?

# DC Direction

# years in DC? Not included 2000

# Ward? Not included 2000

# Family Financial Situation
# 25. Now thinking only about your own situation, have any of the cuts in
# spending or programs over the past few years hurt you or your family directly?

DF00 <- mutate( DF00, 
                SpendingHurtFamYes = ifelse(DF00$q20== "Yes", 1, 0),
                SpendingHurtFamNo = ifelse(DF00$q20  == "No", 1, 0),
                NoOpinSpendingFam = ifelse(DF00$q20== "Don't know", 1, 0))
                

# Income1
DF00 <- mutate( DF00, 
                ThirtyKorMore = ifelse(DF00$income1== "$30,000 or more", 1, 0),
                ThirtyKorLess = ifelse(DF00$income1  == "Less than $30,000, or ", 1, 0))

# Income2
DF00 <- mutate( DF00, 
                Btwn12kand20k = ifelse(DF00$income1== "$12,000 but less than $20,000", 1, 0),
                Btwn20kand30k = ifelse(DF00$income1  == "$20,000 $20,000 but less than $30,000", 1, 0),
                Btwn8kand12k = ifelse(DF00$income1  == "$8,000 but less than $12,000", 1, 0),
                Under8k = ifelse(DF00$income1  == "Under $8,000", 1, 0))

# Income3
DF00 <- mutate( DF00, 
                Btw30kand50k = ifelse(DF00$income1== "$30,000 but less than $50,000", 1, 0),
                Bwn50kand75k = ifelse(DF00$income1  == "$50,000 but less than $75,000, or", 1, 0),
                MoreThan75k = ifelse(DF00$income1  == "$75,000 or more ", 1, 0))

###### 2002 ##############
#install.packages("haven")
library(haven)
DF02 = read_spss("/Users/aricaschuett/Documents/MiLO/Gentrification/03554-0001-Data.por")

# Biggest Problem Q17
DF02 <- mutate(DF02, 
               BP02Crime = ifelse(DF02$Q17  == 1, 1, 0), 
               BP02School = ifelse(DF02$Q17  == 2, 1, 0), 
               BP02Services = ifelse(DF02$Q17  == 3, 1, 0), 
               BP02Jobs = ifelse(DF02$Q17  == 4, 1, 0),
               BP02Terrorism = ifelse(DF02$Q17 == 5, 1, 0), 
               BP02Housing = ifelse(DF02$Q17  == 6, 1, 0), #"housing costs, low income housing"
               BP02Other = ifelse(DF02$Q17  == 7, 1, 0), 
               BP02DK = ifelse(DF02$Q17  == 8, 1, 0), 
               BP02NA= ifelse(DF02$Q17  == 9, 1, 0))

# In General, would you say this kind of redevelopment is a good thing or mainly a bad thing? Q32
DF02 <- mutate(DF02, 
               DevMainlyGood = ifelse(DF02$Q32 == 1, 1, 0), 
               DevMainlyBad = ifelse(DF02$Q32 == 2, 1, 0), 
               DevNeitherGB = ifelse(DF02$Q32 == 3, 1, 0),
               DevBothGB = ifelse(DF02$Q32 == 4, 1, 0),
               DevNoOpin = ifelse(DF02$Q32 == 8, 1, 0))

# Is this redevelopment happening in your neighborhood or not?
DF02 <- mutate(DF02, 
               DevInMyNbhd = ifelse(DF02$Q33 == 1, 1, 0), 
               DevNotInMyNbhd = ifelse(DF02$Q33 == 2, 1, 0), 
               DKDevInMyNbhd = ifelse(DF02$Q33 == 8, 1, 0),
               DevInMyNbhdNA = ifelse(DF02$Q33 == 9, 1, 0))
 
# Thinking about your own personal housing situation, is the redevelopment in your
# neighborhood mainly a good thing for you or mainly a bad thing for you?
DF02 <- mutate(DF02, 
               DevGood4You = ifelse(DF02$Q34 == 1, 1, 0), 
               DevBad4You = ifelse(DF02$Q34 == 2, 1, 0), 
               DevNeither4You = ifelse(DF02$Q34 == 3, 1, 0),
               DevBoth4You = ifelse(DF02$Q34 == 4, 1, 0),
               DKDev4You = ifelse(DF02$Q34 == 8, 1, 0),
               Dev4YouNA = ifelse(DF02$Q34 == 9, 1, 0))

# Has redevelopment caused you to consider moving?
DF02 <- mutate(DF02, 
               DevYesMove = ifelse(DF02$Q35 == 1, 1, 0), 
               DevMaybeMove = ifelse(DF02$Q35 == 2, 1, 0), 
               DevNoMove = ifelse(DF02$Q35 == 3, 1, 0),
               DKDevMove = ifelse(DF02$Q35 == 4, 1, 0),
               DevMoveNoOpin = ifelse(DF02$Q35 == 8, 1, 0))


# if yes, have you seriously considered moving or just talked about it?
DF02 <- mutate(DF02, 
               DevYesMove = ifelse(DF02$Q35 == 1, 1, 0), 
               DevMaybeMove = ifelse(DF02$Q35 == 2, 1, 0), 
               DevNoMove = ifelse(DF02$Q35 == 3, 1, 0),
               DKDevMove = ifelse(DF02$Q35 == 4, 1, 0),
               DevMoveNoOpin = ifelse(DF02$Q35 == 8, 1, 0))


# Would you move within out outside of the district?
DF02 <- mutate(DF02, 
               DevYesMove = ifelse(DF02$Q35 == 1, 1, 0), 
               DevMaybeMove = ifelse(DF02$Q35 == 2, 1, 0), 
               DevNoMove = ifelse(DF02$Q35 == 3, 1, 0),
               DKDevMove = ifelse(DF02$Q35 == 4, 1, 0),
               DevMoveNoOpin = ifelse(DF02$Q35 == 8, 1, 0))


# Time in DC

# Ward
DF02 <- mutate(DF02, 
               Ward1 = ifelse(DF02$Q43 == 1, 1, 0), 
               Ward2 = ifelse(DF02$Q43 == 2, 1, 0), 
               Ward3 = ifelse(DF02$Q43 == 3, 1, 0),
               Ward4 = ifelse(DF02$Q43 == 4, 1, 0),
               Ward5 = ifelse(DF02$Q43 == 5, 1, 0), 
               Ward6 = ifelse(DF02$Q43 == 6, 1, 0), 
               Ward7 = ifelse(DF02$Q43 == 7, 1, 0),
               Ward8 = ifelse(DF02$Q43 == 8, 1, 0),
               WardDK = ifelse(DF02$Q43 == 98, 1, 0),
               WardNoOpin = ifelse(DF02$Q43 == 99, 1, 0))

# Quadrant Q40
DF02 <- mutate(DF02, 
               Northwest = ifelse(DF02$Q40 == 1, 1, 0), 
               Northeast = ifelse(DF02$Q40 == 2, 1, 0), 
               Southwest = ifelse(DF02$Q40 == 3, 1, 0),
               Southeast = ifelse(DF02$Q40 == 4, 1, 0),
               QuadDK = ifelse(DF02$Q40 == 8, 1, 0),
               QuadNoOpin = ifelse(DF02$Q40 == 9, 1, 0))

# Education: grade Q909
DF02 <- mutate(DF02, 
               Edu8thGrade = ifelse(DF02$Q35 == 1, 1, 0), 
               EduHS = ifelse(DF02$Q35 == 2, 1, 0), 
               EduGradHS = ifelse(DF02$Q35 == 3, 1, 0),
               EduSomeCollege = ifelse(DF02$Q35 == 4, 1, 0),
               EduGradCollege = ifelse(DF02$Q35 == 5, 1, 0),
               EduPostGrad = ifelse(DF02$Q35 == 6, 1, 0),
               EduDKNoOpin= ifelse(DF02$Q35 == 8, 1, 0),
               EduPostGrad = ifelse(DF02$Q35 == 9, 1, 0))

# Education: degree Q909a
DF02 <- mutate(DF02, 
               Assocaites = ifelse(DF02$Q909A == 1, 1, 0), 
               Bachelors = ifelse(DF02$Q909A  == 2, 1, 0), 
               Other = ifelse(DF02$Q909A  == 3, 1, 0),
               DKDegree = ifelse(DF02$Q909A  == 8, 1, 0),
               DegreeNoOpin = ifelse(DF02$Q909A  == 9, 1, 0))

# Age Q910



# Rent or Own
DF02 <- mutate(DF02, 
               Rent = ifelse(DF02$Q48 == 1, 1, 0), 
               Own = ifelse(DF02$Q48 == 2, 1, 0), 
               DKHouseHold = ifelse(DF02$Q48 == 8, 1, 0),
               HouseHoldNoOpin = ifelse(DF02$Q48 == 9, 1, 0))

# Q PID 
DF02 <- mutate(DF02, 
               DevYesMove = ifelse(DF02$Q35 == 1, 1, 0), 
               DevMaybeMove = ifelse(DF02$Q35 == 2, 1, 0), 
               DevNoMove = ifelse(DF02$Q35 == 3, 1, 0),
               DKDevMove = ifelse(DF02$Q35 == 4, 1, 0),
               DevMoveNoOpin = ifelse(DF02$Q35 == 8, 1, 0))

# Race: m918
DF02 <- mutate(DF02, 
               White = ifelse(DF02$RACE == 1, 1, 0), 
               Black = ifelse(DF02$RACE == 2, 1, 0), 
               WHisp = ifelse(DF02$RACE == 3, 1, 0),
               BHisp = ifelse(DF02$RACE == 4, 1, 0),
               Hisp = ifelse(DF02$RACE == 5, 1, 0),
               OtherRace = ifelse(DF02$RACE == 6, 1, 0),
               RaceNA = ifelse(DF02$RACE == 9, 1, 0),
               DKRace = ifelse(DF02$RACE == 8, 1, 0))

# Gender Q921
DF02 <- mutate(DF02, 
               Male = ifelse(DF02$SEX == 1, 1, 0), 
               Female = ifelse(DF02$SEX == 2, 1, 0))

# Do you think race relations in the district are getting better/worse or staying the same Q14
DF02 <- mutate(DF02, 
               DCRaceRelationsBetter = ifelse(DF02$Q14 == 1, 1, 0), 
               DCRaceRelationsWorse= ifelse(DF02$Q14 == 2, 1, 0), 
               DCRaceRelationsSame = ifelse(DF02$Q14 == 3, 1, 0),
               DCRaceRelationsDK = ifelse(DF02$Q14 == 4, 1, 0),
               DCRaceRelationsNoOpin = ifelse(DF02$Q14 == 8, 1, 0))

###### 2006 ############################################################################################################################################
#DC 2006
#install.packages("haven")
library(haven)

DF06 <- read_dta("/Users/aricaschuett/Documents/MiLO/Gentrification/uswash2006-117934.dta")

library(tidyverse)
#DF06 <- read_dta("/Users/aricaschuett/Documents/MiLO/Gentrification/22167-0001-Data.dta")

# Biggest Problem
DF06 <- mutate(DF06, 
               BP06Crime = ifelse(DF06$q13  == 1, 1, 0), 
               BP06School = ifelse(DF06$q13  == 2, 1, 0), 
               BP06Services = ifelse(DF06$q13  == 3, 1, 0), 
               BP06Jobs = ifelse(DF06$q13  == 4, 1, 0),
               BP06Terrorism = ifelse(DF06$q13  == 5, 1, 0), 
               BP06Housing = ifelse(DF06$q13  == 6, 1, 0), #"housing costs, low income housing"
               BP06Econ = ifelse(DF06$q13  == 50, 1, 0), 
               BP06Poverty = ifelse(DF06$q13  == 51, 1, 0), # Poverty/homelessness
               BP06TaxStructure = ifelse(DF06$q13  == 52, 1, 0), 
               BP06Budget = ifelse(DF06$q13  == 53, 1, 0), 
               BP06CityGov = ifelse(DF06$q13  == 54, 1, 0), 
               BP06SelfGov = ifelse(DF06$q13  == 55, 1, 0), 
               BP06Traffic = ifelse(DF06$q13  == 56, 1, 0), 
               BP06SocialServices = ifelse(DF06$q13  == 57, 1, 0), 
               BP06RaceRelations = ifelse(DF06$q13  == 58, 1, 0), 
               BP06Misc = ifelse(DF06$q13  == 97, 1, 0))

# Gentrification is good for...Whites
DF06 <- mutate(DF06, 
               DevGood4Whites = ifelse(DF06$q20a == 1, 1, 0), 
               DevBad4Whites = ifelse(DF06$q20a == 2, 1, 0), 
               DevNoOpinWhites = ifelse(DF06$q20a == 8, 1, 0))

# Gentrification is good for...Blacks
DF06 <- mutate(DF06, 
               DevGood4Blacks = ifelse(DF06$q20b == 1, 1, 0), 
               DevBad4Blacks = ifelse(DF06$q20b == 2 , 1, 0)) 
#           DevNoOpinBlacks = ifelse(DF06$q20b == NA, "DK No Opinion", 0))

# Gentrification is good for...The Rich
DF06 <- mutate(DF06, 
               DevGood4Rich = ifelse(DF06$q20c == 1, 1, 0), 
               DevBad4Rich = ifelse(DF06$q20c == 2, 1, 0), 
               DevNoOpinRich = ifelse(DF06$q20c == 8, 1, 0))

# Gentrification is good for...The Poor
DF06 <- mutate(DF06, 
               DevGood4Poor = ifelse(DF06$q20d == 1, 1, 0), 
               DevBad4Poor = ifelse(DF06$q20d == 2, 1, 0), 
               DevNoOpinPoor = ifelse(DF06$q20d == 8, 1, 0))

# Gentrification is good for...businesses
DF06 <- mutate(DF06, 
               DevGood4Business = ifelse(DF06$q20e == 1, 1, 0), 
               DevBad4Business = ifelse(DF06$q20e == 2, 1, 0), 
               DevNoOpinBusiness = ifelse(DF06$q20e == 8, 1, 0))

# Gentrification is good for...your neighborhood
DF06 <- mutate(DF06, 
               DevGood4Nbhd = ifelse(DF06$q20f == 1, 1, 0), 
               DevBad4Nbhd = ifelse(DF06$q20f == 2, 1, 0), 
               DevNoOpinNbhd = ifelse(DF06$q20f == 8, 1, 0))

# Gentrification is good for...people like you
DF06 <- mutate(DF06, 
               DevGood4PeopleLikeU = ifelse(DF06$q20g == 1, 1, 0), 
               DevBad4PeopleLikeU = ifelse(DF06$q20g == 2, 1, 0), 
               DevNoOpinPeopleLikeU = ifelse(DF06$q20g == 8, 1, 0))

# Gentrification is good for...The city
DF06 <- mutate(DF06, 
               DevGood4City = ifelse(DF06$q20h == 1, 1, 0), 
               DevBad4City = ifelse(DF06$q20h == 2, 1, 0), 
               DevNoOpinCity = ifelse(DF06$q20h == 8, 1, 0))

# Gentrification is good for...The middle class
DF06 <- mutate(DF06, 
               DevGood4MidCl = ifelse(DF06$q20i == 1, 1, 0), 
               DevBad4MidCl = ifelse(DF06$q20i == 2, 1, 0), 
               DevNoOpinMidCl = ifelse(DF06$q20i == 8, 1, 0))

# Forced to move
DF06 <- mutate(DF06, 
               ForcedMoveLikely = ifelse(DF06$q21 == 1, 1, 0), 
               ForcedMovePossible = ifelse(DF06$q21 == 2, 1, 0), 
               ForcedMoveUnlikely = ifelse(DF06$q21 == 3, 1, 0), 
               ForcedMoveHappened = ifelse(DF06$q21 == 4, 1, 0))

# own/rent
DF06 <- mutate(DF06, 
               Rent = ifelse(DF06$q42 == 1, 1, 0), 
               Own = ifelse(DF06$q42 == 2, 1, 0), 
               Other = ifelse(DF06$q42 == 3, 1, 0))

# Could you find a home if you needed to move?
DF06 <- mutate(DF06, 
               CouldFindHome = ifelse(DF06$q22 == 1, 1, 0), 
               CouldNotFindHome= ifelse(DF06$q22 == 2, 1, 0))

# if yes, could you find one in the district?
DF06 <- mutate(DF06, 
               FindHomeinDC = ifelse(DF06$q23 == 1, 1, 0), 
               FindHomeinBurbs = ifelse(DF06$q23 == 2, 1, 0))

# Are the Washington nationals good/bad?
DF06 <- mutate(DF06, 
               MBLGood = ifelse(DF06$q32 == 1, 1, 0), 
               MLBBad = ifelse(DF06$q32 == 2, 1, 0), 
               MBLMixed = ifelse(DF06$q32 == 3, 1, 0))

# Are the Washington nationals good/bad for SE DC residents?
DF06 <- mutate(DF06, 
               MLBGoodSE = ifelse(DF06$q33 == 1, 1, 0), 
               MLBBadSE = ifelse(DF06$q33 == 2, 1, 0), 
               MLBNoImpactSE = ifelse(DF06$q33 == 3, 1, 0))

# Have you been to a baseball game?
DF06 <- mutate(DF06, 
               Been2MLB = ifelse(DF06$q34 == 1, 1, 0), 
               NotBeen2MLB = ifelse(DF06$q34  == 2, 1, 0))


# Registered Voter
DF06 <- mutate(DF06, 
               Registered = ifelse(DF06$q1 == 1, 1, 0), 
               NotRegistered = ifelse(DF06$q2  == 2, 1, 0),
               DKRegistered = ifelse(DF06$q2  == 2, 1, 0))

# Income
DF06 <- mutate(DF06, 
               IncomeMissing = ifelse(DF06$income == -1, 1, 0), 
               IncomeUnder20k = ifelse(DF06$income == 1, 1, 0),
               Income20kto35k = ifelse(DF06$income  == 2, 1, 0),
               Income35kto50k = ifelse(DF06$income == 3, 1, 0),
               Income50kto75k = ifelse(DF06$income == 4, 1, 0), 
               Income75kto100k = ifelse(DF06$income == 5, 1, 0),
               Income75kto100k = ifelse(DF06$income == 6, 1, 0),
               Income100kplus = ifelse(DF06$income  == 7, 1, 0),
               IncomeDKNoOpn = ifelse(DF06$income == 8, 1, 0))

# Race
DF06 <- mutate(DF06, 
              RaceMissing = ifelse(DF06$q918 == -1, 1, 0), 
              RaceWhite = ifelse(DF06$q918 == 1, 1, 0),
              RaceBlack = ifelse(DF06$q918  == 2, 1, 0),
              RaceWhtHisp = ifelse(DF06$q918 == 3, 1, 0), 
              RaceBlkHisp = ifelse(DF06$q918 == 4, 1, 0),
              RaceHisp = ifelse(DF06$q918  == 5, 1, 0),
              RaceAsian = ifelse(DF06$q918 == 6, 1, 0),
              RaceOther = ifelse(DF06$q918  == 7, 1, 0),
              RaceDKNoOpn = ifelse(DF06$q918 == 8, 1, 0))

# Education
DF06 <- mutate(DF06, 
               EduMissing = ifelse(DF06$q909 == 1, 1, 0), 
               EduLess8grade = ifelse(DF06$q909 == 2, 1, 0),
               EduSomeHS = ifelse(DF06$q909  == 2, 1, 0),
               EduGradHS = ifelse(DF06$q909 == 1, 1, 0), 
               EduSomeCollege = ifelse(DF06$q909 == 2, 1, 0),
               EduGradCollege = ifelse(DF06$q909  == 2, 1, 0),
               EduPostGrad = ifelse(DF06$q909 == 1, 1, 0))

# Ward
DF06 <- mutate(DF06, 
               Ward1 = ifelse(DF06$q38 == 1, 1, 0), 
               Ward2 = ifelse(DF06$q38 == 2, 1, 0),
               Ward3 = ifelse(DF06$q38  == 3, 1, 0),
               Ward4 = ifelse(DF06$q38 == 4, 1, 0), 
               Ward5 = ifelse(DF06$q38 == 5, 1, 0),
               Ward6 = ifelse(DF06$q38  == 6, 1, 0),
               Ward7 = ifelse(DF06$q38 == 7, 1, 0),
               Ward8 = ifelse(DF06$q38 == 8, 1, 0),
               WardDKNoOpn = ifelse(DF06$q38 == 9, 1, 0),
               WardMissing = ifelse(DF06$q38 == -1, 1, 0))

# Gender
DF06 <- mutate(DF06, 
               Male = ifelse(DF06$q921 == 1, 1, 0), 
               Female = ifelse(DF06$q921 == 2, 1, 0))

colnames(DF06)[colnames(DF06) == "q2"] <- "Party"
colnames(DF06)[colnames(DF06) == "racenet"] <- "Race"
colnames(DF06)[colnames(DF06) == "q38"] <- "Ward"
colnames(DF06)[colnames(DF06) == "q921"] <- "Gender"
colnames(DF06)[colnames(DF06) == "q910"] <- "Age"
colnames(DF06)[colnames(DF06) == "q39"] <- "TimeInDC"

#colnames(DF06)[colnames(DF06) == "q20b"] <- "Dev4Blks"


###### 2008 ##########################################################################################################################################
DF08 <- read_dta("/Users/aricaschuett/Documents/MiLO/Gentrification/24602-0001-Data (1).dta")


# What is the biggest problem facing the district today, the one you want the maor to work the hardest to solve?
DF08 <- mutate(DF08, 
               BP06Crime = ifelse(DF08$Q4  == 1, 1, 0), 
               BP06School = ifelse(DF08$Q4  == 2, 1, 0), 
               BP06Services = ifelse(DF08$Q4  == 3, 1, 0), 
               BP06Jobs = ifelse(DF08$Q4  == 4, 1, 0),
               BP06Terrorism = ifelse(DF08$Q4  == 5, 1, 0), 
               BP06Housing = ifelse(DF08$Q4  == 6, 1, 0), #"housing costs, low income housing"
               BP06Econ = ifelse(DF08$Q4  == 7, 1, 0), 
               BP06Poverty = ifelse(DF08$Q4  == 8, 1, 0), # Poverty/homelessness
               BP06TaxStructure = ifelse(DF08$Q4  == 9, 1, 0), 
               BP06Budget = ifelse(DF08$Q4  == 10, 1, 0), 
               BP06CityGov = ifelse(DF08$Q4  == 11, 1, 0), 
               BP06SelfGov = ifelse(DF08$Q4  == 12, 1, 0), 
               BP06Traffic = ifelse(DF08$Q4  == 13, 1, 0), 
               BP06SocialServices = ifelse(DF08$Q4 == 14, 1, 0), 
               BP06RaceRelations = ifelse(DF08$Q4  == 15, 1, 0), 
               BP06Taxis = ifelse(DF08$Q4  == 16, 1, 0), 
               BP06Tax = ifelse(DF08$Q4  == 17, 1, 0), 
               BP06HIV = ifelse(DF08$Q4  == 18, 1, 0),
               BP06Other = ifelse(DF08$Q4  == 19, 1, 0), 
               BP06Misc = ifelse(DF08$Q4  == 97, 1, 0))

# What is the second biggest problem facing the district today, the one you want the maor to work the hardest to solve?
DF08 <- mutate(DF08, 
               SndBP06Crime = ifelse(DF08$Q5  == 1, 1, 0), 
               SndBP06School = ifelse(DF08$Q5  == 2, 1, 0), 
               SndBP06Services = ifelse(DF08$Q5  == 3, 1, 0), 
               SndBP06Jobs = ifelse(DF08$Q5  == 4, 1, 0),
               SndBP06Terrorism = ifelse(DF08$Q5  == 5, 1, 0), 
               SndBP06Housing = ifelse(DF08$Q5  == 6, 1, 0), #"housing costs, low income housing"
               SndBP06Econ = ifelse(DF08$Q5  == 7, 1, 0), 
               SndBP06Poverty = ifelse(DF08$Q5  == 8, 1, 0), # Poverty/homelessness
               SndBP06TaxStructure = ifelse(DF08$Q5  == 9, 1, 0), 
               SndBP06Budget = ifelse(DF08$Q5  == 10, 1, 0), 
               SndBP06CityGov = ifelse(DF08$Q5  == 11, 1, 0), 
               SndBP06SelfGov = ifelse(DF08$Q5  == 12, 1, 0), 
               SndBP06Traffic = ifelse(DF08$Q5  == 13, 1, 0), 
               SndBP06SocialServices = ifelse(DF08$Q5 == 14, 1, 0), 
               SndBP06RaceRelations = ifelse(DF08$Q5  == 15, 1, 0), 
               SndBP06Taxis = ifelse(DF08$Q5  == 16, 1, 0), 
               SndBP06Tax = ifelse(DF08$Q5  == 17, 1, 0), 
               SndBP06HIV = ifelse(DF08$Q5  == 18, 1, 0),
               SndBP06Other = ifelse(DF08$Q5  == 19, 1, 0), 
               SndBP06Misc = ifelse(DF08$Q5  == 97, 1, 0))


# Are you satisfied or dissatisfied with the number of African Americans Fenty has appointed to serve in city government?
DF08 <- mutate(DF08, 
               VSatisfiedAAinGOV = ifelse(DF08$Q11 == 1, 1, 0), 
               SatisfiedAAinGOV = ifelse(DF08$Q11 == 1, 1, 0), 
               DisatisfiedAAinGOV  = ifelse(DF08$Q11 == 2, 1, 0), 
               VDisatisfiedAAinGOV = ifelse(DF08$Q11 == 8, 1, 0))

#  Changing topics...these days, how safe from crime do you feel in your neighborhood?
#  Would you say you feel very safe from crime, somewhat safe, not too safe, or not safe at all?
DF08 <- mutate(DF08, 
               VSafeMyNbhd = ifelse(DF08$Q16 == 1, 1, 0), 
               SafeMyNbhd = ifelse(DF08$Q16 == 1, 1, 0), 
               NotSafeMyNbhd = ifelse(DF08$Q16 == 2, 1, 0), 
               VNotSafeMyNbhd = ifelse(DF08$Q16 == 8, 1, 0))

# As you may know, the district government is trying to redevelop parts of the city to attract new businesses and residents. 
# Do you think this process of redevelopment is mainly good or mainly bad for ...The rich
DF08 <- mutate(DF08, 
               DevGood4Rich = ifelse(DF08$Q17_1 == 1, 1, 0), 
               DevBad4Rich = ifelse(DF08$Q17_1 == 2, 1, 0), 
               DevNoOpinRich = ifelse(DF08$Q17_1 == 8, 1, 0))

# Do you think this process of redevelopment is mainly good or mainly bad for ...The poor
DF08 <- mutate(DF08, 
               DevGood4Poor = ifelse(DF08$Q17_2 == 1, 1, 0), 
               DevBad4Poor = ifelse(DF08$Q17_2 == 2, 1, 0), 
               DevNoOpinPoor = ifelse(DF08$Q17_2 == 8, 1, 0))


# Do you think this process of redevelopment is mainly good or mainly bad for ...your neighborhood
DF08 <- mutate(DF08, 
               DevGood4YourNbhd= ifelse(DF08$Q17_3 == 1, 1, 0), 
               DevBad4YourNbhd = ifelse(DF08$Q17_3 == 2, 1, 0), 
               DevNoOpinYourNbhd = ifelse(DF08$Q17_3 == 8, 1, 0))

# Do you think this process of redevelopment is mainly good or mainly bad for ...The city overall
DF08 <- mutate(DF08, 
               DevGood4City = ifelse(DF08$Q17_4 == 1, 1, 0), 
               DevBad4City = ifelse(DF08$Q17_4 == 2, 1, 0), 
               DevNoOpinCity = ifelse(DF08$Q17_4 == 8, 1, 0))

# Do you think there is any chance you will be forced to move out of your current neighborhood because of redevelopment, 
# or do you think that will happen to you?
DF08 <- mutate(DF08, 
               ForcedMoveLikely = ifelse(DF08$Q18 == 1, 1, 0), 
               ForcedMovePossible = ifelse(DF08$Q18 == 2, 1, 0), 
               ForcedMoveUnlikely = ifelse(DF08$Q18 == 3, 1, 0), 
               ForcedMoveHappened = ifelse(DF08$Q18 == 4, 1, 0))

# Are you currently renting the place where you now live or do you own it?
DF08 <- mutate(DF08, 
               Rent = ifelse(DF08$Q19 == 1, 1, 0), 
               Own = ifelse(DF08$Q19 == 2, 1, 0), 
               Other = ifelse(DF08$Q19 == 8, 1, 0))

# If you had to move today, do you think you could (buy a home/find a rental) in the District that you 
# would like and could afford, or do you think you would have to move to the suburbs to find something you would like and could afford?
DF08 <- mutate(DF08, 
               CouldFindHome = ifelse(DF08$Q20 == 1, 1, 0), 
               CouldNotFindHome = ifelse(DF08$Q20 == 2, 1, 0))


# If you were looking to buy a home, do you think you could find something in the District that you would like and could afford, or do you
# think you would have to move to the suburbs to find that?
DF08 <- mutate(DF08, 
               FindHomeinDC = ifelse(DF08$Q21 == 1, 1, 0), 
               FindHomeinBurbs = ifelse(DF08$Q21 == 2, 1, 0))

# On another subject, considering everything, do you think bringing the Washington Nationals major league baseball team to the District has 
# been a (good thing) or a (bad thing) for the city?
DF08 <- mutate(DF08, 
               MLBGood4DC = ifelse(DF08$Q34 == 1, 1, 0), 
               MLBBad4DC = ifelse(DF08$Q34 == 2, 1, 0), 
               MLBMixed4DC = ifelse(DF08$Q34 == 8, 1, 0))


# You may have heard that a new publicly funded baseball stadium for the Nationals opens in Southeast Washington this year. 
# Do you think the new stadium will have a (positive) impact, a (negative) impact, or no impact on the residents of SE DC?
DF08 <- mutate(DF08, 
               MLBGood4SERes = ifelse(DF08$Q35 == 1, 1, 0), 
               MLBBad4SERes = ifelse(DF08$Q35 == 2, 1, 0), 
               MLBNoOpinSERes = ifelse(DF08$Q35 == 8, 1, 0))

# Generally speaking, do you favor or oppose using city funds to help finance a new soccer stadium for the District's Major League Soccer team, DC United?
DF08 <- mutate(DF08, 
               StrgFavMLSStadium = ifelse(DF08$Q36 == 1, 1, 0), 
               FavMLSStadium = ifelse(DF08$Q36 == 2, 1, 0), 
               OppMLSStadium = ifelse(DF08$Q36 == 3, 1, 0), 
               StrgOppMLSStadium = ifelse(DF08$Q36 == 4, 1, 0))

# How concerned are you right now about not being able to pay your rent, mortgage, or other housing costs-are you worried, 
# moderately worried, not too worried, or not worried at all?
DF08 <- mutate(DF08, 
               VWorriedHousingCosts = ifelse(DF08$Q38 == 1, 1, 0), 
               WorriedHousingCosts = ifelse(DF08$Q38 == 2, 1, 0),
               NotWorriedHousingCosts = ifelse(DF08$Q38 == 3, 1, 0), 
               NotAtAllWorriedHousingCosts = ifelse(DF08$Q38 == 4, 1, 0))

# What describes your family's financial situation?
DF08 <- mutate(DF08, 
               GettingAhead = ifelse(DF08$Q39 == 1, 1, 0), 
               Maintain = ifelse(DF08$Q39 == 2, 1, 0), 
               FallBedhind = ifelse(DF08$Q39 == 3, 1, 0))

# ward
DF08 <- mutate(DF08, 
               Ward1 = ifelse(DF08$Q41 == 1, 1, 0), 
               Ward2 = ifelse(DF08$Q41 == 2, 1, 0),
               Ward3 = ifelse(DF08$Q41 == 3, 1, 0),
               Ward4 = ifelse(DF08$Q41 == 4, 1, 0),
               Ward5 = ifelse(DF08$Q41 == 5, 1, 0),
               Ward6 = ifelse(DF08$Q41 == 6, 1, 0),
               Ward7 = ifelse(DF08$Q41 == 7, 1, 0),
               Ward8 = ifelse(DF08$Q41 == 8, 1, 0))

# quadrant
DF08 <- mutate(DF08, 
               NW = ifelse(DF08$Q40 == 1, 1, 0), 
               NE = ifelse(DF08$Q40 == 2, 1, 0),
               SW = ifelse(DF08$Q40 == 3, 1, 0), 
               SE = ifelse(DF08$Q40 == 4, 1, 0))

# houshold member work in gov?
DF08 <- mutate(DF08, 
               WorkFedHH = ifelse(DF08$Q42 == 1, 1, 0), 
               WorkCityHH = ifelse(DF08$Q42 == 2, 1, 0), 
               BothFedCityHH = ifelse(DF08$Q42 == 3, 1, 0),
               NeitherFedCityHH = ifelse(DF08$Q42 == 4, 1, 0))

# PID
DF08 <- mutate(DF08, 
               Dem = ifelse(DF08$Q901 == 1, 1, 0), 
               Rep = ifelse(DF08$Q901 == 2, 1, 0), 
               Indep = ifelse(DF08$Q901 == 3, 1, 0))

# If Independent or DK NA
DF08 <- mutate(DF08, 
               Dem = ifelse(DF08$Q904 == 1, 1, 0), 
               Rep = ifelse(DF08$Q904 == 2, 1, 0), 
               Neither = ifelse(DF08$Q904 == 3, 1, 0))

# Registered Voter
DF08 <- mutate(DF08, 
               Reg2Vote = ifelse(DF08$Q905 == 1, 1, 0), 
               NotReg2Vote = ifelse(DF08$Q905 == 2, 1, 0))

# Ideology
DF08 <- mutate(DF08, 
               Liberal = ifelse(DF08$Q908A == 1, 1, 0), 
               Moderate = ifelse(DF08$Q908A == 2, 1, 0), 
               Conservative = ifelse(DF08$Q908A == 3, 1, 0),
               DontThinkLikeThat = ifelse(DF08$Q908A == 4, 1, 0),)


# Last Grade
DF08 <- mutate(DF08, 
               lessthan8thgrd = ifelse(DF08$Q909 == 1, 1, 0), 
               someHS = ifelse(DF08$Q909 == 2, 1, 0), 
               HSGrad = ifelse(DF08$Q909 == 3, 1, 0),
               SomeCollege = ifelse(DF08$Q909 == 4, 1, 0),
               CollegeGrad = ifelse(DF08$Q909 == 5, 1, 0),
               PostGrad = ifelse(DF08$Q909 == 6, 1, 0))

# College
DF08 <- mutate(DF08, 
               AADegree = ifelse(DF08$Q909A == 1, 1, 0), 
               Bachelors = ifelse(DF08$Q909A == 2, 1, 0), 
               Other = ifelse(DF08$Q909A == 3, 1, 0))


# Age by number is Q910


# Are you Hispanic origin or background?
DF08 <- mutate(DF08, 
               YesHispanic = ifelse(DF08$Q918 == 1, 1, 0), 
               NotHispanic = ifelse(DF08$Q918 == 2, 1, 0))


#colnames(DF08)[colnames(DF08) == "Q918"] <- "Race"
# Are you black/white some other race?
DF08 <- mutate(DF08,
               White = ifelse(DF08$Q918 == 1, 1, 0),
               Black = ifelse(DF08$Q918 == 2, 1, 0),
               WhiteHispanic = ifelse(DF08$Q918 == 3, 1, 0),
               BlackHispanic = ifelse(DF08$Q918 == 4, 1, 0),
               Asian = ifelse(DF08$Q918 == 5, 1, 0),
               OtherRace = ifelse(DF08$Q918 == 6, 1, 0),
               RaceMissing = ifelse(DF08$Q918 == -1, 1, 0))


# Income
DF08 <- mutate(DF08, 
               Under20k = ifelse(DF08$INCOME == 1, 1, 0), 
               From20kto35k = ifelse(DF08$INCOME == 2, 1, 0), 
               From35kto50k = ifelse(DF08$INCOME == 8, 1, 0),
               From50kto75k = ifelse(DF08$INCOME == 8, 1, 0),
               From75kto100k = ifelse(DF08$INCOME == 8, 1, 0),
               Over100k = ifelse(DF08$INCOME == 8, 1, 0))

# 100k+?
DF08 <- mutate(DF08, 
               From100kto150k = ifelse(DF08$INCOME2 == 1, 1, 0), 
               From150kto200k = ifelse(DF08$INCOME2 == 2, 1, 0), 
               Over200k = ifelse(DF08$INCOME2 == 8, 1, 0))

# Gender
DF08 <- mutate(DF08, 
               Male = ifelse(DF08$Q921 == 1, 1, 0), 
               Female = ifelse(DF08$Q921 == 2, 1, 0))

colnames(DF08)[colnames(DF08) == "Q910"] <- "Age"

###### Visualizations 2006-08 #################################################################################################################################################
###### black views of gentrification for BLACKS 2006 #############

blackres06 <- filter(DF06, Race == 2)
blackOpinonGenforBlks06 <- 
  blackres06 %>%
  count(q20b)

blackOpinonGenforBlks06$q20b <- as.numeric(blackOpinonGenforBlks06$q20b)
blackOpinonGenforBlks06$q20b[blackOpinonGenforBlks06$q20b== 1] <- "Mostly Good"
blackOpinonGenforBlks06$q20b[blackOpinonGenforBlks06$q20b== 2] <- "Mostly Bad"
blackOpinonGenforBlks06$q20b[blackOpinonGenforBlks06$q20b== 8] <- "DK No Opinion"

plotBlackview06 <- 
  ggplot(blackOpinonGenforBlks06, aes(x = q20b, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "Black Residents Views of Redevelopment for Blacks 2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview06


###### white views of gentrification for BLACKS 2006 #########################################
whiteres06 <- filter(DF06, Race == 1)
whiteOpinonGenforBlks06 <- 
  whiteres06 %>%
  count(q20b)

whiteOpinonGenforBlks06$q20b <- as.numeric(whiteOpinonGenforBlks06$q20b)
whiteOpinonGenforBlks06$q20b[whiteOpinonGenforBlks06$q20b== 1] <- "Mostly Good"
whiteOpinonGenforBlks06$q20b[whiteOpinonGenforBlks06$q20b== 2] <- "Mostly Bad"
whiteOpinonGenforBlks06$q20b[whiteOpinonGenforBlks06$q20b== 8] <- "DK No Opinion"

plotwhiteview06 <- 
  ggplot(whiteOpinonGenforBlks06, aes(x = q20b, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "White Residents Views of Redevelopment for Blacks 2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotwhiteview06


## Cowplot them
RD_Opn_byRace06 <- plot_grid(plotBlackview06, plotwhiteview06, 
                               nrow = 1)
RD_Opn_byRace06

###### Black Views on gentrification for WHITE 2006 ############################
blackOpinonGenforWht06 <- 
  blackres06 %>%
  count(q20a)

blackOpinonGenforWht06$q20a <- as.numeric(blackOpinonGenforWht06$q20a)
blackOpinonGenforWht06$q20a[blackOpinonGenforWht06$q20a== 1] <- "Mostly Good"
blackOpinonGenforWht06$q20a[blackOpinonGenforWht06$q20a== 2] <- "Mostly Bad"
blackOpinonGenforWht06$q20a[blackOpinonGenforWht06$q20a== 8] <- "DK No Opinion"

plotBlackview4Wht06 <- 
  ggplot(blackOpinonGenforWht06, aes(x = q20a, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "Black Residents Views of Redevelopment for whites 2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview4Wht06

###### White Views on gentrification for WHITES 2006 ##############################
whiteOpinonGenforWht06 <- 
  whiteres06 %>%
  count(q20a)

whiteOpinonGenforWht06$q20a <- as.numeric(whiteOpinonGenforWht06$q20a)
whiteOpinonGenforWht06$q20a[whiteOpinonGenforWht06$q20a== 1] <- "Mostly Good"
whiteOpinonGenforWht06$q20a[whiteOpinonGenforWht06$q20a== 2] <- "Mostly Bad"
whiteOpinonGenforWht06$q20a[whiteOpinonGenforWht06$q20a== 8] <- "DK No Opinion"

plotwhiteview4Wht06 <- 
  ggplot(whiteOpinonGenforWht06, aes(x = q20a, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "White Residents Views of Redevelopment for whites 2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotwhiteview4Wht06

## Cowplot them
RD_Opn_byRace06 <- plot_grid(plotBlackview4Wht06, plotwhiteview4Wht06, 
                             nrow = 1)
RD_Opn_byRace06

##### Black views on gentrification for THE RICH 2006 ##############################
blackOpinonGenforRich06 <- 
  blackres06 %>%
  count(q20c)

blackOpinonGenforRich06$q20c <- as.numeric(blackOpinonGenforRich06$q20c)
blackOpinonGenforRich06$q20c[blackOpinonGenforRich06$q20c== 1] <- "Mostly Good"
blackOpinonGenforRich06$q20c[blackOpinonGenforRich06$q20c== 2] <- "Mostly Bad"
blackOpinonGenforRich06$q20c[blackOpinonGenforRich06$q20c== 8] <- "DK No Opinion"

plotBlackview4Rich06 <- 
  ggplot(blackOpinonGenforRich06, aes(x = q20c, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "Black Residents Views of Redevelopment for The Rich 2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview4Rich06

##### White views on gentrification for THE RICH 2006 ##############################
WhtOpinonGenforRich06 <- 
  whiteres06 %>%
  count(q20c)

WhtOpinonGenforRich06$q20c <- as.numeric(WhtOpinonGenforRich06$q20c)
WhtOpinonGenforRich06$q20c[WhtOpinonGenforRich06$q20c== 1] <- "Mostly Good"
WhtOpinonGenforRich06$q20c[WhtOpinonGenforRich06$q20c== 2] <- "Mostly Bad"
WhtOpinonGenforRich06$q20c[WhtOpinonGenforRich06$q20c== 8] <- "DK No Opinion"

plotWhtview4Rich06 <- 
  ggplot(WhtOpinonGenforRich06, aes(x = q20c, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "White Residents Views of Redevelopment for The Rich 2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotWhtview4Rich06

## Cowplot them
RD_Opn_byRace06 <- plot_grid(plotBlackview4Rich06, plotWhtview4Rich06, 
                             nrow = 1)
RD_Opn_byRace06
###### Black views on gentrification for YOUR NEIGHBORHOOD 2006 ##################
blackOpinonGenforNbhd06 <- 
  blackres06 %>%
  count(q20f)

blackOpinonGenforNbhd06$q20f <- as.numeric(blackOpinonGenforNbhd06$q20f)
blackOpinonGenforNbhd06$q20f[blackOpinonGenforNbhd06$q20f== 1] <- "Mostly Good"
blackOpinonGenforNbhd06$q20f[blackOpinonGenforNbhd06$q20f== 2] <- "Mostly Bad"
blackOpinonGenforNbhd06$q20f[blackOpinonGenforNbhd06$q20f== 8] <- "DK No Opinion"

plotBlackview4Nbhd06 <- 
  ggplot(blackOpinonGenforNbhd06, aes(x = q20f, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "Black Residents Views of Redevelopment for Their Nbhd 2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview4Nbhd06

##### White views on gentrification for YOUR NEIGHBORHOOD 2006 ##############################
WhtOpinonGenforNbhd06 <- 
  whiteres06 %>%
  count(q20f)

WhtOpinonGenforNbhd06$q20f <- as.numeric(WhtOpinonGenforNbhd06$q20f)
WhtOpinonGenforNbhd06$q20f[WhtOpinonGenforNbhd06$q20f== 1] <- "Mostly Good"
WhtOpinonGenforNbhd06$q20f[WhtOpinonGenforNbhd06$q20f== 2] <- "Mostly Bad"
WhtOpinonGenforNbhd06$q20f[WhtOpinonGenforNbhd06$q20f== 8] <- "DK No Opinion"

plotWhtview4Nbhd06 <- 
  ggplot(WhtOpinonGenforNbhd06, aes(x = q20f, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "White Residents Views of Redevelopment for Their Nbhd 2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotWhtview4Nbhd06

## Cowplot them
RD_Opn_byRace06 <- plot_grid(plotBlackview4Nbhd06, plotWhtview4Nbhd06, 
                             nrow = 1)
RD_Opn_byRace06
###### Black views on gentrification for PEOPLE LIKE YOU  2006 #############
BlkOpnGenforPplLikeU06 <- 
  blackres06 %>%
  count(q20g)

BlkOpnGenforPplLikeU06$q20g <- as.numeric(BlkOpnGenforPplLikeU06$q20g)
BlkOpnGenforPplLikeU06$q20g[BlkOpnGenforPplLikeU06$q20g== 1] <- "Mostly Good"
BlkOpnGenforPplLikeU06$q20g[BlkOpnGenforPplLikeU06$q20g== 2] <- "Mostly Bad"
BlkOpnGenforPplLikeU06$q20g[BlkOpnGenforPplLikeU06$q20g== 8] <- "DK No Opinion"

plotBlackview4PPL06 <- 
  ggplot(BlkOpnGenforPplLikeU06, aes(x = q20g, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "Black Views of Redevelopment for People like You 2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview4PPL06

##### White views on gentrification for PEOPLE LIKE YOU 2006 ##############################
WhtOpnGenforPplLikeU06 <- 
  whiteres06 %>%
  count(q20g)

WhtOpnGenforPplLikeU06$q20g <- as.numeric(WhtOpnGenforPplLikeU06$q20g)
WhtOpnGenforPplLikeU06$q20g[WhtOpnGenforPplLikeU06$q20g== 1] <- "Mostly Good"
WhtOpnGenforPplLikeU06$q20g[WhtOpnGenforPplLikeU06$q20g== 2] <- "Mostly Bad"
WhtOpnGenforPplLikeU06$q20g[WhtOpnGenforPplLikeU06$q20g== 8] <- "DK No Opinion"

plotWhtview4PPL06 <- 
  ggplot(WhtOpnGenforPplLikeU06, aes(x = q20g, y = n/sum(n)*100)) +
  geom_col() +                                                     #### removed expansion
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "White Views of Redevelopment for People Like You 2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotWhtview4PPL06

## Cowplot them
RD_Opn_byRace06 <- plot_grid(plotBlackview4PPL06, plotWhtview4PPL06, 
                             nrow = 1)
RD_Opn_byRace06
###### Black views on gentrification for THE CITY OVERALL  2006 #############
BlkOpnGenforCITY06 <- 
  blackres06 %>%
  count(q20h)

BlkOpnGenforCITY06$q20h <- as.numeric(BlkOpnGenforCITY06$q20h)
BlkOpnGenforCITY06$q20h[BlkOpnGenforCITY06$q20h== 1] <- "Mostly Good"
BlkOpnGenforCITY06$q20h[BlkOpnGenforCITY06$q20h== 2] <- "Mostly Bad"
BlkOpnGenforCITY06$q20h[BlkOpnGenforCITY06$q20h== 8] <- "DK No Opinion"

plotBlackview4CITY06 <- 
  ggplot(BlkOpnGenforCITY06, aes(x = q20h, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "Black Views of Redevelopment for The City  2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview4CITY06

##### White views on gentrification for THE CITY OVERALL 2006 ##############################
WhtOpnGenforCITY06 <- 
  whiteres06 %>%
  count(q20h)

WhtOpnGenforCITY06$q20h <- as.numeric(WhtOpnGenforCITY06$q20h)
WhtOpnGenforCITY06$q20h[WhtOpnGenforCITY06$q20h== 1] <- "Mostly Good"
WhtOpnGenforCITY06$q20h[WhtOpnGenforCITY06$q20h== 2] <- "Mostly Bad"
WhtOpnGenforCITY06$q20h[WhtOpnGenforCITY06$q20h== 8] <- "DK No Opinion"

plotWhtview4CITY06 <- 
  ggplot(WhtOpnGenforCITY06, aes(x = q20h, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +  #### removed expansion
  labs(y = "Percent", x= "views", title = "White Views of Redevelopment for The City 2006")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotWhtview4CITY06

## Cowplot them
RD_Opn_byRace06 <- plot_grid(plotBlackview4CITY06, plotWhtview4CITY06, 
                             nrow = 1)
RD_Opn_byRace06
###### black views of gentrification for THE RICH 2008 #############
blackres08 <- filter(DF08, RACENET == 2)
blackOpinonGenforRICH08 <- 
  blackres08 %>%
  count(Q17_1)   # The Rich 17_1, The Poor 17_2, The nbhd 17_3, the city 17_4

blackOpinonGenforRICH08$Q17_1 <- as.numeric(blackOpinonGenforRICH08$Q17_1)
blackOpinonGenforRICH08$Q17_1[blackOpinonGenforRICH08$Q17_1== 1] <- "Mostly Good"
blackOpinonGenforRICH08$Q17_1[blackOpinonGenforRICH08$Q17_1== 2] <- "Mostly Bad"
blackOpinonGenforRICH08$Q17_1[blackOpinonGenforRICH08$Q17_1== 8] <- "DK No Opinion"

plotBlackview08 <- 
  ggplot(blackOpinonGenforRICH08, aes(x = Q17_1, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "Black Views of Redevelopment for The Rich 2008")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview08

###### white views of gentrification for THE RICH 2008 #########################################
whiteres08 <- filter(DF08, RACENET == 1)
whiteOpinonGenforRICH08 <- 
  whiteres08 %>%
  count(Q17_1)

whiteOpinonGenforRICH08$Q17_1 <- as.numeric(whiteOpinonGenforRICH08$Q17_1)
whiteOpinonGenforRICH08$Q17_1[whiteOpinonGenforRICH08$Q17_1== 1] <- "Mostly Good"
whiteOpinonGenforRICH08$Q17_1[whiteOpinonGenforRICH08$Q17_1== 2] <- "Mostly Bad"
whiteOpinonGenforRICH08$Q17_1[whiteOpinonGenforRICH08$Q17_1== 8] <- "DK No Opinion"

plotwhiteview08 <- 
  ggplot(whiteOpinonGenforRICH08, aes(x = Q17_1, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "White Views of Redevelopment for The Rich 2008")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotwhiteview08


## Cowplot them
RD_Opn_byRace08 <- plot_grid(plotBlackview08, plotwhiteview08, 
                             nrow = 1)
RD_Opn_byRace08

###### Black Views on gentrification for THE POOR 2008 ############################
blackOpinonGenforPOOR08 <- 
  blackres08 %>%
  count(Q17_2)

blackOpinonGenforPOOR08$Q17_2 <- as.numeric(blackOpinonGenforPOOR08$Q17_2)
blackOpinonGenforPOOR08$Q17_2[blackOpinonGenforPOOR08$Q17_2== 1] <- "Mostly Good"
blackOpinonGenforPOOR08$Q17_2[blackOpinonGenforPOOR08$Q17_2== 2] <- "Mostly Bad"
blackOpinonGenforPOOR08$Q17_2[blackOpinonGenforPOOR08$Q17_2== 8] <- "DK No Opinion"

plotBlackview4Poor08 <- 
  ggplot(blackOpinonGenforPOOR08, aes(x = Q17_2, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "Black Views of Redevelopment for The Poor 2008")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview4Poor08

###### White Views on gentrification for THE POOR 2008 ##############################
whiteOpinonGenforPOOR08 <- 
  whiteres08 %>%
  count(Q17_2)

whiteOpinonGenforPOOR08$Q17_2 <- as.numeric(whiteOpinonGenforPOOR08$Q17_2)
whiteOpinonGenforPOOR08$Q17_2[whiteOpinonGenforPOOR08$Q17_2== 1] <- "Mostly Good"
whiteOpinonGenforPOOR08$Q17_2[whiteOpinonGenforPOOR08$Q17_2== 2] <- "Mostly Bad"
whiteOpinonGenforPOOR08$Q17_2[whiteOpinonGenforPOOR08$Q17_2== 8] <- "DK No Opinion"

plotwhiteview4Poor08 <- 
  ggplot(whiteOpinonGenforPOOR08, aes(x = Q17_2, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "White Views of Redevelopment for The Poor 2008")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotwhiteview4Poor08

## Cowplot them
RD_Opn_byRace08 <- plot_grid(plotBlackview4Poor08, plotwhiteview4Poor08, 
                             nrow = 1)
RD_Opn_byRace08

##### Black views on gentrification for THE NBHD 2008 ##############################
blackOpinonGenforNBHD08 <- 
  blackres08 %>%
  count(Q17_3)

blackOpinonGenforNBHD08$Q17_3 <- as.numeric(blackOpinonGenforNBHD08$Q17_3)
blackOpinonGenforNBHD08$Q17_3[blackOpinonGenforNBHD08$Q17_3== 1] <- "Mostly Good"
blackOpinonGenforNBHD08$Q17_3[blackOpinonGenforNBHD08$Q17_3== 2] <- "Mostly Bad"
blackOpinonGenforNBHD08$Q17_3[blackOpinonGenforNBHD08$Q17_3== 8] <- "DK No Opinion"

plotBlackview4Nbhd08 <- 
  ggplot(blackOpinonGenforNBHD08, aes(x = Q17_3, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "Black Views of Redevelopment for The Neighborhood 2008")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview4Nbhd08

##### White views on gentrification for THE NBHD 2008 ##############################
WhtOpinonGenforRich08 <- 
  whiteres08 %>%
  count(Q17_3)

WhtOpinonGenforRich08$Q17_3 <- as.numeric(WhtOpinonGenforRich08$Q17_3)
WhtOpinonGenforRich08$Q17_3[WhtOpinonGenforRich08$Q17_3== 1] <- "Mostly Good"
WhtOpinonGenforRich08$Q17_3[WhtOpinonGenforRich08$Q17_3== 2] <- "Mostly Bad"
WhtOpinonGenforRich08$Q17_3[WhtOpinonGenforRich08$Q17_3== 8] <- "DK No Opinion"

plotWhtview4Nbhd08 <- 
  ggplot(WhtOpinonGenforRich08, aes(x = Q17_3, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "White Views of Redevelopment for The Neighborhood 2008")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotWhtview4Nbhd08

## Cowplot them
RD_Opn_byRace08 <- plot_grid(plotBlackview4Nbhd08, plotWhtview4Nbhd08, 
                             nrow = 1)
RD_Opn_byRace08

###### Black views on gentrification for THE CITY 2008 ##################
blackOpinonGenforCITY08 <- 
  blackres08 %>%
  count(Q17_4)

blackOpinonGenforCITY08$Q17_4 <- as.numeric(blackOpinonGenforCITY08$Q17_4)
blackOpinonGenforCITY08$Q17_4[blackOpinonGenforCITY08$Q17_4== 1] <- "Mostly Good"
blackOpinonGenforCITY08$Q17_4[blackOpinonGenforCITY08$Q17_4== 2] <- "Mostly Bad"
blackOpinonGenforCITY08$Q17_4[blackOpinonGenforCITY08$Q17_4== 8] <- "DK No Opinion"

plotBlackview4CITY08 <- 
  ggplot(blackOpinonGenforCITY08, aes(x = Q17_4, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "Black Views of Redevelopment for The CITY 2008")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview4CITY08

##### White views on gentrification for THE CITY 2008 ##############################
WhtOpinonGenforCITY08 <- 
  whiteres08 %>%
  count(Q17_4)

WhtOpinonGenforCITY08$Q17_4 <- as.numeric(WhtOpinonGenforCITY08$Q17_4)
WhtOpinonGenforCITY08$Q17_4[WhtOpinonGenforCITY08$Q17_4== 1] <- "Mostly Good"
WhtOpinonGenforCITY08$Q17_4[WhtOpinonGenforCITY08$Q17_4== 2] <- "Mostly Bad"
WhtOpinonGenforCITY08$Q17_4[WhtOpinonGenforCITY08$Q17_4== 8] <- "DK No Opinion"

plotWhtview4CITY08 <- 
  ggplot(WhtOpinonGenforCITY08, aes(x = Q17_4, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "White Views of Redevelopment for The City 2008")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotWhtview4CITY08

## Cowplot them
RD_Opn_byRace08 <- plot_grid(plotBlackview4CITY08, plotWhtview4CITY08, 
                             nrow = 1)
RD_Opn_byRace08
###### 2011 #########
DF11 = read_csv("/Users/aricaschuett/Documents/MiLO/Gentrification/31119633.csv")


# 24. As you may know, 'gentrification' is the process in which developers or higher
# income families buy and fix up homes or apartment buildings in working class city
# neighborhoods.  Some say (this type of redevelopment is good because it may draw new
# businesses to the area, increase home values and encourage higher income residents to
#  move into the neighborhood). Others say (this type of redevelopment is bad because it
# may cause rents and property taxes to increase, and force lower income residents to
# move out of the neighborhood.)  In general, would you say this kind of redevelopment
# is mainly a good thing or mainly a bad thing?
DF11 <- mutate( DF11, 
                DevMainlyGood = ifelse(DF11$qn24 == "Mainly good", 1, 0),
                DevMainlyBad = ifelse(DF11$qn24  == "Mainly bad", 1, 0),
                DK_NoOpnDev = ifelse(DF11$qn24  == "Don't know", 1, 0))

# 25. Is this kind of redevelopment happening in your own neighborhood, or not?
DF11 <- mutate( DF11, 
                DevInNbhd = ifelse(DF11$qn25 == "Yes", 1, 0),
                DevNotInNbhd = ifelse(DF11$qn25  == "No", 1, 0),
                DevNoOpn = ifelse(DF11$qn25  == "Don't know", 1, 0))

# 25. (IF YES) Thinking about your own personal housing situation, is the redevelopment
# in your neighborhood mainly a good thing for you or mainly a bad thing for you?
DF11 <- mutate( DF11, 
                DevGoodInNbhd = ifelse(DF11$qn26 == "Mainly good", 1, 0),
                DevBadInNbhd = ifelse(DF11$qn26  == "Mainly bad", 1, 0),
                DevNoOpnInNbhd = ifelse(DF11$qn26  == "Don't know", 1, 0))

# Income
DF11 <- mutate( DF11, 
                IncomeGtr200k = ifelse(DF11$xnd15c == "$200k or more", 1, 0),
                Btwn150kand200k = ifelse(DF11$xnd15c  == "$150k to under $200k", 1, 0),
                Income100kPlus = ifelse(DF11$xnd15c  == "$100k+ Undetermined", 1, 0),
                Btwn100kand150k = ifelse(DF11$xnd15c == "$100k to under $150k", 1, 0),
                Btwn65kand100k = ifelse(DF11$xnd15c  == "$65k but less than $100k", 1, 0),
                Btwn50kand65k = ifelse(DF11$xnd15c  == "$50k but less than $65k", 1, 0),
                Btwn35kand50k = ifelse(DF11$xnd15c == "$35k but less than $50k", 1, 0),
                Btwn20kand35k = ifelse(DF11$xnd15c == "$20k but less than $35k", 1, 0),
                LessThan20k = ifelse(DF11$xnd15c  == "Less than $20k", 1, 0),
                IncomeRefused = ifelse(DF11$xnd15c == "Refused", 1, 0))
# Race
DF11 <- mutate( DF11, 
                White = ifelse(DF11$qnd11a== "White", 1, 0),
                Black = ifelse(DF11$qnd11a  == "Black", 1, 0),
                BlackHisp = ifelse(DF11$qnd11a  == "Black Hispanic ", 1, 0),
                WhiteHisp = ifelse(DF11$qnd11a == "White Hispanic", 1, 0),
                Asian = ifelse(DF11$qnd11a== "Asian", 1, 0),
                HispanicNRG = ifelse(DF11$qnd11a  == "Don't know Hispanic", 1, 0),
                NoRaceGiven = ifelse(DF11$qnd11a  == "(no race given)", 1, 0),
                Refused = ifelse(DF11$qnd11a == "Refused", 1, 0),
                OtherRace = ifelse(DF11$qnd11a == "Other Race", 1, 0))
# Ward
DF11 <- mutate( DF11, 
                Ward1 = ifelse(DF11$qnd3 == "Ward 1", 1, 0),
                Ward2 = ifelse(DF11$qnd3  == "Ward 2", 1, 0),
                Ward3 = ifelse(DF11$qnd3  == "Ward 3", 1, 0),
                Ward4 = ifelse(DF11$qnd3 == "Ward 4", 1, 0),
                Ward5 = ifelse(DF11$qnd3== "Ward 5", 1, 0),
                Ward6 = ifelse(DF11$qnd3  == "Ward 6", 1, 0),
                Ward7 = ifelse(DF11$qnd3  == "Ward 7", 1, 0),
                Ward8 = ifelse(DF11$qnd3  == "Ward 8", 1, 0),
                DkWard = ifelse(DF11$qnd3  == "Don't know", 1, 0),
                RefusedWard = ifelse(DF11$qnd3  == "Refused", 1, 0))

# Rent or Own?
DF11 <- mutate( DF11, 
                Rent = ifelse(DF11$qn21 == "Rent", 1, 0),
                Own = ifelse(DF11$qn21  == "Own", 1, 0),
                HousingOther= ifelse(DF11$qn21  == "Other/something else", 1, 0),
                HousingRefused = ifelse(DF11$qn21  == "Refused", 1, 0))

# Financial Situation
DF11 <- mutate( DF11, 
                FinSecureVHigh = ifelse(DF11$qn21 == "Very secure", 1, 0),
                FinSecureHigh = ifelse(DF11$qn21  == "Very insecure", 1, 0),
                FinSecureMod= ifelse(DF11$qn21  == "Somewhat secure", 1, 0),
                FinSecureLow = ifelse(DF11$qn21  == "Somewhat insecure", 1, 0),
                FinSecureDK= ifelse(DF11$qn21  == "Don't know", 1, 0),
                FinSecureRefused= ifelse(DF11$qn21  == "Refused", 1, 0))

# Find a house in DC
DF11 <- mutate( DF11, 
                CouldFindinDC = ifelse(DF11$qn21 == "Could find something in the District", 1, 0),
                CouldNotFindinDC = ifelse(DF11$qn21  == "Have to move to the suburbs", 1, 0),
                NoOpnFindinDC= ifelse(DF11$qn21  == "Don't know", 1, 0))


###### 2014 ###############################################################################################################################

load("/Users/aricaschuett/Documents/MiLO/Gentrification/2014-01-14_DC_ROPER.RData")

DF14 <- `2014-01-14_DC_ROPER`

DF14[DF14 == 98] <- NA
#Rename some columns
#colnames(DF14)[colnames(DF14) == "q905"] <- "RegVoter"
colnames(DF14)[colnames(DF14) == "q1"] <- "Party"
colnames(DF14)[colnames(DF14) == "racenet"] <- "Race"
#colnames(DF14)[colnames(DF14) == "wardsnew"] <- "Ward"
colnames(DF14)[colnames(DF14) == "q2"] <- "Party_NotReg"
colnames(DF14)[colnames(DF14) == "q3"] <- "Interest_DemorPTR"
colnames(DF14)[colnames(DF14) == "q2q3net"] <- "Interest_DemorPTRPty"
colnames(DF14)[colnames(DF14) == "q4"] <- "PTV_DemorPTR"
colnames(DF14)[colnames(DF14) == "q4net"] <- "PTV" # I think this covers all party voters
colnames(DF14)[colnames(DF14) == "q5"] <- "CandLean_DemPTR"



#Create Binary Variables for survey responses 
# q905
DF14 <- mutate(DF14, 
             RegVoter = ifelse(DF14$q905 == 1, 1, 0), 
             UnRegVoter = ifelse(DF14$q905 == 2, 1, 0))

# height zoning restrictions
DF14 <- mutate(DF14, 
             ProHtLm = ifelse(DF14$q30 == 1, 1, 0), 
             AntiHtLm = ifelse(DF14$q30 == 2, 1, 0), 
             NoOpinHtLm = ifelse(DF14$q30 == 8, 1, 0))

# Gentrification' is the process in which developers or higher income families buy and fix up homes or
# apartment buildings in working class city neighborhoods. Would you say this is mainly a good thing or mainly a bad thing? 
DF14 <- mutate(DF14, 
             ProGen = ifelse(DF14$q31 == 1, 1, 0), 
             AntiGen = ifelse(DF14$q31 == 2, 1, 0), 
             NeitherGen = ifelse(DF14$q31 == 3, 1, 0),
             BothGen = ifelse(DF14$q31 == 4, 1, 0),
             NoOpinGen = ifelse(DF14$q31 == 8 , 1, 0))
#q31 <- DF14$q31

# redevelopment & good/bad for people like you
DF14 <- mutate(DF14, 
             DevGood4Me = ifelse(DF14$q32a == 1, 1, 0), 
             DevBad4Me = ifelse(DF14$q32a == 2, 1, 0), 
             NoDevOpn4Me = ifelse(DF14$q32a == 8, 1, 0))

# redevelopment good/bad for long-time residents
DF14 <- mutate(DF14, 
             DevGood4LngTm = ifelse(DF14$q32b == 1, 1, 0), 
             DevBad4LngTm = ifelse(DF14$q32b == 2, 1, 0), 
             DevNoOpn4LngTm = ifelse(DF14$q32b == 8, 1, 0))

# redevelopment good/bad for New residents
DF14 <- mutate(DF14, 
             DevGood4New = ifelse(DF14$q32c == 1, 1, 0), 
             DevBad4New = ifelse(DF14$q32c == 2, 1, 0), 
             DevNoOpn4New = ifelse(DF14$q32c == 8, 1, 0))

# redevelopment good/bad for White residents
DF14 <- mutate(DF14, 
             DevGood4Wht = ifelse(DF14$q32d == 1, 1, 0), 
             DevBad4Wht = ifelse(DF14$q32d == 2, 1, 0), 
             DevNoOpn4Wht = ifelse(DF14$q32d == 8, 1, 0))

# redevelopment good/bad for Black residents                                      ### with 06
DF14 <- mutate(DF14, 
             DevGood4Blk = ifelse(DF14$q32e == 1, 1, 0), 
             DevBad4Blk = ifelse(DF14$q32e == 2, 1, 0), 
             DevNoOpn4Blk = ifelse(DF14$q32e == 8, 1, 0))

# redevelopment good/bad for rich residents
DF14 <- mutate(DF14, 
             DevGood4Rich = ifelse(DF14$q32f == 1, 1, 0), 
             DevBad4Rich = ifelse(DF14$q32f == 2, 1, 0), 
             DevNoOpn4Rich = ifelse(DF14$q32f == 8, 1, 0))

# redevelopment good/bad for poor residents
DF14 <- mutate(DF14, 
             DevGood4Poor = ifelse(DF14$q32g == 1, 1, 0), 
             DevBad4Poor = ifelse(DF14$q32g == 2, 1, 0), 
             DevNoOpn4Poor = ifelse(DF14$q32g == 8, 1, 0))

# District mainly unified or divided
DF14 <- mutate(DF14, 
             Unified = ifelse(DF14$q33 == 1, 1, 0), 
             Divided = ifelse(DF14$q33 == 2, 1, 0), 
             NoOpnDiv = ifelse(DF14$q33 == 8, 1, 0))

# Divided Race/Income
DF14 <- mutate(DF14, 
             DivRace = ifelse(DF14$q33a == 1, 1, 0), 
             DivIncome = ifelse(DF14$q33a == 2, 1, 0), 
             DivBoth = ifelse(DF14$q33a == 3, 1, 0),
             DivNeither = ifelse(DF14$q33a == 4, 1, 0), 
             DivDKNoOp = ifelse(DF14$q33a ==8, 1, 0))

# Legalizing marijuana
DF14 <- mutate(DF14, 
             ProMarij = ifelse(DF14$q38 == 1, 1, 0), 
             AntiMarij = ifelse(DF14$q38 == 2, 1, 0), 
             NoOpnMarij = ifelse(DF14$q38 == 8, 1, 0))

# Reduce Punishment marijuana
DF14 <- mutate(DF14, 
             MorePunitMarij = ifelse(DF14$q39 == 1, 1, 0), 
             LessPunitMarij = ifelse(DF14$q39 == 2, 1, 0), 
             NoOpnPunitMarij = ifelse(DF14$q39 == 8, 1, 0))

# Quadrant of DC
DF14 <- mutate(DF14, 
             Northwest = ifelse(DF14$quad == 1, 1, 0), 
             Northeast = ifelse(DF14$quad == 2, 1, 0), 
             Southwest = ifelse(DF14$quad == 3, 1, 0),
             Southeast = ifelse(DF14$quad == 4, 1, 0))

# West or East of Anacostia River
DF14 <- mutate(DF14, 
             WestofRiv = ifelse(DF14$q33 == 1, 1, 0), 
             EastofRiv = ifelse(DF14$q33 == 2, 1, 0))

# Ideological orientation
DF14 <- mutate(DF14, 
             LiberalPID = ifelse(DF14$q33 == 1, 1, 0), 
             ModeratePID = ifelse(DF14$q33 == 2, 1, 0), 
             ConservativePID = ifelse(DF14$q33 == 3, 1, 0),
             NoApplicablePID = ifelse(DF14$q33 == 4, 1, 0),
             NoOpinPID = ifelse(DF14$q33 == 5, 1, 0))

# Last HS Grade
DF14 <- mutate(DF14, 
             EigthGrade = ifelse(DF14$q909 == 1, 1, 0), 
             SomeHS = ifelse(DF14$q909 == 2, 1, 0), 
             GradHS = ifelse(DF14$q909 == 3, 1, 0),
             SomeCollege = ifelse(DF14$q909 == 4, 1, 0),
             GradCollege = ifelse(DF14$q909 == 5, 1, 0),
             PostGrad = ifelse(DF14$q909 == 6, 1, 0), 
             NoOpinEdu = ifelse(DF14$q909 == 8, 1, 0))

# Vote in last Dem primary
DF14 <- mutate(DF14, 
             VotedPrimary = ifelse(DF14$q41 == 1, 1, 0), 
             DidntVotePrimary = ifelse(DF14$q41 == 2, 1, 0), 
             NoOpnPrimary = ifelse(DF14$q41 == 8, 1, 0))

# Years in DC
DF14 <- mutate(DF14, 
               YrsinDC0to5 = ifelse(DF14$q21 == 1, 1, 0), 
               YrsinDC6to19  = ifelse(DF14$q21 == 2, 1, 0), 
               YrsinDC20to39  = ifelse(DF14$q21 == 3, 1, 0),
               YrsinDC40Plus = ifelse(DF14$q21 == 4, 1, 0), 
               YrsinDCWholeLife = ifelse(DF14$q21 == 5, 1, 0),
               YrsinDCDK = ifelse(DF14$q21 == 6, 1, 0))
# wardsnew
DF14 <- mutate(DF14, 
               Ward1 = ifelse(DF14$wardnew == 1, 1, 0), 
               Ward2  = ifelse(DF14$wardnew == 2, 1, 0), 
               Ward3  = ifelse(DF14$wardnew == 3, 1, 0),
               Ward4 = ifelse(DF14$wardnew == 4, 1, 0), 
               Ward5 = ifelse(DF14$wardnew == 5, 1, 0),
               Ward6 = ifelse(DF14$wardnew == 6, 1, 0),
               Ward7 = ifelse(DF14$wardnew == 7, 1, 0),
               Ward8 = ifelse(DF14$wardnew == 8, 1, 0))

# racenet
DF14 <- mutate(DF14, 
               White = ifelse(DF14$Race == 1, 1, 0), 
               Black  = ifelse(DF14$Race == 2, 1, 0), 
               Hispanic  = ifelse(DF14$Race == 3, 1, 0),
               Asian = ifelse(DF14$Race == 6, 1, 0), 
               RaceOther = ifelse(DF14$Race == 7, 1, 0),
               RaceDKNoOpn = ifelse(DF14$Race == 8, 1, 0))

###### 2015 #####################################################################################################################################################

DF15 <-read_csv("/Users/aricaschuett/Documents/MiLO/Gentrification/31115652.csv")

DF15Test <-read_dta("/Users/aricaschuett/Documents/MiLO/Gentrification/31115652.DTA")

# 9. If you had to move today, do you think you could (buy a home/ find a rental)
# in the District that you would like and could afford, or do you think you would
# have to move to the suburbs to find something you would like and could afford?
DF15 <- mutate( DF15, 
                CouldFindinDC = ifelse(DF15$q9 == "Could find something in District", 1, 0),
                CouldFindinSub = ifelse(DF15$q9 == "Would have to move to suburbs", 1, 0),
                DK_NoOpn = ifelse(DF15$q9 == "(VOL) DK/No Opinion", 1, 0),
                NAp_Ref = ifelse(DF15$q9 == "(VOL) NA/Refused", 1, 0))

# 11. As you may know, the District government is trying to redevelop parts of
# the city to attract new businesses and residents. Do you think this process of
# redevelopment is mainly good or mainly bad for people like you? 
DF15 <- mutate( DF15, 
                ReDevGood = ifelse(DF15$q11 == "Mainly good", 1, 0),
                ReDevBad = ifelse(DF15$q11 == "Mainly bad", 1, 0),
                ReDevNoOp = ifelse(DF15$q11 == "(VOL) DK/No Opinion", 1, 0),
                ReDevNA = ifelse(DF15$q11 == "(VOL) NA/Refused", 1, 0))                                                                                  

# 12. Would you support or oppose District government spending one hundred
# million dollars a year, about one percent of the city budget, to provide
# affordable housing units for low-income residents?
DF15 <- mutate( DF15, 
                SupAffHous = ifelse(DF15$q12 == "Support", 1, 0),
                OppAffHous = ifelse(DF15$q12 == "Oppose", 1, 0),
                AffHousNoOp = ifelse(DF15$q12 == "(VOL) DK/No Opinion", 1, 0),
                AffHousvNA = ifelse(DF15$q12 == "(VOL) NA/Refused", 1, 0))  

# 5. Generally speaking, how would you rate the job (ITEM): excellent, good, not
# so good or poor?
#   How about the job (NEXT ITEM)? IF NEEDED: Generally speaking, how would you
# rate the job (ITEM): excellent, good, not so good or poor?
#   ***RANDOMIZE ITEMS***
#   a. that District police are doing
# b. the District is doing to create and maintain affordable housing
# c. the District is doing helping the homeless 


# Police Attitudes
DF15 <- mutate( DF15, 
                StApprovePolice = ifelse(DF15$q5a == "Excellent", 1, 0),
                ApprovePolice = ifelse(DF15$q5a == "Good", 1, 0),
                NSGPolice = ifelse(DF15$q5a == "NotSoGood", 1, 0),
                PoorPolice = ifelse(DF15$q5a == "Poor", 1, 0),
                DKPolice = ifelse(DF15$q5a == "(VOL) DK/No Opinion", 1, 0))  


# Affordable Housing Actions
DF15 <- mutate( DF15, 
                StApproveAffHousing = ifelse(DF15$q5b == "Excellent", 1, 0),
                ApproveAffHousing = ifelse(DF15$q5b == "Good", 1, 0),
                NSGAffHousing = ifelse(DF15$q5b == "NotSoGood", 1, 0),
                PoorAffHousing = ifelse(DF15$q5b == "Poor", 1, 0),
                DKAffHousing = ifelse(DF15$q5b == "(VOL) DK/No Opinion", 1, 0))

# Remedy Homelessness
DF15 <- mutate( DF15, 
                StApproveHomelessPolicy = ifelse(DF15$q5c == "Excellent", 1, 0),
                ApproveHomelessPolicy = ifelse(DF15$q5c == "Good", 1, 0),
                NSGHomelessPolicy = ifelse(DF15$q5c == "NotSoGood", 1, 0),
                PoorHomelessPolicy = ifelse(DF15$q5c == "Poor", 1, 0),
                DKHomelessPolicy = ifelse(DF15$q5c == "(VOL) DK/No Opinion", 1, 0))

colnames(DF15)[colnames(DF15) == "racenet"] <- "Race"

# race
DF15 <- mutate( DF15, 
                White = ifelse(DF15$q5c == "white", 1, 0),
                Black = ifelse(DF15$q5c == "black", 1, 0),
                Hisp = ifelse(DF15$q5c == "Hispanic", 1, 0),
                Asian = ifelse(DF15$q5c == "Asian", 1, 0),
                OtherRace = ifelse(DF15$q5c == "Other", 1, 0),
                DkNoOpnRace = ifelse(DF15$q5c == "DK/No opinion ", 1, 0))


blackres15 <- filter(DF15, Race == "black")

colnames(DF14)[colnames(DF14) == "q910"] <- "Age"

# ward DNE 2015

# PID
DF15 <- mutate( DF15, 
                PIDDem = ifelse(DF15$q5c == "A Democrat", 1, 0),
                PIDRep = ifelse(DF15$q5c == "A Republican", 1, 0),
                PIDInd = ifelse(DF15$q5c == "An Independent", 1, 0),
                PIDOther = ifelse(DF15$q5c == "Other (SPECIFY)", 1, 0),
                PIDNA = ifelse(DF15$q5c == "(VOL) NA/Refused", 1, 0),
                PIDKNoOpin = ifelse(DF15$q5c == "(VOL) DK/No Opinion", 1, 0))

# Do you rent/own
DF15 <- mutate( DF15, 
                Rent = ifelse(DF15$q8 == "Renting", 1, 0),
                Own = ifelse(DF15$q8 == "Owning", 1, 0),
                HousingOther = ifelse(DF15$q8 == "Other/something else", 1, 0),
                HousingRefused = ifelse(DF15$q8 == "(VOL) NA/Refused", 1, 0))

# years in DC 
DF15 <- mutate( DF15, 
                InDCLessThan5Yrs = ifelse(DF15$q8 == "0-5 yrs", 1, 0),
                InDC6to19Yrs = ifelse(DF15$q8 == "6-19 yrs", 1, 0),
                InDC20to39Yrs = ifelse(DF15$q8 == "20-39 yrs", 1, 0),
                InDC40PlusYrs = ifelse(DF15$q8 == "40+ yrs", 1, 0),
                InDCNoOpin = ifelse(DF15$q8 == "No opinion", 1, 0),
                InDCWholeLifes = ifelse(DF15$q8 == "Whole life", 1, 0))

# Fall Behind/ Getting Ahead
DF15 <- mutate( DF15, 
                FinFallingBehind = ifelse(DF15$q8 == "Falling behind financially", 1, 0),
                FinGettingAhead = ifelse(DF15$q8 == "Getting ahead financially", 1, 0),
                FinMaintain = ifelse(DF15$q8 == "Have just enough money to maintain standard of living", 1, 0),
                FinNARefused = ifelse(DF15$q8 == "(VOL) NA/Refused", 1, 0),
                FinDKNoOpin = ifelse(DF15$q8 == "(VOL) DK/No Opinion", 1, 0))


# Gender
DF15 <- mutate( DF15, 
                Female = ifelse(DF15$q8 == "Female", 1, 0),
                Male = ifelse(DF15$q8 == "Male", 1, 0))


# Stay in DC or get pushed out
DF15 <- mutate( DF15, 
                FindInDC = ifelse(DF15$q8 == "Could find something in District", 1, 0),
                HaveToLeaveDC = ifelse(DF15$q8 == "Would have to move to suburbs", 1, 0),
                DKLeaveStayDC = ifelse(DF15$q8 == "(VOL) DK/No Opinion", 1, 0),
                NoOpnLeaveStayDC = ifelse(DF15$q8 == "(VOL) NA/Refused", 1, 0))

# Income
DF15 <- mutate( DF15, 
                IncomeUnder20k = ifelse(DF15$income == "Under 20 thousand dollars", 1, 0),
                Income20kto35k = ifelse(DF15$income == "20 to under 35 thousand", 1, 0),
                Income35kto50k = ifelse(DF15$income == "35 to under 50 thousand", 1, 0),
                Income50kto65k = ifelse(DF15$income == "50 to under 65 thousand", 1, 0),
                Income65kto100k = ifelse(DF15$income == "65 to under 100 thousand", 1, 0),
                IncomeOver100k = ifelse(DF15$income == "100 thousand or more", 1, 0),
                IncomeNA = ifelse(DF15$income == "(VOL) NA/Refused", 1, 0))

################################################################3
blackOpinonRD <- 
  blackres15 %>%
  count(q11)


plotBlackview <- 
  ggplot(blackOpinonRD, aes(x = q11, y = n/sum(n)*100 )) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = "Black Residents Views of Redevelopment 2015")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview


###### Census Data added to 2014 ##########################################################################################################################################################
# Manually enter census data
# Total population percentage change 2000-2010 (Abridged Population Column J)
DF14 <- mutate(DF14,
             PctChg2000_2010 = ifelse(wardnew == 1, 3.8,
                                      ifelse(wardnew == 2, 21,
                                             ifelse(wardnew == 3, 4.7,
                                                    ifelse(wardnew == 4, 1,
                                                           ifelse(wardnew == 5, 3.8,
                                                                  ifelse(wardnew == 6, 7.2,
                                                                         ifelse(wardnew == 7, 2.5,
                                                                                ifelse(wardnew == 8, -0.5, NA))))))))
)





# white Pop change (Abridged Population Column D)
DF14 <- mutate(DF14,
             wpopchg = ifelse(wardnew == 1, 0.546639286,
                              ifelse(wardnew == 2, 0.27731617,
                                     ifelse(wardnew == 3, 0.051406524,
                                            ifelse(wardnew == 4, 0.342808377,
                                                   ifelse(wardnew == 5, 1.115300031,
                                                          ifelse(wardnew == 6, 0.773803662,
                                                                 ifelse(wardnew == 7, 0.515004055,
                                                                        ifelse(wardnew ==8, -0.213158534, NA))))))))
)


# Black Population Change (Abridged Population Column G)
DF14 <- mutate(DF14,
             bpopchg = ifelse(wardnew == 1, -0.255521945,
                              ifelse(wardnew == 2, -0.235597353,
                                     ifelse(wardnew == 3, -0.042821159,
                                            ifelse(wardnew == 4, -0.145768112,
                                                   ifelse(wardnew == 5, -0.088909848,
                                                          ifelse(wardnew == 6, -0.243309002,
                                                                 ifelse(wardnew == 7, -0.005859573,
                                                                        ifelse(wardnew ==8, 0.014595893, NA))))))))
)



# PctPoorPersons_2010_14 (Abridged wellbeing Column B)
DF14 <- mutate(DF14,
             PctPoorPersons_2010_14 = ifelse(wardnew == 1, 13,
                                             ifelse(wardnew == 2, 13,
                                                    ifelse(wardnew == 3, 9.8,
                                                           ifelse(wardnew == 4, 13,
                                                                  ifelse(wardnew == 5, 20,
                                                                         ifelse(wardnew == 6, 14,
                                                                                ifelse(wardnew == 7, 26,
                                                                                       ifelse(wardnew == 8, 37, NA))))))))
)


# PctUnemployed_2010_14 (Abridged wellbeing Column C)
DF14 <- mutate(DF14,
             PctUnemployed2010_14 = ifelse(wardnew == 1, 7.1,
                                           ifelse(wardnew == 2, 4.2,
                                                  ifelse(wardnew == 3, 3.9,
                                                         ifelse(wardnew == 4, 11,
                                                                ifelse(wardnew == 5, 16,
                                                                       ifelse(wardnew == 6, 7.2,
                                                                              ifelse(wardnew == 7, 20,
                                                                                     ifelse(wardnew == 8, 25, NA))))))))
)


# PctOwnerOccupiedHsgUnits_2010_14 (Abridged wellbeing Column C)
DF14 <- mutate(DF14,
             PctOwnerOcc2010_14 = ifelse(wardnew == 1, 34,
                                         ifelse(wardnew == 2, 36,
                                                ifelse(wardnew == 3, 52,
                                                       ifelse(wardnew == 4, 59,
                                                              ifelse(wardnew == 5, 47,
                                                                     ifelse(wardnew == 6, 43,
                                                                            ifelse(wardnew == 7, 39,
                                                                                   ifelse(wardnew == 8, 21, NA))))))))
)



# grayvote10 (Abridged Politics Column B)
DF14 <- mutate(DF14,
             grayvote10 = ifelse(wardnew == 1, 65.32,
                                 ifelse(wardnew == 2, 56.49,
                                        ifelse(wardnew == 3, 53.02,
                                               ifelse(wardnew == 4, 78.87,
                                                      ifelse(wardnew == 5, 88.35,
                                                             ifelse(wardnew == 6, 63.62,
                                                                    ifelse(wardnew == 7, 94.27,
                                                                           ifelse(wardnew == 8, 94.60, NA))))))))
)

#  %turnout10 (Abridged Politics Column E)
DF14 <- mutate(DF14,
             pctturnout10 = ifelse(wardnew == 1, 27.47,
                                   ifelse(wardnew == 2, 25.89,
                                          ifelse(wardnew == 3, 34.24,
                                                 ifelse(wardnew == 4, 33.63,
                                                        ifelse(wardnew == 5, 30.98,
                                                               ifelse(wardnew == 6, 32.80,
                                                                      ifelse(wardnew == 7, 30.05,
                                                                             ifelse(wardnew == 8, 22.96, NA))))))))
)


#  311_calls per 1000(Abridged Politics Column E)
DF14 <- mutate(DF14,
             calls_311 = ifelse(wardnew == 1, 29.57213075,
                                ifelse(wardnew == 2, 28.58889481,
                                       ifelse(wardnew == 3, 10.673495,
                                              ifelse(wardnew == 4, 15.24289655,
                                                     ifelse(wardnew == 5, 18.58480917,
                                                            ifelse(wardnew == 6, 24.22368421,
                                                                   ifelse(wardnew == 7, 16.72520488,
                                                                          ifelse(wardnew == 8, 11.37628628, NA))))))))
)



#  Rent Burden(Rent Burden column B)
DF14 <- mutate(DF14,
             rent_burden = ifelse(wardnew == 1, 40,
                                  ifelse(wardnew == 2, 41,
                                         ifelse(wardnew == 3, 46,
                                                ifelse(wardnew == 4, 51,
                                                       ifelse(wardnew == 5, 54,
                                                              ifelse(wardnew == 6, 40,
                                                                     ifelse(wardnew == 7, 50,
                                                                            ifelse(wardnew == 8, 55, NA))))))))
)


#  Turnout86(Rent Burden column J)
DF14 <- mutate(DF14,
             turnout82 = ifelse(wardnew == 1, 12.703,
                                ifelse(wardnew == 2, 13.224,
                                       ifelse(wardnew == 3, 18.939,
                                              ifelse(wardnew == 4, 18.5834,
                                                     ifelse(wardnew == 5, 17.0729,
                                                            ifelse(wardnew == 6, 13.4236,
                                                                   ifelse(wardnew == 7, 14.81257,
                                                                          ifelse(wardnew == 8, 7.841603, NA))))))))
)


#  Turnout86(Turnout 1974-2014 column M)
DF14 <- mutate(DF14,
             turnout86 = ifelse(wardnew == 1, 42.43268619,
                                ifelse(wardnew == 2, 42.77904328,
                                       ifelse(wardnew == 3, 55.67855579,
                                              ifelse(wardnew == 4, 52.06980126,
                                                     ifelse(wardnew == 5, 47.51915266,
                                                            ifelse(wardnew == 6, 46.09655656,
                                                                   ifelse(wardnew == 7, 46.66979637,
                                                                          ifelse(wardnew == 8, 33.70932403, NA))))))))
)


#  Pop Change 1980-1990
DF14 <- mutate(DF14,
             pctChange1980_90 = ifelse(wardnew == 1, 1.3,
                                       ifelse(wardnew == 2, 3.5,
                                              ifelse(wardnew == 3, 7.9,
                                                     ifelse(wardnew == 4, -6.3,
                                                            ifelse(wardnew == 5, -6.7,
                                                                   ifelse(wardnew == 6, -6.2,
                                                                          ifelse(wardnew == 7, -15,
                                                                                 ifelse(wardnew == 8, -11, NA))))))))
)


#  Pop Change 1990-2000
DF14 <- mutate(DF14,
             pctChange1990_00 = ifelse(wardnew == 1, 1,
                                       ifelse(wardnew == 2, 4.9,
                                              ifelse(wardnew == 3, 1.5,
                                                     ifelse(wardnew == 4, -3.9,
                                                            ifelse(wardnew == 5, -14, # check this 
                                                                   ifelse(wardnew == 6, -6.10,
                                                                          ifelse(wardnew == 7, -11,
                                                                                 ifelse(wardnew == 8, -15, NA))))))))
)


#  Pop Change 2000-2010
DF14 <- mutate(DF14,
             pctChange2000_10 = ifelse(wardnew == 1, 3.9,
                                       ifelse(wardnew == 2, 16,
                                              ifelse(wardnew == 3, 4.6,
                                                     ifelse(wardnew == 4, 1,
                                                            ifelse(wardnew == 5, 3.8,
                                                                   ifelse(wardnew == 6, 13,
                                                                          ifelse(wardnew == 7, 0.7,
                                                                                 ifelse(wardnew == 8, -0.3, NA))))))))
)


#  Pop Change 1980-2010
DF14 <- mutate(DF14,
             pctChange1980_10= ifelse(wardnew == 1, 6.3,
                                      ifelse(wardnew == 2, 26.06679,
                                             ifelse(wardnew == 3, 5.61221,
                                                    ifelse(wardnew == 4, -8.96716,
                                                           ifelse(wardnew == 5, -16.64367,
                                                                  ifelse(wardnew == 6, -0.923531,
                                                                         ifelse(wardnew == 7, -23.45193,
                                                                                ifelse(wardnew == 8, -24.257161, NA))))))))
)


#  2011-2015 Demographics % Black
DF14 <- mutate(DF14,
             pctBlack2011_15 = ifelse(wardnew == 1, 30.3,
                                      ifelse(wardnew == 2, 8.08,
                                             ifelse(wardnew == 3, 6.90,
                                                    ifelse(wardnew == 4, 56.40,
                                                           ifelse(wardnew == 5, 70.40,
                                                                  ifelse(wardnew == 6, 35.50,
                                                                         ifelse(wardnew == 7, 94.20,
                                                                                ifelse(wardnew == 8, 92.80, NA))))))))
)


#  2011-2015 % Vacant Housing
DF14 <- mutate(DF14,
             pctVacant2011_15= ifelse(wardnew == 1, 7.3,
                                      ifelse(wardnew == 2, 11.00,
                                             ifelse(wardnew == 3, 7.20,
                                                    ifelse(wardnew == 4, 7.20,
                                                           ifelse(wardnew == 5, 11.50,
                                                                  ifelse(wardnew == 6, 8.10,
                                                                         ifelse(wardnew == 7, 13.20,
                                                                                ifelse(wardnew == 8, 14.20, NA))))))))
)



#  2011-2015 % Renter Occupied
DF14 <- mutate(DF14,
             pctRenterOccupied = ifelse(wardnew == 1, 65.90,
                                        ifelse(wardnew == 2, 64.90,
                                               ifelse(wardnew == 3, 48.40,
                                                      ifelse(wardnew == 4, 40.20,
                                                             ifelse(wardnew == 5, 52.80,
                                                                    ifelse(wardnew == 6, 57.80,
                                                                           ifelse(wardnew == 7, 62.00,
                                                                                  ifelse(wardnew == 8, 79.50, NA))))))))
)


#  2011-2015 % Owner Occupied
DF14 <- mutate(DF14,
             pctOwnerOccupied = ifelse(wardnew == 1, 34.10,
                                       ifelse(wardnew == 2, 35.10,
                                              ifelse(wardnew == 3, 51.60,
                                                     ifelse(wardnew == 4, 59.80,
                                                            ifelse(wardnew == 5, 47.20,
                                                                   ifelse(wardnew == 6, 42.20,
                                                                          ifelse(wardnew == 7, 38.00,
                                                                                 ifelse(wardnew == 8, 20.50, NA))))))))
)

#  2011-2015 % Moved 2010-2014
DF14 <- mutate(DF14,
             Moved2010_14 = ifelse(wardnew == 1, 48.30,
                                   ifelse(wardnew == 2, 49.70,
                                          ifelse(wardnew == 3, 38.40,
                                                 ifelse(wardnew == 4, 27.00,
                                                        ifelse(wardnew == 5, 34.90,
                                                               ifelse(wardnew == 6, 42.30,
                                                                      ifelse(wardnew == 7, 30.90,
                                                                             ifelse(wardnew == 8, 41.20, NA))))))))
)


#  2011-2015 % Moved 2000-2009
DF14 <- mutate(DF14,
             Moved2000_09 = ifelse(wardnew == 1, 33.50,
                                   ifelse(wardnew == 2, 32.30,
                                          ifelse(wardnew == 3, 31.00,
                                                 ifelse(wardnew == 4, 32.60,
                                                        ifelse(wardnew == 5, 31.90,
                                                               ifelse(wardnew == 6, 33.30,
                                                                      ifelse(wardnew == 7, 35.90,
                                                                             ifelse(wardnew == 8, 35.70, NA))))))))
)


#  2011-2015 % Moved Before 1999
DF14 <- mutate(DF14,
             Movedpre2000 = ifelse(wardnew == 1, 16.57,
                                   ifelse(wardnew == 2, 15.02,
                                          ifelse(wardnew == 3, 28.83,
                                                 ifelse(wardnew == 4, 38.97,
                                                        ifelse(wardnew == 5, 31.78,
                                                               ifelse(wardnew == 6, 21.88,
                                                                      ifelse(wardnew == 7, 32.42,
                                                                             ifelse(wardnew == 8, 21.23, NA))))))))
)



#  2011-2015 % Unemployed 2011-2015
DF14 <- mutate(DF14,
             Unemployed2011_15 = ifelse(wardnew == 1, 5.1,
                                        ifelse(wardnew == 2, 2.7,
                                               ifelse(wardnew == 3, 2.6,
                                                      ifelse(wardnew == 4, 6.9,
                                                             ifelse(wardnew == 5, 9.00,
                                                                    ifelse(wardnew == 6, 4.70,
                                                                           ifelse(wardnew == 7, 11.00,
                                                                                  ifelse(wardnew == 8, 12.70, NA))))))))
)


#  2011-2015 % Cash Assistance 
DF14 <- mutate(DF14,
             CashAssis2011_15 = ifelse(wardnew == 1, 2.10,
                                       ifelse(wardnew == 2, 0.5,
                                              ifelse(wardnew == 3, 0.8,
                                                     ifelse(wardnew == 4, 2.10,
                                                            ifelse(wardnew == 5, 4.30,
                                                                   ifelse(wardnew == 6, 2.80,
                                                                          ifelse(wardnew == 7, 7.50,
                                                                                 ifelse(wardnew == 8, 12.40, NA))))))))
)


#  2011-2015 % SNAP
DF14 <- mutate(DF14,
             SNAP2011_15 = ifelse(wardnew == 1, 10.70,
                                  ifelse(wardnew == 2, 2.60,
                                         ifelse(wardnew == 3, 1.20,
                                                ifelse(wardnew == 4, 11.60,
                                                       ifelse(wardnew == 5, 19.50,
                                                              ifelse(wardnew == 6, 10.10,
                                                                     ifelse(wardnew == 7, 32.20,
                                                                            ifelse(wardnew == 8, 41.60, NA))))))))
)


#  2011-2015 % Management
DF14 <- mutate(DF14,
             Mngmt2011_15 = ifelse(wardnew == 1, 65.10,
                                   ifelse(wardnew == 2, 75.00,
                                          ifelse(wardnew == 3, 77.50,
                                                 ifelse(wardnew == 4, 51.80,
                                                        ifelse(wardnew == 5, 50.00,
                                                               ifelse(wardnew == 6, 72.40,
                                                                      ifelse(wardnew == 7, 32.50,
                                                                             ifelse(wardnew == 8, 29.70, NA))))))))
)

#  2011-2015 Median HH Income
DF14 <- mutate(DF14,
             MedianHHIncome2011_15 = ifelse(wardnew == 1, 82195,
                                            ifelse(wardnew == 2, 100388,
                                                   ifelse(wardnew == 3, 112873,
                                                          ifelse(wardnew == 4, 74600,
                                                                 ifelse(wardnew == 5, 57554,
                                                                        ifelse(wardnew == 6, 94343,
                                                                               ifelse(wardnew == 7, 39165,
                                                                                      ifelse(wardnew == 8, 30910, NA))))))))
)

#  2011-2015 % BA
DF14 <- mutate(DF14,
             pctBA2011_15 = ifelse(wardnew == 1, 65.5,
                                   ifelse(wardnew == 2, 83.20,
                                          ifelse(wardnew == 3, 86.10,
                                                 ifelse(wardnew == 4, 46.80,
                                                        ifelse(wardnew == 5, 39.40,
                                                               ifelse(wardnew == 6, 68.90,
                                                                      ifelse(wardnew == 7, 16.90,
                                                                             ifelse(wardnew == 8, 13.90, NA))))))))
)

#  2011-2015 % Different Residence 1 year ago
DF14 <- mutate(DF14,
             MovedInPastYear = ifelse(wardnew == 1, 22.80,
                                      ifelse(wardnew == 2, 27.90,
                                             ifelse(wardnew == 3, 19.10,
                                                    ifelse(wardnew == 4, 9.70,
                                                           ifelse(wardnew == 5, 16.30,
                                                                  ifelse(wardnew == 6, 20.50,
                                                                         ifelse(wardnew == 7, 14.70,
                                                                                ifelse(wardnew == 8, 17.70, NA))))))))
)


# Write to CSV
#write.csv(DC_DF14, "DC_dataChecked11-11.csv", row.names= FALSE)

###### Visualizations################################################################################################################################


library(tidyverse)
#Respondents by Ward
count_byWard <- 
  DF14 %>%
  group_by(wardnew) %>%
  count()

WardRepresentation<- 
  ggplot(count_byWard, aes(x = wardnew, y = n)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(y = "Respondents", x = "Ward", title = "Roper Survey Respondents by Ward 2014") +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
WardRepresentation


# Race_count_byWard <- 
#   DC_df %>%
#   group_by(wardnew) %>%
#   count(Race)
# 
# RaceCount <-
#   Race_count_byWard %>%
#   group_by(wardnew) %>%
#   count(Race)
# 
# Race_count_byWard <- na.omit(Race_count_byWard)

#This data would be best visualized with a map not a graph. Maybe colors and transparency. 
# 
# respond_byrace <- ggplot(Race_count_byWard, aes(factor(Race), fill = factor(n))) +
#   geom_bar(position = "dodge")
# respond_byrace


###### Pop Change Graph 1980-2010 ##############################################
total_PopChange <- 
  DF14 %>%
  group_by(wardnew) %>%
  count(pctChange1980_10)
total_PopChange <- unlist(total_PopChange[2])

WardsForTables <- 
  DF14 %>%
  group_by(wardnew) %>%
  count()
wards <- unlist(WardsForTables[1])

popchangeTable <- data.frame(total_PopChange, wards)
PlotPopChange<- 
  ggplot(popchangeTable, aes(x = wards, y = total_PopChange)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  labs(y = "Percent Change", x = "Ward", title = "Ward Population Change 1980-2010") +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
PlotPopChange

###### Black views of gentrification Graph 2014 ##########################################################################

blackres <- filter(DF14, Race == 2)

blackOpinonGen <- 
  blackres %>%
  count(q31)

blackOpinonGen <- na.omit(blackOpinonGen)
blackOpinonGen$q31[5] <- 5
# Mainly a good thing
# Mainly a bad thing
#(VOL) Both
#(VOL) DK/No Opinion


plotBlackview <- 
  ggplot(blackOpinonGen, aes(x = q31, y = n/sum(blackOpinonGen)*100 )) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1.19))) +
  labs(y = "Percent", x= "views", title = "Black Residents Opinion on Gentrification 2014")+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                     labels = c('Good', 'Bad', 'Neither', 'Both', 'Dk/No Opin')) +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackview

###### White Views on gentrification 2014 #########################################################################
whiteres <- filter(DF14, Race == 1)

whiteOpinonGen <- 
  whiteres %>%
  count(q31)

whiteOpinonGen <- na.omit(whiteOpinonGen) 
whiteOpinonGen$q31[5] <- 5


plotWhiteview <- 
  ggplot(whiteOpinonGen, aes(x = q31, y = n/sum(whiteOpinonGen)*100 )) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(y = "Percent", x= "views", title = "White Residents Opinion on Gentrification 2014")+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                     labels = c('Good', 'Bad', 'Neither', 'Both', 'Dk/No Opin')) +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotWhiteview


BlackWhiteGenPlots <- plot_grid(plotBlackview, plotWhiteview, nrow = 1)
BlackWhiteGenPlots

# now add the title
title <- ggdraw() + 
  draw_label(
    "Attitudes on Gentrification 2015",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, BlackWhiteGenPlots,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
###### Black Opinion Stadium Graph 2014 ##############################################################
blackOpinonStad <- 
  blackres %>%
  count(q29net)

blackOpinonStad <- na.omit(blackOpinonStad)
blackOpinonStad$q29net[3] <- 3
# good investment NET
# bad investment NET
# DK/No opinion


plotBlackviewStad <- 
  ggplot(blackOpinonStad, aes(x = q29net, y = n/sum(blackOpinonStad)*100 )) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(y = "Count", x= "views", title = "Black Residents")+
  scale_x_continuous(breaks = c(1, 2, 3),
                     labels = c('Good', 'Bad', 'Dk/No Opin')) +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackviewStad


###### White Opinion Stadium Graph 2014 ##########################################################################
whiteOpinonStad <- 
  whiteres %>%
  count(q29net)

whiteOpinonStad <- na.omit(whiteOpinonStad)
whiteOpinonStad$q29net[3] <- 3
# good investment NET
# bad investment NET
# DK/No opinion


plotwhiteviewStad <- 
  ggplot(whiteOpinonStad, aes(x = q29net, y = n/sum(whiteOpinonStad)*100 )) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(y = "Count", x= "views", title = "White Residents")+
  scale_x_continuous(breaks = c(1, 2, 3),
                     labels = c('Good', 'Bad', 'Dk/No Opin')) +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
#plotwhiteviewStad

BlackWhiteStad <- plot_grid(plotBlackviewStad, plotwhiteviewStad, nrow=1)
#BlackWhiteStad

# now add the title
title <- ggdraw() + 
  draw_label(
    "Support for Stadium",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, BlackWhiteStad,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

###### Black Opinion Building Height 2014 ##########################################################################

blackOpinonHT <- 
  blackres %>%
  count(q30)

blackOpinonHT <- na.omit(blackOpinonHT)
blackOpinonHT$q30[3] <- 3
# good investment NET
# bad investment NET
# DK/No opinion


plotBlackviewHT <- 
  ggplot(blackOpinonHT, aes(x = q30, y = n/sum(blackOpinonHT)*100 )) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(y = "Percent", x= "Views", title = "Black Residents View Height Restritictions")+   ## maintain? check 
  scale_x_continuous(breaks = c(1, 2, 3),
                     labels = c('Good', 'Bad', 'Dk/No Opin')) +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotBlackviewHT

###### White Opinion Building Height Graph 2014 #######################################################################

whiteOpinonHT <- 
  whiteres %>%
  count(q30)

whiteOpinonHT <- na.omit(whiteOpinonHT)
whiteOpinonHT$q30[3] <- 3
# good investment NET
# bad investment NET
# DK/No opinion


plotWhiteviewHT <- 
  ggplot(whiteOpinonHT, aes(x = q30, y = n/sum(whiteOpinonHT)*100 )) +
  geom_col()+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(y = "Percent", x= "Views", title = "White Residents")+
  scale_x_continuous(breaks = c(1, 2, 3),
                     labels = c('Good', 'Bad', 'Dk/No Opin')) +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
#plotWhiteviewHT

BlackWhiteHT <- plot_grid(plotBlackviewHT, plotWhiteviewHT, nrow = 1)
#BlackWhiteHT

# now add the title
title <- ggdraw() + 
  draw_label(
    "Support for Height Restrictions",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, BlackWhiteHT,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
###### Number of respondents by ward 2014 graph ######################################################################
#Respondents by Ward
Race_count_byWard <- 
  DF14 %>%
  group_by(wardnew) %>%
  count(Race)

wardpop <- 
  DF14 %>%
  group_by(wardnew) %>%
  count()
wardpop <- na.omit(wardpop)

BlackCount <-
  DF14%>%
  group_by(wardnew) %>%
  count("bin"= Race==2)
BlackCount <- na.omit(BlackCount)
BlackCount <- split(BlackCount, BlackCount$bin)

BlackPop <- data.frame(BlackCount[2])


relativeBlack <- BlackPop$TRUE.n/wardpop$n
relBlackDF <- data.frame(wardpop$wardnew, relativeBlack)

BlackRelativePop<- 
  ggplot(relBlackDF, aes(x = wardpop.wardnew, y = relativeBlack)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(y = "Black Pop/ Total Pop", x = "Ward", title = "Black") +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
BlackRelativePop


###### White residents by ward Graph 2014 ########################################################################

WhiteCount <-
  DF14 %>%
  group_by(wardnew) %>%
  count("bin"= Race==1)
WhiteCount <- na.omit(WhiteCount)
WhiteCount <- split(WhiteCount, WhiteCount$bin)

WhitePop <- data.frame(WhiteCount[2])


relativeWhite <- WhitePop$TRUE.n/wardpop$n
relWhiteDF <- data.frame(wardpop$wardnew, relativeWhite)

WhiteRelativePop<- 
  ggplot(relWhiteDF, aes(x = wardpop.wardnew, y = relativeWhite)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(y = "White Pop/ Total Pop", x = "Ward", title = "white") +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
WhiteRelativePop

RaceRelativePop <- plot_grid(BlackRelativePop, WhiteRelativePop, nrow =1)
#RaceRelativePop ### I would like to align the grids a bit better, they are close

# now add the title
title <- ggdraw() + 
  draw_label(
    "Relative Population by Ward",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(                                                      ##### ERRORs HERE
  title, RaceRelativePop,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)


###### Rent Burden by Ward Graph Year 2014 ##########################################################################

RentBurdenByWard <-
  DF14 %>%
  group_by(wardnew) %>%
  count(rent_burden)
RentBurdenByWard <- na.omit(RentBurdenByWard)

RentBurdenVis<- 
  ggplot(RentBurdenByWard, aes(x = wardnew , y = rent_burden)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(y = "Rent Burden", x = "Ward", title = "Rent Burden by Ward") +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
RentBurdenVis

###### Home Ownership By Ward Graph Year 2014 ########################################################################


OwnershipByWard <-
  DF14 %>%
  group_by(wardnew) %>%
  count(pctOwnerOccupied)
OwnershipByWard <- na.omit(OwnershipByWard)

OwnPctVis<- 
  ggplot(OwnershipByWard, aes(x = wardnew , y = pctOwnerOccupied)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
  labs(y = "% Owner Occupied", x = "Ward", title = "Owner Occupied") +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
OwnPctVis

###### Rentals By Ward Graph Year 2014 ########################################################################
RentalByWard <-
  DF14 %>%
  group_by(wardnew) %>%
  count(pctRenterOccupied)
RentalByWard <- na.omit(RentalByWard)

RentPctVis<- 
  ggplot(RentalByWard, aes(x = wardnew , y = pctRenterOccupied)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  labs(y = "% Renter Occupied", x = "Ward", title = "Renter Occupied") +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
#RentPctVis

OwnerRentalVis <- plot_grid(OwnPctVis, RentPctVis, nrow=1 )
#OwnerRentalVis


# now add the title
title <- ggdraw() + 
  draw_label(
    "Home Occulation by Ward",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, OwnerRentalVis,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

###### 311 Calls By Ward Year 2014 #########################################################################

calls311byWard <-
  DF14 %>%
  group_by(wardnew) %>%
  count(calls_311)
calls311byWard <- na.omit(calls311byWard)

calls311Vis<- 
  ggplot(calls311byWard, aes(x = wardnew , y = calls_311)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_col() +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  labs(y = "311 Class Count 2014)", x = "Ward", title = "311 Calls Per Ward")
calls311Vis

###### 311 Calls Per White residents  by Ward #######################################################################
# we dont' have population
white_count_byWard <-
  DF14 %>%
  group_by(wardnew) %>%
  count(Race == 1)
white_count_byWard <- as.data.frame(white_count_byWard)
white_count_byWard <- na.omit(white_count_byWard)

white_count_byWard <- white_count_byWard %>%
  filter(`Race == 1` == TRUE)

callsbyRelativeWhite <- calls311byWard$calls_311/white_count_byWard
callsbyRelativeWhiteDF<- data.frame(wardpop$wardnew, callsbyRelativeWhite$n)

callsbyRelativeWhiteVis<- 
  ggplot(callsbyRelativeWhiteDF, aes(x = wardpop.wardnew , y = callsbyRelativeWhite.n)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  labs(y = "311 Calls", x = "Ward", title = "Per White Resident")
#callsbyRelativeWhiteVis

###### 311 Calls Per Black residents  by Ward #####################################################################
# we dont' have population
black_count_byWard <-
  DF14 %>%
  group_by(wardnew) %>%
  count(Race == 2)

black_count_byWard <- as.data.frame(black_count_byWard)
black_count_byWard <- na.omit(black_count_byWard)

black_count_byWard <- black_count_byWard %>%
  filter(`Race == 2` == TRUE)

callsbyRelativeBlack <- calls311byWard$calls_311/black_count_byWard
callsbyRelativeBlackDF<- data.frame(wardpop$wardnew, callsbyRelativeBlack)

callsbyRelativeBlackVis<- 
  ggplot(callsbyRelativeBlackDF, aes(x = wardpop.wardnew , y = n)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  labs(y = "311 Calls", x = "Ward", title = "Per Black Resident")
#callsbyRelativeBlackVis

### These Dont seem Helpful. 
calls_311Grid <- plot_grid(calls311Vis, callsbyRelativeBlackVis, callsbyRelativeWhiteVis, nrow =1)
#RaceRelativePop ### I would like to align the grids a bit better, they are close

# now add the title
title <- ggdraw() + 
  draw_label(
    "311 Calls Total & Disaggreaged by Race",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot               ### ERRORS HERE
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, calls_311Grid,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

###### Regression of 311 Calls 14-15 ######################################################################


reg311A <- lm(calls_311 ~ pctBlack2011_15 , data= DF14)
reg311B <- lm(calls_311 ~ pctOwnerOccupied, data = DF14)
reg311C <- lm(calls_311 ~ bpopchg, data = DF14) # as black pop increases 311 calls go down
reg311D <- lm(calls_311 ~ wpopchg, data = DF14) # as white pop increases 311 calls go up

# # Reg A: Pct Black to 311 Calls
# BPct311Calls <- plot(DC_df$pctBlack2011_15, DC_df$calls_311)
# visReg311A <- abline(reg311A)
# 
# # Reg B: Pct Black to 311 Calls
# Owner311Calls <- plot(DC_df$pctOwnerOccupied, DC_df$calls_311)
# visReg311B <- abline(reg311B)
# 
# # Reg C: Black pop Change to 311 calls
# BPopChange311Calls <- plot(DC_df$bpopchg, DC_df$calls_311)
# visReg311C <- abline(reg311C)
# 
# # Reg D: White pop Change to 311 calls
# WPopChange311Calls <- plot(DC_df$wpopchg, DC_df$calls_311)
# visReg311D <- abline(reg311D)


RegAVis <- DF14%>%
  ggplot(aes(x = pctBlack2011_15, y = calls_311)) +
  geom_point() + 
  geom_text(aes(label=wardnew),hjust= -1, vjust=0) +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "Percent Black" , y = "311 calls" )
#RegAVis

RegBVis <- DF14 %>%
  ggplot(aes(x = pctOwnerOccupied, y = calls_311)) +
  geom_point() +
  geom_text(aes(label=wardnew),hjust= -1, vjust=0) +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "Percent Owner Occupied" , y = "311 calls" )

RegCVis <- DF14 %>%
  ggplot(aes(x = bpopchg *100, y = calls_311)) +
  geom_point() +
  geom_text(aes(label=wardnew),hjust= -1, vjust=0) +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "black pop change" , y = "311 calls" )

RegDVis <- DF14 %>%
  ggplot(aes(x = wpopchg *100, y = calls_311)) +
  geom_point() +
  geom_text(aes(label=wardnew),hjust= -1, vjust=0) +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "white pop change" , y = "311 calls" )



## Cowplot them
calls_Vis311 <- plot_grid(RegAVis, RegBVis, RegCVis, RegDVis,   ### Some ERRORS HERE *****
                       nrow = 2)

# now add the title
title <- ggdraw() + 
  draw_label(
    "311 Call Trends by Ward 2014",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, calls_Vis311,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)



###### Visualizations 2015 #########################
whiteres15 <- filter(DF15, Race == "white")


whiteOpinonRD <- 
  whiteres15 %>%
  count(q11)


plotWhiteview <- 
  ggplot(whiteOpinonRD, aes(x = q11, y = n/sum(n)*100 )) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, .3))) +
  labs(y = "Percent", x= "views", title = "White Residents Views of Redevelopment")+   ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotWhiteview


## Cowplot them
RD_Opn_byRace <- plot_grid(plotBlackview, plotWhiteview, 
                          nrow = 1)

# now add the title
title <- ggdraw() + 
  draw_label(
    "Redevelopment Opinions 2015",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, RD_Opn_byRace,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)


###### General Gentrification ########
## Create new data frame of percents of respondents 
# because the number of respondents differs for each survey

## Attitudes Generally 02 &  14
# Total Attitudes
GenDV02 <- DF02 %>%
  count(Q32)

# Creates character out of column 1 not double
GenDV02$Q32 <- as.numeric(GenDV02$Q32)
GenDV02$Q32[GenDV02$Q32== 1] <- "Mostly Good"
GenDV02$Q32[GenDV02$Q32== 2] <- "Mostly Bad"
GenDV02$Q32[GenDV02$Q32== 3] <- "Neither"
GenDV02$Q32[GenDV02$Q32== 4] <- "Both"
GenDV02$Q32[GenDV02$Q32== 8] <- "DK No Opinion"


plotGenDV02 <- 
  ggplot(GenDV02, aes(x = Q32, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = " Residents Views of Redevelopment 2002")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotGenDV02


## 2014 Generally
GenDV14 <- DF14 %>%
  count(q31)

# Creates character out of column 1 not double
GenDV14$q31 <- as.numeric(GenDV14$q31)
GenDV14$q31[GenDV14$q31== 1] <- "Mostly Good"
GenDV14$q31[GenDV14$q31== 2] <- "Mostly Bad"
GenDV14$q31[GenDV14$q31== 3] <- "Neither"
GenDV14$q31[GenDV14$q31== 4] <- "Both"
GenDV14$q31[GenDV14$q31== 8] <- "DK No Opinion"


plotGenDV14 <- 
  ggplot(GenDV14, aes(x = q31, y = n/sum(n)*100)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  labs(y = "Percent", x= "views", title = " Residents Views of Redevelopment 2014")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotGenDV14

## Cowplot them
GenDV02and14 <- plot_grid(plotGenDV02, plotGenDV14, 
                             nrow = 1)
GenDV02and14


### Gentrification views by Race
Black02 <- filter(DF02, Black == 1)
GenDVBlack02 <- 
  Black02 %>%
  count(Q32)

# Creates character out of column 1 not double
GenDVBlack02$Q32 <- as.numeric(GenDVBlack02$Q32)
GenDVBlack02$Q32[GenDVBlack02$Q32== 1] <- "Mostly Good"
GenDVBlack02$Q32[GenDVBlack02$Q32== 2] <- "Mostly Bad"
GenDVBlack02$Q32[GenDVBlack02$Q32== 3] <- "Neither"
GenDVBlack02$Q32[GenDVBlack02$Q32== 4] <- "Both"
GenDVBlack02$Q32[GenDVBlack02$Q32== 8] <- "DK No Opinion"


plotGenDVBlack02 <- 
  ggplot(GenDVBlack02, aes(x = Q32, y = n/sum(n)*100)) +
  geom_col() +
  # scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1)) +
  labs(y = "Percent", x= "views", title = "Black Residents Views of Redevelopment 2002")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotGenDVBlack02


# White Residents Views 2002
White02 <- filter(DF02, White == 1)
GenDVWhite02 <- 
  White02 %>%
  count(Q32)

GenDVWhite02$Q32 <- as.numeric(GenDVWhite02$Q32)
GenDVWhite02$Q32[GenDVWhite02$Q32== 1] <- "Mostly Good"
GenDVWhite02$Q32[GenDVWhite02$Q32== 2] <- "Mostly Bad"
GenDVWhite02$Q32[GenDVWhite02$Q32== 3] <- "Neither"
GenDVWhite02$Q32[GenDVWhite02$Q32== 4] <- "Both"
GenDVWhite02$Q32[GenDVWhite02$Q32== 8] <- "DK No Opinion"


plotGenDVWhite02 <- 
  ggplot(GenDVWhite02, aes(x = Q32, y = n/sum(n)*100)) +
  geom_col() +
 # scale_y_continuous(expand = expansion(mult = c(0, 1))) +
  scale_y_continuous(labels = percent_format(), limits=c(0,1)) +
  labs(y = "Percent", x= "views", title = "White Residents Views of Redevelopment 2002")+    ### Shorten title if using cowplot
  geom_text(aes(label = signif(n/sum(n)*100, digits = 3)), vjust = -0.5) +
  theme_minimal()
plotGenDVWhite02


## Cowplot them
GenDV02ByRace <- plot_grid(plotGenDVBlack02, plotGenDVWhite02, 
                          nrow = 1)
GenDV02ByRace
####### Support Gen 


###### Do not Support

###### Support Black v White 


### Attitudes for People like You 00, 06, 14, & 15
####### Support Gen 




###### Do not Support

###### Support Black v White 


### Your neighborhood 02, 06, 08
####### Support Gen 

###### Do not Support

###### Support Black v White 


##### Winners and Losers Gentrification 
### Black White
### Rich Poor
### Your NBHD



