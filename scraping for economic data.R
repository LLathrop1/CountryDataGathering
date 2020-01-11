library(tidyverse)
library(ggplot2)
library(rvest)
library(dplyr)
library(plyr)
library(readxl)

#Getting the list of countries link
website<- "https://www.focus-economics.com/countries"
country_list<-read_html(website) %>%
  html_node("#main > div.row-main > div.row.mobile-order-change > div.col.col-1.grid-8 > div > div > div > ul") 
  
#getting the nnames of the countries
country_list_name<- country_list %>%
  html_nodes("li")%>%
  html_node("a")%>%
  html_text()

#Getting the url extension to the website
country_url_list<- country_list %>%
  html_nodes('li')%>%
  html_node("a") %>%
  html_attr("href")

#Making a table with the data
countries<-as.data.frame(cbind.data.frame(country_list_name,country_url_list), row.names = NULL, col.names = names(c("Country.Name","Country.URL")))%>%
  mutate(country_url_list = paste("https://www.focus-economics.com",country_url_list, sep = ""))

#Function for getting table
pulldata<-function(x,y){
  
  read_html(x)%>% 
    html_node("table")%>% 
    html_table(fill = TRUE)%>%
    cbind.data.frame(y)%>%
    list()
  
}

#pulling data from site for all countries
country_eco1<- mapply(pulldata, countries[c(1:20,22:44,46:129),2], countries[c(1:20,22:44,46:129),1])%>%
  rbind.fill()
country_eco1<-country_eco1[c(1:2845,2847:3083),c(1,6,7)]
names(country_eco1)<- c("V1","2017","Country")
country_eco1<-spread(country_eco1, "V1", "2017")


#population data
popdata1<- read_html("http://worldpopulationreview.com/")%>%
  html_nodes("#liveWorldPop > div.content.has-flag.container-fluid.horizontal-section-container.undefined > div > div > div > div:nth-child(2) > div:nth-child(2) > div:nth-child(2) > table")%>%
  html_table()%>%
  rbind.fill()%>%
  as.data.frame()
names(popdata1)<- c("Flag","Country","2019 Population", "2018 Population", "Pop_Area (km2)", "2019 Density (km2)","Growth Rate","World Percent","Rank of Population")

#male female population
genderpop<- read_html("http://statisticstimes.com/demographics/countries-by-sex-ratio.php")%>%
  html_nodes("#table_id")%>%
  html_table(fill=TRUE)%>%
  rbind.fill()%>%
  as.data.frame()
names(genderpop)<-c("Gender Rank","Country","Male Ratio","Female Ratio","Gender_Population","Male Population","Female Population","Continent")

#population table 2
popdata2<- read_html("https://www.worldometers.info/world-population/population-by-country/")%>%
  html_nodes("#example2")%>%
  html_table()%>%
  rbind.fill()%>%
  as.data.frame()
names(popdata2)<- c("Rank_Pop2","Country","Pop2_Population 2019","Yearly Change", "Net Pop Change","Density Pop2 (km2)", "Pop2_Land Area (km2)","Migrants","Fert Rate","Median Age Pop2", "Urban Pop Ratio", "World Share of Pop_Pop2")

#age of people
agedemodata<- read_html("https://www.nationmaster.com/country-info/stats/People/Age-distribution/Population-aged-15--24/Total")%>%
  html_nodes("#table-container > div.table-wrapper > div.scrollable > table")%>%
  html_table()%>%
  rbind.fill()%>%
  as.data.frame()
names(agedemodata)<- c("Rank_age", "Country", "Agedemo_Population", "Date Taken", "Graph", "History")

#density by land
densitydata<- read_html("http://worldpopulationreview.com/countries/countries-by-density/")%>%
  html_nodes("#dataTable > div.content.has-flag.container-fluid.horizontal-section-container.undefined > div > div > div > div > div:nth-child(2) > table")%>%
  html_table()%>%
  rbind.fill()%>%
  as.data.frame()
names(densitydata)<- c("Rank of country","Country","Density (KM2)","Density (mi2)", "Density_Population", "Area (KM2)")

#income data
incomedata<- read_html("https://www.worlddata.info/average-income.php")%>%
  html_nodes("#tabsort")%>%
  html_table()%>%
  rbind.fill()%>%
  as.data.frame()
names(incomedata)<-c("Rank_Income","Country", "Avg Annual Income", "Avg Monthly Income")

#education data
educationdata<- read_html("http://worldpopulationreview.com/countries/education-rankings-by-country/")%>%
  html_nodes("#dataTable > div.content.has-flag.container-fluid.horizontal-section-container.undefined > div > div > div > div > div:nth-child(2) > table")%>%
  html_table()%>%
  rbind.fill()%>%
  as.data.frame()
names(educationdata)<- c("Country", "Total Ed Score","Reading Score", "Math Score", "Science Score")

#age mean data
agedata2<- read_html("http://worldpopulationreview.com/countries/median-age/")%>%
  html_nodes("#__next > div > div.outer-container.clearfix > div.inside-body-content-container.clearfix > div:nth-child(2) > div > div.col-md-8 > div > div > table")%>%
  html_table()%>%
  rbind.fill()%>%
  as.data.frame()
names(agedata2)<- c("Country","Median Age","Median Age Male", "Median Age Female")

#life excpentance
lifelongdata<- read_html("http://worldpopulationreview.com/countries/life-expectancy-by-country/")%>%
  html_nodes("#__next > div > div.outer-container.clearfix > div.inside-body-content-container.clearfix > div:nth-child(2) > div > div > div > div > div > div:nth-child(2) > table")%>%
  html_table()%>%
  rbind.fill()%>%
  as.data.frame()
names(lifelongdata)<- c("Country","Avg Life", "Avg Life Male", "Avg Life Female")

#debt ratio
debtratiodata<- read_html("http://worldpopulationreview.com/countries/countries-by-national-debt/")%>%
  html_nodes("#__next > div > div.outer-container.clearfix > div.inside-body-content-container.clearfix > div:nth-child(2) > div.content.has-flag.container-fluid.horizontal-section-container.undefined > div > div > div > div > div:nth-child(2) > table")%>%
  html_table()%>%
  rbind.fill()%>%
  as.data.frame()
names(debtratiodata)<- c("Country", "National Debt to GDP Ratio","Debt Ratio_Population")


#joning data by country
#b<- join(popdata2, c(country_eco1,popdata1,genderpop,agedemodata,densitydata,incomedata,
                    #educationdata,agedata2,lifelongdata,debtratiodata
                   
CountryDataDemo<-join(popdata2,country_eco1, by="Country", type="full")
CountryDataDemo<-join(CountryDataDemo,popdata1, by="Country", type="full")
CountryDataDemo<-join(CountryDataDemo,genderpop, by="Country", type="full")
CountryDataDemo<-join(CountryDataDemo,agedemodata, by="Country", type="full")
CountryDataDemo<-join(CountryDataDemo,densitydata, by="Country", type="full")
CountryDataDemo<-join(CountryDataDemo,incomedata, by="Country", type="full")
CountryDataDemo<-join(CountryDataDemo,educationdata, by="Country", type="full")
CountryDataDemo<-join(CountryDataDemo,agedata2, by="Country", type="full")
CountryDataDemo<-join(CountryDataDemo,lifelongdata, by="Country", type="full")
CountryDataDemo<-join(CountryDataDemo,debtratiodata, by="Country", type="full")


#cellphone speed
cellspeed<- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_Internet_connection_speeds")%>%
  html_node("#mw-content-text > div > table:nth-child(4) > tbody > tr:nth-child(2) > td:nth-child(2) > table")%>%
  html_table()%>%
  rbind.fill()
names(cellspeed)<-c("Rank of Cell Speed","Country","Ave Connection Speed (Mb/s)")
CountryDataDemo<-join(CountryDataDemo,cellspeed, by="Country", type="full")

#percent of coverage for 4G
speed4gcover<- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_4G_LTE_penetration")%>%
  html_node("#mw-content-text > div > table:nth-child(8)")%>%
  html_table()%>%
  rbind.fill()
names(speed4gcover)<-c("Rank of 4G","Country","Coverage of 4G")
CountryDataDemo<-join(CountryDataDemo,speed4gcover, by="Country", type="full")

#mobile users across the world
mobileusers<- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_number_of_mobile_phones_in_use")%>%
  html_node("#mw-content-text > div > table")%>%
  html_table()%>%
  rbind.fill()
names(mobileusers)<-c("Rank by Mobile Users","Country","Num of Phone Numbers","Mobile_Population","Connections Per 100","Date of data")
CountryDataDemo<-join(CountryDataDemo,mobileusers, by="Country", type="full")


#internet users across wht world needs spreadsheet 7
internetusers2<-read_excel("C:/Users/larry/Downloads/AAAAAA.xlsx", sheet = "Sheet6")
names(internetusers2)<-c("Country","Internet Users","Internet_Population of Country","Internet User Rank","Percetnage of Internet User",'Rank of Percentage user internet')
CountryDataDemo<-join(CountryDataDemo,internetusers2, by="Country", type="full")


#Internet host by country by sheet in excel 8
internethost<-read_excel("C:/Users/larry/Downloads/AAAAAA.xlsx", sheet = "Sheet7")
names(internethost)<- c("Country","Internet Host Total","Internet Host Rank")
CountryDataDemo<-join(CountryDataDemo,internethost, by="Country", type="full")


#corpoate debt by country
corpratedebt<-read_html("https://en.wikipedia.org/wiki/List_of_countries_by_corporate_debt")%>%
  html_nodes("#mw-content-text > div > table")%>%
  html_table()%>%
  rbind.fill()
corpratedebt<-corpratedebt[,c(1,13)]
names(corpratedebt)<-c("Country","2017 Corprate Debt Rate")
CountryDataDemo<-join(CountryDataDemo,corpratedebt, by="Country", type="full")


#Country scholar journel publication
scholarjor<-read_html("https://en.wikipedia.org/wiki/List_of_countries_by_number_of_scientific_and_technical_journal_articles")%>%
  html_nodes("#mw-content-text > div > table:nth-child(2)")%>%
  html_table()%>%
  rbind.fill()
names(scholarjor)<-c("Rank of published","Country","Published Articles 2016 Total")
CountryDataDemo<-join(CountryDataDemo,scholarjor, by="Country", type="full")

#McDonlads by country
macdonladlist<-read_html("https://en.wikipedia.org/wiki/List_of_countries_with_McDonald%27s_restaurants")%>%
  html_nodes("#mw-content-text > div > table:nth-child(7)")%>%
  html_table()%>%
  rbind.fill()
names(macdonladlist)<-c("Rank by McD","Country","First Open","First Location","Current Number","McD Source","People Per McD","Notes")
CountryDataDemo<-join(CountryDataDemo,macdonladlist, by="Country", type="full")


#GDP of savings on excel sheet 9
savinggdp<-read_excel("C:/Users/larry/Downloads/AAAAAA.xlsx", sheet = "Sheet9")
names(savinggdp)<-c("Rankof SveGDP","Country","Cross National Savings Percent of GDP","Year of Svaing GDP Taken")
CountryDataDemo<-join(CountryDataDemo,savinggdp, by="Country", type="full")

#physioaions per 10k people on excel sheet 10
docpeople<-read_excel("C:/Users/larry/Downloads/AAAAAA.xlsx", sheet = "Sheet10")
names(docpeople)<-c("Doc Rank","Country","Total Physicans","Physicans per 10K","2013 Physicans per 10K")
docpeople<-docpeople[,c(1,2,5)]
CountryDataDemo<-join(CountryDataDemo,docpeople, by="Country", type="full")

#road map for logtics on excel sheet 12
roadstrack<-read_excel("C:/Users/larry/Downloads/AAAAAA.xlsx", sheet = "Sheet11")
names(roadstrack)<-c("Road Rank","Country","Length of Roads (km)","Expressways","Year of Road Data")
CountryDataDemo<-join(CountryDataDemo,roadstrack, by="Country", type="full")

#labor force and type of labor sector on excel sheeet 12
laborforce<-read_excel("C:/Users/larry/Downloads/AAAAAA.xlsx", sheet = "Sheet12")
names(laborforce)<-c("Labor Rank","Country","Labor Force","Date of Labor Data","Agriculture Labor","Industry Labor","Service Labor","Date of Labor data Year","")
CountryDataDemo<-join(CountryDataDemo,laborforce, by="Country", type="full")

#government type by country
governmenttype<-read_html("https://en.wikipedia.org/wiki/List_of_countries_by_system_of_government")%>%
  html_nodes("#mw-content-text > div > table:nth-child(14)")%>%
  html_table()%>%
  rbind.fill()
names(governmenttype)<-c("Country","Constitutional Form", "Head of State","Basis of Exc Legitimacy")
CountryDataDemo<-join(CountryDataDemo,governmenttype, by="Country", type="full")


#tariffs rate by country
traffrate<-read_html("https://en.wikipedia.org/wiki/List_of_countries_by_tariff_rate")%>%
  html_nodes("#mw-content-text > div > table")%>%
  html_table()%>%
  rbind.fill()
names(traffrate)<-c("Rank by Traiff","Country","Traiff Rate Applied","Year Updated Traiff")
CountryDataDemo<-join(CountryDataDemo,traffrate, by="Country", type="full")

#happy index by country
happyindex<-read_html("https://en.wikipedia.org/wiki/World_Happiness_Report")%>%
  html_nodes("#mw-content-text > div > table")%>%
  html_table()%>%
  rbind.fill()
names(happyindex)<-c("Happy Rank", "Country","Happy Score","Happy GDA per Captia","Soical Support","Healthly Life Exp","Freedom of Life Choice","Generosity","Perception of Corruption")
CountryDataDemo<-join(CountryDataDemo,happyindex, by="Country", type="full")


#education index
eduindex<-read_html("https://en.wikipedia.org/wiki/Education_Index")%>%
  html_nodes("#mw-content-text > div > table")%>%
  html_table()%>%
  rbind.fill()
names(eduindex)<-c("Ed Index Rank", "Country","Education Index","Expected Years of School","Mean Year of School","HDI Rank","Continent EduIndex")
CountryDataDemo<-join(CountryDataDemo,eduindex, by="Country", type="full")


#mean wealth of adults
adultwealth<-read_html("https://en.wikipedia.org/wiki/List_of_countries_by_wealth_per_adult")%>%
  html_nodes("#mw-content-text > div > table:nth-child(14)")%>%
  html_table()%>%
  rbind.fill()
names(adultwealth)<-c("Adult Wwealth Rank","Country","Median Wealth Adult","Mean Wealth Adult","Adult Population_Adult Wealth")
CountryDataDemo<-join(CountryDataDemo,adultwealth, by="Country", type="full")


#consumer market gpd 
consumermarkets<-read_html("https://en.wikipedia.org/wiki/List_of_largest_consumer_markets")%>%
  html_nodes("#mw-content-text > div > table:nth-child(3) > tbody > tr > td:nth-child(2) > table")%>%
  html_table()%>%
  rbind.fill()
names(consumermarkets)<-c("Country","Makret Size of Consumers","Markets Percent of GDP","Year taken for Consumer Markets")
CountryDataDemo<-join(CountryDataDemo,consumermarkets, by="Country", type="full")

#minium wage data 
miniumwage<-read_html("https://en.wikipedia.org/wiki/List_of_minimum_wages_by_country")%>%
  html_nodes("#mw-content-text > div > table")%>%
  html_table(fill=TRUE)%>%
  rbind.fill()
names(miniumwage)<-c("Country","Miniwage Desc","Annual Income USD","Annunal Income PPP","Work Week Hours","Hourly Rate USD","Hourly Rate PPP","Percent of GDP for minium wage","Effective PPP")
CountryDataDemo<-join(CountryDataDemo,miniumwage, by="Country", type="full")

#Innovation index
innovationindex<-read_html("https://en.wikipedia.org/wiki/International_Innovation_Index")%>%
  html_nodes("#mw-content-text > div > table:nth-child(11)")%>%
  html_table()%>%
  rbind.fill()
names(innovationindex)<-c("Innovation Rank","Country","Innvoation Overall Score","Innovation Inputs","Innovation Performance")
CountryDataDemo<-join(CountryDataDemo,innovationindex, by="Country", type="full")

#ease of business
easebusiness<-read_html("https://en.wikipedia.org/wiki/Ease_of_doing_business_index")%>%
  html_nodes("#mw-content-text > div > table")%>%
  html_table()%>%
  rbind.fill()
easebusiness<- easebusiness[,c(1,2,4)]
names(easebusiness)<-c("Ease of Business Classification","Country","Year 2019 Eas of Business Rank")
CountryDataDemo<-join(CountryDataDemo,easebusiness, by="Country", type="full")

#economic Freedom index
ecofreeindex<-read_html("https://en.wikipedia.org/wiki/Index_of_Economic_Freedom")%>%
  html_nodes("#mw-content-text > div > table:nth-child(7)")%>%
  html_table()%>%
  rbind.fill()
names(ecofreeindex)<-c("Country","Economic Freedom Score","Eco Freedom Change from Last Year")
CountryDataDemo<-join(CountryDataDemo,ecofreeindex, by="Country", type="full")

#web index score
webindexscore<-read_html("https://en.wikipedia.org/wiki/Web_index")%>%
  html_nodes("#mw-content-text > div > table > tbody > tr > td:nth-child(1) > table")%>%
  html_table()%>%
  rbind.fill()
names(webindexscore)<-c("Web Index Rank","Country","Web Index Score")
CountryDataDemo<-join(CountryDataDemo,webindexscore, by="Country", type="full")

#cars per captial on excel sheet 13
carspeople<-read_excel("C:/Users/larry/Downloads/AAAAAA.xlsx", sheet = "Sheet13")
names(carspeople)<-c("Rank of Cars Per People","Country","Cars per 1k People","Total Cars","Year of Car Data")
CountryDataDemo<-join(CountryDataDemo,carspeople, by="Country", type="full")

#rail usage by people
trainuseage<-read_html("https://en.wikipedia.org/wiki/List_of_countries_by_rail_usage")%>%
  html_nodes("#mw-content-text > div > table:nth-child(13)")%>%
  html_table()%>%
  rbind.fill()
names(trainuseage)<-c("Train Useage Rank","Country","Billions Pass by KM","Data Year for Train")
CountryDataDemo<-join(CountryDataDemo,trainuseage, by="Country", type="full")

#freedom classification on excel sheet 14
freedomindex<-read_excel("C:/Users/larry/Downloads/AAAAAA.xlsx", sheet = "Sheet14")
names(freedomindex)<-c("country","Freedom World Score","Index Economic Freedom","Press Freedom Score","Democracy Index")
CountryDataDemo<-join(CountryDataDemo,freedomindex, by="Country", type="full")

  
#where to be born index
wherebornindex<-read_html("https://en.wikipedia.org/wiki/Where-to-be-born_Index")%>%
  html_nodes("#mw-content-text > div > table:nth-child(11)")%>%
  html_table()%>%
  rbind.fill()
names(wherebornindex)<-c("Where Born Rank","Country","Where Born Score")
CountryDataDemo<-join(CountryDataDemo,wherebornindex, by="Country", type="full")

#country list of net exports
netexports<-read_html("https://en.wikipedia.org/wiki/List_of_countries_by_net_exports")%>%
  html_nodes("#mw-content-text > div > table")%>%
  html_table()%>%
  rbind.fill()
names(netexports)<-c("Net Export Rank","Country","Net Export in USD","Percent of GDP on Net Exports",'Year Net Export Data Taken')
CountryDataDemo<-join(CountryDataDemo,netexports, by="Country", type="full")

#Country complexity index on excel sheet 15
complexindex <- read_excel("C:/Users/larry/Downloads/AAAAAA.xlsx", sheet = "Sheet15")
names(complexindex)<-c("Rank of Complex","Country Abrv","Country","ECI Value Complex Score")
CountryDataDemo<-join(CountryDataDemo,complexindex, by="Country", type="full")

#airport count by country
totalairport<-read_html("https://www.cia.gov/library/publications/the-world-factbook/rankorder/2053rank.html")%>%
  html_nodes("#rankOrder")%>%
  html_table()%>%
  rbind.fill()
names(totalairport)<-c("TOtal Airport Rank","Country","Total Airports","Date of Airport Info")
CountryDataDemo<-join(CountryDataDemo,totalairport, by="Country", type="full")

#cost of living index
costoflivingindex<-read_html("https://www.worlddata.info/cost-of-living.php")%>%
  html_nodes("#tabsort")%>%
  html_table()%>%
  rbind.fill()
names(costoflivingindex)<-c("Cost of Living Index Rank","Country","Cost of Living Index Score","Monthly Income Needed","Purchasing Power of Consumer")
CountryDataDemo<-join(CountryDataDemo,costoflivingindex, by="Country", type="full")

#cost of 1gb cell service from the size for the sheet https://howmuch.net/sources/the-price-of-mobile-internet-worldwide-2019
costof1gb <- read_excel("C:/Users/larry/Downloads/AAAAAA.xlsx", sheet = "Sheet17")
names(costof1gb)<-c("Rank of 1 GB", "Country Code","Country","Continent","Plans Measured","Average 1 gb Price","Currency","Converstion Rate to USD","Avger 1 gb USD","Cheapeast 1gb Local Cur","Cheapest 1gb USD Cur","EXP 1gb Local CUR","EXP 1gb USD Cur","Sample 1gb date")
CountryDataDemo<-join(CountryDataDemo,costof1gb, by="Country", type="full")



<-read_html("")%>%
  html_nodes("")%>%
  html_table()%>%
  rbind.fill()
names()<-c("")

incomedata$`Avg Monthly Income` <-as.numeric(gsub("\\$","",incomedata$`Avg Monthly Income`))
debtratiodata$`National Debt to GDP Ratio`<-debtratiodata$`National Debt to GDP Ratio`/100

costof1gb$`EXP 1gb USD Cur`<-as.numeric(gsub('\\$',"",costof1gb$`EXP 1gb USD Cur`))

gg<- consumermarkets$`Markets Percent of GDP`<-as.numeric(consumermarkets$`Markets Percent of GDP`)/100
consumermarkets$`Makret Size of Consumers`<-as.numeric(gsub(",","",consumermarkets$`Makret Size of Consumers`))




