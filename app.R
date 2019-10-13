library(shiny)
# install.packages('dplyr')
library(dplyr)

library(choroplethr)

library(choroplethrMaps)

library(ggplot2)

library(shinythemes)


# install.packages('readxl')
library(readxl)
install.packages("caret")
library(caret)

# install.packages('data.table')
library(data.table)

library(sqldf)

library(h2o)

# library(xgboost)

library(e1071)

library(markdown)



library(ggthemes)

# library(ggmap)

library(maps)
# install.packages('mapdata')
library(mapdata)





# ------------------------------------ Tab 1 Data Prep ---------------------------------

# setwd('E:/R') ####Set your working Directory Accordingly

setwd('D:\\Anjali\\Purdue\\Fall 1\\Analytics for R\\Data Palooza\\Project files\\Final')
getwd()

county_data <- readxl::read_excel("infant-mortality-by-county-yr-COD.xlsx", col_names = T)
# head(county_data)


county_yr <- county_data %>%
  
  group_by(CNTY, CHILD_DEATH_YR) %>%
  
  summarize(value = sum(CNT_BY_UNDERLYING_COD, na.rm = T))



data(county.regions)

county.regions <- filter(county.regions, state.name == "indiana")



# county.regions



county_yr$CNTY <- tolower(county_yr$CNTY )



totl_county <-  data.frame(region=numeric(),
                           
                           county.fips.character=character(),
                           
                           county.name=character(),
                           
                           state.name =character(),
                           
                           state.fips.character=character(),
                           
                           state.abb=character(),
                           
                           year = numeric())





for (i in unique(county_yr$CHILD_DEATH_YR)){
  
  xx<-county.regions
  
  xx$year <- i
  
  totl_county =rbind(totl_county,xx)
  
}





county_test <- left_join( totl_county, county_yr, by = c( "county.name" = "CNTY", "year" = "CHILD_DEATH_YR"))



county_test$value[is.na(county_test$value)] <- 0





# ------------------------------------ Tab 2 Data Prep ---------------------------------

mortalitycause <- read.csv("infant-mortality-by-county-year-and-underlying-cause-of-death.csv",stringsAsFactors = FALSE)

obgyn <- read.csv("count-of-ob-gyn-providers-by-county.csv",stringsAsFactors = FALSE)

obemor <- read.csv("infant-mortality-by-couty and obesity.csv",stringsAsFactors = FALSE)



# ----------- Plot2 ---------------------

a <-sqldf("
          
          SELECT UNDERLYING_COD, sum(CNT_BY_UNDERLYING_COD) as count
          
          FROM mortalitycause
          
          GROUP BY UNDERLYING_COD
          
          ORDER BY sum(CNT_BY_UNDERLYING_COD) DESC
          
          ")

b <- a[1:10,]

b$UNDERLYING_COD[6]<-"Unknown"

# b



# causeplot<-ggplot(data=b, aes(x=reorder(b$UNDERLYING_COD,b$count),y=b$count))+geom_bar(stat="identity")+coord_flip()+labs(title = "Top 10 Causes of Infant Death", x= ("Count"),y=("Cause of Death"))







# ----------- Plot3 ---------------------

ob <- obgyn[order(-obgyn$Providers.Count),][1:10,]

# ob

# obgynp<-ggplot(data=ob, aes(x=reorder(ob$County.Name,-ob$Providers.Count), y=ob$Providers.Count)) +
#   
#   geom_bar(stat="identity")+labs(title = "Top 10 Counties with OBGYN Provides", x= ("County"),y=("Number of OBGYN Providers"))





# ----------- Plot4 ---------------------



obemor$County<- NULL

# colnames(obemor)

obemor <-obemor %>%
  
  rename(
    
    Infant_Mortalities=Infant.Mortalities,
    
    Number_of_Obese_women=Number.of.Obese.women
    
    
    
  )

c <-sqldf("
          
          SELECT County, X2018
          
          FROM obemor
          
          GROUP BY County, X2018
          
          ORDER BY X2018 DESC
          
          ")

d <- c[1:10,]

# d



# obemoreplot<-ggplot(data=d, aes(x=reorder(d$COUNTY,-d$X2018),y=d$X2018))+geom_bar(stat="identity")+labs(title = "Distribution of Obese Women by County", x= ("County"),y=("Count of Obese Women"))





# ----------- Prediction Tab ---------------------



dm <- read.csv('clean_data.csv')

# dm%>%mutate(Target=ifelse(CHILD_MORT_IND=='Y',1,0))->dm

# dm%>%select(-CHILD_MORT_IND)->dm

# dm%>%select(-CHILD_ID)->dm

# dm%>%select(-MOTHER_ID)->dm

# dm%>%select(-X)->dm

set.seed(1234) # set a seed so you can replicate your results



# inTrain <- createDataPartition(y = dm$Target,   # outcome variable

#                                p = .70,   # % of training data you want

#                                list = F)

# # create your partitions

# train <- dm[inTrain,]  # training data set

# test <- dm[-inTrain,]

#model1 <- glm(Target~., family = binomial, data=train)



# library(h2o)

h2o.init()

h2o.clusterInfo()

dm$Target<- as.factor(dm$Target)

dmh <- as.h2o(dm)

y <- "Target"                       # target variable to learn

x <- setdiff(names(dmh), y)

parts <- h2o.splitFrame(dmh, 0.8, seed=99) # randomly partition data into 80/20

train <- parts[[1]]                         # random set of training obs

test <- parts[[2]]



# rf <- h2o.randomForest(x,y,train)

# h2o.performance(rf, test)



gbm <- h2o.gbm(x,y,train)

h2o.performance(gbm,test)



# glm <- h2o.glm(x,y,train)

# h2o.performance(glm,test)



# dl <- h2o.deeplearning(x,y,train)

# h2o.performance(dl,test)





# --------------------------------------------------------------------------------------

# ------------------------------------ App Starts Here ---------------------------------

# --------------------------------------------------------------------------------------



ui <- fluidPage(
  
  
  
  theme = shinytheme("flatly"),
  
  headerPanel('Healthy Mommy and Baby'),
  
  
  
  
  
  navbarPage("Navbar",
             
             tabPanel("County Map",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          selectInput('yr', 'Year', unique(county_test$year),
                                      
                                      selected =unique(county_test$year)[[2]])
                          
                          
                          
                        ),
                        
                        mainPanel(
                          
                          plotOutput('plot1', height=700,width = 600)
                          
                        )
                        
                      )
                      
             ),
             
             tabPanel("Graphs",
                      
                      fluidRow(
                        
                        plotOutput("plot2"),
                        
                        plotOutput("plot3"),
                        
                        plotOutput("plot4")
                        
                      )
                      
             ),
             
             tabPanel("Prediction Proability" ,
                      
                      fluidRow(
                        
                        column(3,
                               
                               h4("Diamonds Explorer"),
                               
                               numericInput(inputId='NUM_BIRTHS_BY_MOTHER', label='NUM_BIRTHS_BY_MOTHER', value = 1,min = NA, max = NA, step = NA,width = NULL),
                               
                               
                               
                               br(),
                               
                               radioButtons(inputId='CHILD_BIRTH_YR_GRP', label='CHILD_BIRTH_YR_GRP', c('2010 - 2012','2013 - 2015','2016 - 2018'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='MOTHER_AGE_GRP', label='MOTHER_AGE_GRP', c('>=65 Years','14 Years and Under','15-18 Years','18-25 Years','25-35 Years','35-45 Years','45-55 Years','55-65 Years'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='MOTHER_MARITALSTATUS_AT_BIRTH', label='MOTHER_MARITALSTATUS_AT_BIRTH', c('Divorced','Married','Married but refused to provide husbands information','Never Married','Widowed'), selected = NULL, inline = FALSE,width = NULL)
                               
                               
                               
                               
                               
                        ),
                        
                        column(4, offset = 1,
                               
                               radioButtons(inputId='MOTHER_RESID_COUNTY_TYPE', label='MOTHER_RESID_COUNTY_TYPE', c('Rural','Rural/Mixed','Urban'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='BIRTH_COMPLICATIONS', label='BIRTH_COMPLICATIONS', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='DIABETES_RISK_PREPREGNANCY', label='DIABETES_RISK_PREPREGNANCY', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='DIABETES_RISK_GESTATIONAL', label='DIABETES_RISK_GESTATIONAL', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='SMOKING_IND', label='SMOKING_IND', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='SMOKING_DURING_PREG_IND', label='SMOKING_DURING_PREG_IND', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='SMOKING_BEFORE_PREG_IND', label='SMOKING_BEFORE_PREG_IND', c('N','Y'), selected = NULL, inline = FALSE,width = NULL)
                               
                               
                               
                        ),
                        
                        column(4,
                               
                               radioButtons(inputId='VISITS_IN_1ST_TRIMESTER_IND', label='VISITS_IN_1ST_TRIMESTER_IND', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='CHILD_GENDER', label='CHILD_GENDER', c('Female','Male'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='CHILD_TRANSFERRED', label='CHILD_TRANSFERRED', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='CHILD_ALIVE', label='CHILD_ALIVE', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='LOW_BIRTH_WEIGHT', label='LOW_BIRTH_WEIGHT', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                               
                               radioButtons(inputId='CHILD_BREASTFED', label='CHILD_BREASTFED', c('N','Y'), selected = NULL, inline = FALSE,width = NULL)
                               
                               
                               
                        ),
                        
                        actionButton("plot", "Click!"), mainPanel(dataTableOutput("Pred"))
                        
                      )
                      
             )
             
             
             
  ))







#   mainPanel(

#    

#     # Output: Tabset w/ plot, summary, and table ----

#     tabsetPanel(type = "tabs",

#                 tabPanel("County Map",

#                          selectInput('yr', 'Year', unique(county_test$year),

#                                                    selected =unique(county_test$year)[[2]]),

#                          plotOutput('plot1', height=700,width = 600)),

#                 tabPanel("Deep Dive Graphs",

#                          fluidRow(

#                            plotOutput("plot2"),

#                            plotOutput("plot3"),

#                            plotOutput("plot4")

#                            )

#                         

#                          ),

#                 tabPanel("Proability Table", tableOutput("table"))

#

#   )

# ))



server <- function(input, output) {
  
  library(dplyr)
  
  library(choroplethr)
  
  library(choroplethrMaps)
  
  library(ggplot2)
  
  
  
  
  
  selectedData <- reactive({
    
    
    
    county_test[county_test$year == input$yr, ]
    
  })
  
  
  
  output$plot1 <- renderPlot({
    
    county_choropleth(selectedData(), legend = "value",
                      
                      state_zoom = "indiana")
    
    
    
  })
  
  
  
  output$plot2 <- renderPlot({
    
    
    
    ggplot(data=b, aes(x=reorder(b$UNDERLYING_COD,b$count),y=b$count))+geom_bar(stat="identity")+coord_flip()+labs(title = "Top 10 Causes of Infant Death", x= ("Count"),y=("Cause of Death"))
    
    
    
  })
  
  
  
  output$plot3 <- renderPlot({
    
    
    
    ggplot(data=ob, aes(x=reorder(ob$County.Name,-ob$Providers.Count), y=ob$Providers.Count)) +
      
      geom_bar(stat="identity")+labs(title = "Top 10 Counties with OBGYN Provides", x= ("County"),y=("Number of OBGYN Providers"))
    
    
    
  })
  
  
  
  output$plot4 <- renderPlot({
    
    
    
    ggplot(data=d, aes(x=reorder(d$COUNTY,-d$X2018),y=d$X2018))+geom_bar(stat="identity")+labs(title = "Distribution of Obese Women by County", x= ("County"),y=("Count of Obese Women"))
    
    
    
  })
  
  
  
  data <- eventReactive(input$plot,{
    
    # req(input$MOTHER_AGE_GRP)
    
    as.h2o(data.frame(NUM_BIRTHS_BY_MOTHER=input$NUM_BIRTHS_BY_MOTHER,
                      
                      CHILD_BIRTH_YR_GRP=input$CHILD_BIRTH_YR_GRP,
                      
                      MOTHER_AGE_GRP=input$MOTHER_AGE_GRP,
                      
                      MOTHER_MARITALSTATUS_AT_BIRTH=input$MOTHER_MARITALSTATUS_AT_BIRTH,
                      
                      MOTHER_RESID_COUNTY_TYPE=input$MOTHER_RESID_COUNTY_TYPE,
                      
                      BIRTH_COMPLICATIONS=input$BIRTH_COMPLICATIONS,
                      
                      DIABETES_RISK_PREPREGNANCY=input$DIABETES_RISK_PREPREGNANCY,
                      
                      DIABETES_RISK_GESTATIONAL=input$DIABETES_RISK_GESTATIONAL,
                      
                      SMOKING_IND=input$SMOKING_IND,
                      
                      SMOKING_DURING_PREG_IND=input$SMOKING_DURING_PREG_IND,
                      
                      SMOKING_BEFORE_PREG_IND=input$SMOKING_BEFORE_PREG_IND,
                      
                      VISITS_IN_1ST_TRIMESTER_IND=input$VISITS_IN_1ST_TRIMESTER_IND,
                      
                      CHILD_GENDER=input$CHILD_GENDER,
                      
                      CHILD_TRANSFERRED=input$CHILD_TRANSFERRED,
                      
                      CHILD_ALIVE=input$CHILD_ALIVE,
                      
                      LOW_BIRTH_WEIGHT=input$LOW_BIRTH_WEIGHT,
                      
                      CHILD_BREASTFED=input$CHILD_BREASTFED))
    
    # weight=input$weight,
    
    # cholesterol=input$cholesterol)
    
  })
  
  
  
  pred <- eventReactive(input$plot,{
    
    h2o.predict(gbm,data(),type="response")
    
  })
  
  
  
  output$Pred <- renderDataTable(pred())
  
  
  
}



shinyApp(ui = ui, server = server)