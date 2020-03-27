

###-----------------------
### Libraries
###-----------------------


library(shinythemes)
library(stringr)
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(RColorBrewer)
library(maps)
library(htmltools)



###-----------------------
### Data
###-----------------------


COVID_19_t_series <-read.csv("data/COVID_19_US_TIME_SERIES_app.csv")
STATES_LAT_LNG<- read.csv("data/States_lat_lng_app.csv")
RECENT_CASES <- read.csv("data/RECENT_CASES_app.csv")
STATE_VARIABLES <- read.csv("data/POPULATION_DENSITY_app.csv")


# update this daily
dates <- seq(as.Date("2020-1-22"), as.Date("2020-03-26"), by="days")


COVID_19_t_series_dates <- COVID_19_t_series

COVID_19_t_series_dates$dates <- dates

COVID_19_t_series_dates_march <- subset(COVID_19_t_series_dates, dates > as.Date("2020-03-01"))
COVID_19_t_series_dates_march9 <- subset(COVID_19_t_series_dates, dates > as.Date("2020-03-09"))

# this needs to be updated daily
COVID_19_t_series_dates_most_recent <- subset(COVID_19_t_series_dates, dates > as.Date("2020-03-18"))







###-----------------------
### plotting time series
###-----------------------




library(ggplot2)
plot_time_series <- function(state){
  p<-ggplot(data=COVID_19_t_series_dates_march9, aes_string(x="dates", y=state)) +
    geom_bar(stat="identity", fill = "red")+
    xlab("Date (2020)") + ylab("Cases")+
    theme(plot.title = element_text(size=50))+
    ggtitle(paste("Covid 19 Cases in", state, "Starting March 2020")) +
    theme(plot.title = element_text(size = 20, face = "bold"))
  return(p)
}
plot_log_time_series <- function(state){
  p<-ggplot(data=COVID_19_t_series_dates_march, aes_string(x="dates", y = sprintf("log(%s+1) ", state))) +
    geom_bar(stat="identity", fill = "red")+
    xlab("Date (2020)") + ylab("Log(Cases)")+
    theme(plot.title = element_text(size=50))+
    ggtitle(paste("Covid Cases in:", state)) +
    theme(plot.title = element_text(size = 15, face = "bold"))


  return(p)
}
plot_time_series("Washington")




###--------------------
### Data set for map
###--------------------




doubling_time <- function(state){
  mod <- glm(COVID_19_t_series_dates_march9[[state]] ~ as.numeric(COVID_19_t_series_dates_march9$dates), family = "poisson")
  #return(log(2)/log(1+lmodel$coefficients[2]))
  return(log(2)/(mod$coefficients[2]))
}
doubling_time_five_most_recent <- function(state){
  mod <- glm(COVID_19_t_series_dates_most_recent[[state]] ~ as.numeric(COVID_19_t_series_dates_most_recent$dates), family = "poisson")
  return(log(2)/(mod$coefficients[2]))
}
R_naught <- function(state){
  mod <- glm(COVID_19_t_series_dates_march9[[state]] ~ as.numeric(COVID_19_t_series_dates_march9$dates), family = "poisson")
  
  return(exp(mod$coefficients[2]))
  #return(exp(lmodel$coefficients[2]))
}
R_naught_five_most_recent <- function(state){
  mod <- glm(COVID_19_t_series_dates_most_recent[[state]] ~ as.numeric(COVID_19_t_series_dates_most_recent$dates), family = "poisson")
  
  return(exp(mod$coefficients[2]))
  #lmodel <- lm(log(COVID_19_t_series_dates_most_recent[[state]]+0.1) ~ COVID_19_t_series_dates_most_recent$dates)
  #return(exp(lmodel$coefficients[2]))
}
R_naught("Texas")
R_naught_five_most_recent("Texas")

R0_ls <- c()
R0_recent_ls <- c()
doubling_time_ls <- c()
doubling_time_recent_ls <- c()
for(name in names(COVID_19_t_series_dates_march9)){
  if( name == "dates"){}else{if(name == "US"){}else{
    R0_ls <- append(R0_ls,R_naught(name)[[1]])
    R0_recent_ls <- append(R0_recent_ls,R_naught_five_most_recent(name)[[1]])
    doubling_time_ls <- append(doubling_time_ls,doubling_time(name)[[1]])
    doubling_time_recent_ls <- append(doubling_time_recent_ls,doubling_time_five_most_recent(name)[[1]])
  }}
}


Crossection_COVID_19_GROWTH <- data.frame(
  lat = STATES_LAT_LNG$lat,
  lng = STATES_LAT_LNG$lng,
  state = STATES_LAT_LNG$state,
  R0 = 100*(R0_ls-1),
  doubling = doubling_time_ls,
  R0_recent = 100*(R0_recent_ls-1),
  doubling_recnet = doubling_time_recent_ls,
  cases = RECENT_CASES$Cases,
  log_cases = log(RECENT_CASES$Cases)
)

states_ls <- as.character(STATES_LAT_LNG$state)
i = 0
for( el in states_ls){
  i = i + 1
  states_ls[i] <- str_replace(states_ls[i] , " ", ".")
}

states_ls  <- append(states_ls, "US")



###----------------------------------
### Crossection data
###----------------------------------


###----------------------------------
### Days since 25 cases
###----------------------------------

days_since_25_css <- c()
l <- length(COVID_19_t_series_dates_march)
i = 0
nms <- c()
cases <- c()
for(el in names(COVID_19_t_series_dates_march[c(3:length(COVID_19_t_series_dates_march)-1)])){
  i = i + 1

  nms <- append(nms, el)
  days_since_25_css <- append(days_since_25_css,
                              sum(COVID_19_t_series_dates_march[[el]]>25))
  if(sum(COVID_19_t_series_dates_march[[el]]>25) == 0){
    days_since_25_css[i] = 0.1
  }
}

###--------------------------
### Some more data set stuff (names)
###--------------------------

Crossection_COVID_19_GROWTH_nms <- data.frame(
  #lat = STATES_LAT_LNG$lat,
  #lng = STATES_LAT_LNG$lng,
  #state = STATES_LAT_LNG$state,
  Growth_Rate = 100*(R0_ls-1),
  Doubling_time = doubling_time_ls,
  Growth_Rate_past_five_days = 100*(R0_recent_ls-1),
  Doubling_time_past_five_days = doubling_time_recent_ls,
  Number_of_cases = RECENT_CASES$Cases,
  Number_of_cases_log_scale = log(RECENT_CASES$Cases)
)



Crossection_COVID_19 <- data.frame(state = nms, Days_after_first_25_cases_identified =days_since_25_css )




###----------------------------------
### Cases and growth rate
###----------------------------------
Crossection_COVID_19 <- cbind(Crossection_COVID_19, Number_of_cases = Crossection_COVID_19_GROWTH$cases,
                              Growth_rate = 100*(Crossection_COVID_19_GROWTH$R0 -1),
                              Doubling_Time = Crossection_COVID_19_GROWTH$doubling,
                              Number_of_cases_log_scale =Crossection_COVID_19_GROWTH$log_cases )


###----------------------------------
### Recemnt and growth rate
###----------------------------------
Crossection_COVID_19 <- cbind(Crossection_COVID_19, R0_recent = Crossection_COVID_19_GROWTH$R0_recent
                              , doubling_recent = Crossection_COVID_19_GROWTH$doubling_recnet)




###----------------------------------
### More Stuff
###----------------------------------




# chagne names to be compatible
nms <- c()
i = 0
for( i in 1:length(STATE_VARIABLES$state)){
  nms <- append(nms,str_replace(STATE_VARIABLES$state[i] , " ", "."))
}


STATE_VARIABLES$state <- nms# merge data sets

Crossection_COVID_19 <- merge(Crossection_COVID_19, STATE_VARIABLES, "state")

###-----------------------
### Utilities
###-----------------------

# Map dots

X_max <- 37800 
print(X_max)
a <- 5
b <- 0.002
off_set <- 10000.0
linear_dot_scale_cases <- function(x){ return(a + b*x) }
linear_dot_scale_cases_col <- function(x){ return(5*log(x)) }
linear_dot_scale_cases_col <- function(x){10+0.0015*x}
c <- 1
d <- 0.25
R0_off_set <- 5


R0_max <- 58

linear_dot_scale_R0 <- function(x){ return(c+d*x) }

linear_dot_scale_R0_col <- function(x){ return(R0_off_set*c + x*(d*R0_max - (R0_off_set-1)*c)/R0_max) }

linear_dot_scale_R0_col(50)
  

###-----------------------
### App
###-----------------------



###-----------------------
### Page layout
###-----------------------


ui <- fluidPage(theme = shinytheme("yeti"),
#fluidPage(
  # title
  titlePanel(
     h1("Covid 19 Cases in the US ", style = {'font'})
    
    ),


  tabsetPanel(

    tabPanel(
      "Map",

      fluidRow(
        column(4,
               helpText("
                        This interactive map tracks the number of cases of COVID-19 and
                        rate at which cases are increasing throught the United States. Select
                        a state to see the number of current cases, growth rate of cases and 
                        projected time for the number of cases to double. The size and color of 
                        the labels can be adjusted to visualize the growth rate and/ or number
                        of current cases. Darker dots represent faster growth rates. 
                        "),
               helpText("
                        Growth of the disease is close to exponential in all states leading to
                        rapid increases in the number of cases. The growth rate is given as a 
                        a percentage increase per day. For example, if a state had a growth rate of
                        100% the number of cases would double every day. The fastest rates of growth 
                        currently are in Mississippi and West Viriginia at 50% and 54%.
                        "),
               helpText("
                        The data presented here is from The Johns Hoptkins Covid research program and can be
                        accessses on git hub at: https://github.com/CSSEGISandData/COVID-19
                        " ),
               selectInput("size",
                           label = "Label size",
                           choices = c("Cases",
                                       "Growth"),
                           selected = "Cases"),
               selectInput("color",
                           label = "Label coloring",
                           choices = c("Cases",
                                       "Growth",
                                       "Red"),
                           selected = "Growth")



               ),

        column(7,
               leafletOutput(outputId = "mymap", height = 600)
        )
      )
    ),







    tabPanel("History of Cases By State", fluidRow(
      column(4,
             helpText("
This shows how the cumulative number of cases reported in each state has changed over time.
The history of cases can be filtered by state or shown as a total history of cases in United States.
Data for this map is from the John Hopkins Coronavirus Research Center and is updated daily.
This data can also be accessed through https://github.com/CSSEGISandData/COVID-19.
                      "),

             selectInput("state",
                         label = "State",
                         choices = states_ls,
                         selected = "US")
      ),

      column(6,
             plotOutput(outputId ="plot1", height = 600)

      )


    )

    ),







    tabPanel("Summary Statistics",fluidRow(
      column(4,
             helpText("
                      This demonstrates the differences in growth rates, caseloads, and doubling times between states to
                      show the national variability in disease spread and burden. Users can select the type of data to
                      display. Data from all states are compiled and represented on histograms."),
             helpText("
                      For example, if doubling time is selected, all the observed doubling times are shown on the x-axis
                      and the number of states with each doubling time is show on the y-axis. This allows you to see both
                      the most common doubling times and states with extremely high or low doubling times.
                      "),

             selectInput("var",
                         label = "histogram",
                         choices = c(names(Crossection_COVID_19_GROWTH_nms)),
                         selected = "Growth_Rate")

      ),
      column(6,
             plotOutput(outputId ="plot2", height = 600)

      )

    )
    ),



    tabPanel("Exploring Related Factors",fluidRow(

      column(4,

             helpText("
                      This helps users explore possible factors that may be contributing to disease burden, spread, growth rate, or spread. Users may select variables for both the x and y axis to visualize if they are correlated. Variables include growth rate, doubling time, case load, average temperature in a state, population of a state, etc. 
                      "),
             selectInput("x_var",
                         label = "X Variable",
                         choices = c(names(Crossection_COVID_19)),
                         selected = "Days_after_first_25_cases_identified"),


             selectInput("y_var",
                         label = "Y Variable",
                         choices = c(names(Crossection_COVID_19)),
                        selected = "Number_of_cases_log_scale")

             
      ),

      column(6,
             plotOutput(outputId ="plot3", height = 600)
      )


    )
    ))



  # row with map and control



  # row time series and histograms



  #    mainPanel(
  #      leafletOutput(outputId = "mymap"))),
  #
  #      fluidRow(column(3,
  #                      plotOutput(outputId ="plot1")),
  #               column(3,
  #                      plotOutput(outputId ="plot1"))
  #               )
  #)
)



###-----------------------
### Server function
###-----------------------



server <- function(input, output) {

  
###----------------
### Popups
###----------------
  
  
  content = paste("<font size='4'> <b>",as.character(STATES_LAT_LNG$state),"</b> </font>","</br>",
                  "<b>Cases:</b>",RECENT_CASES$Cases, "</br>",
                  "<b>Growth rate:</b>", round(Crossection_COVID_19_GROWTH$R0,1),
                  "%  per day", "</br>",
                  "<b>Doubling time:</b>", round(Crossection_COVID_19_GROWTH$doubling, 1)," days")


###---------
### map
###--------

  pal <- colorNumeric(
    palette = "Reds", domain = log(Crossection_COVID_19_GROWTH$cases)+1)



  output$mymap <- renderLeaflet({
    leaflet(Crossection_COVID_19_GROWTH) %>%
      setView(lng = -99, lat = 45, zoom = 2) %>% addTiles() %>%
      addCircleMarkers( radius = ~linear_dot_scale_cases(cases), 
                        color = ~pal(R0_recent+0.1),stroke = FALSE,fillOpacity = 0.5, group = "pnt") %>%
      addCircleMarkers(radius = ~linear_dot_scale_cases(cases),
                       color = "lightgrey",stroke = FALSE, fillOpacity = 0.01, popup = content)
  })



###---------------------
### histogram/ box plots
###----------------------

  output$plot1 <- renderPlot({
    plot_time_series(input$state)
  })



  output$plot3 <- renderPlot({
    ggplot(Crossection_COVID_19, aes_string(x=input$x_var,y=input$y_var)) +
      geom_point(color="black", fill="grey")+
      xlab(input$x_var) + ylab(input$y_var)+
      theme(plot.title = element_text(size=10, face = "bold"))+
      ggtitle(paste("Scatter plot of", input$x_var, "and", input$y_var))
    #hist(Crossection_COVID_19_GROWTH[[input$var]])
  })



  output$plot2 <- renderPlot({
    ggplot(Crossection_COVID_19_GROWTH_nms , aes_string(x=input$var)) +
      geom_histogram(color="black", fill="grey")+
      xlab(input$var) + ylab("Frequency")+aes(xmin = 0)+
      theme(plot.title = element_text(size=17, face = "bold"))+
      ggtitle(paste("National distribution of", input$var))
    #hist(Crossection_COVID_19_GROWTH[[input$var]])
  })

  output$Text1 <- renderText(input$state)






  ###---------------------
  ### update map
  ###----------------------



  observe({
    proxy <- leafletProxy("mymap", data = Crossection_COVID_19_GROWTH)
    proxy %>% clearMarkers()
    if (input$size == "Growth" & input$color == "Growth") {

      
      
      
      # color
      
      
      
      pal <- colorNumeric(
        palette = "Reds", domain = linear_dot_scale_R0(Crossection_COVID_19_GROWTH$R0))

      
      
      ##-----------
      ## map output
      ##-----------
      
      
      
      
      proxy %>% addCircleMarkers( radius = ~linear_dot_scale_R0(R0), color = ~pal(linear_dot_scale_R0_col(R0)),stroke = FALSE,
                                  fillOpacity = 0.5, group = "pnt") %>%
        addCircleMarkers(radius = ~linear_dot_scale_R0(R0),
                         color = "black",fillOpacity = 0.01,stroke = FALSE, popup = content)}
    
    
    
    
    
    
    
    else{
      if(input$size == "Cases" & input$color == "Growth"){

        
        
        
        
        
        # color
        
        
        
        
        
        pal <- colorNumeric(
          palette = "Reds", domain = linear_dot_scale_R0(Crossection_COVID_19_GROWTH$R0))

        
        
        ##-----------
        ## map output
        ##-----------
        
        
        
        proxy %>% addCircleMarkers( radius = ~linear_dot_scale_cases(cases), 
                                    color = ~pal(linear_dot_scale_R0_col(R0)),stroke = FALSE,
                                    fillOpacity = 0.5, group = "pnt") %>%
          addCircleMarkers(radius = ~linear_dot_scale_cases(cases),
                           color = "black",fillOpacity = 0.01,
                           stroke = FALSE, popup = content)
      
        
        
        
        
        
        
    }else{
        if(input$size == "Cases" & input$color == "Cases"){


          
          
          # color
          
          
          pal <- colorNumeric(palette = "Reds", 
                              domain = linear_dot_scale_cases_col(Crossection_COVID_19_GROWTH$cases))



          ##-----------
          ## map output
          ##-----------
          
          
          
          
          proxy %>% addCircleMarkers( radius = ~linear_dot_scale_cases(cases), 
                                      color = ~pal(linear_dot_scale_cases_col(cases)),stroke = FALSE,
                                      fillOpacity = 0.5, group = "pnt") %>%
            addCircleMarkers(radius = ~linear_dot_scale_cases(cases),
                             fillOpacity = 0.01,
                             color = "black",stroke = FALSE, popup = content)


          
          
          
          
        }else{if(input$size == "Growth" & input$color == "Cases"){


          # color
          
          
          pal <- colorNumeric(
            palette = "Reds", domain = linear_dot_scale_cases_col(Crossection_COVID_19_GROWTH$cases ))


          
          ##-----------
          ## map output
          ##-----------
          
          

          proxy %>% addCircleMarkers( radius = ~linear_dot_scale_R0(R0), color = ~pal(linear_dot_scale_cases_col(cases)),stroke = FALSE,
                                      fillOpacity = 0.5, group = "pnt") %>%
            addCircleMarkers(radius = ~linear_dot_scale_R0(R0),
                             color = "black",fillOpacity = 0.01,stroke = FALSE, popup = content)


          
          
          
          
        }else{if(input$size == "Growth" & input$color == "Red"){



          
          ##-----------
          ## map output
          ##-----------
          
          
          


          proxy %>% addCircleMarkers( radius = ~linear_dot_scale_R0(R0_recent), color = "red",stroke = FALSE,
                                      fillOpacity = 0.5, group = "pnt") %>%
            addCircleMarkers(radius = ~linear_dot_scale_R0(R0_recent),fillOpacity = 0.01,color = "black",stroke = FALSE, popup = content)

          
          
          
          
          

        }else{if(input$size == "Cases" & input$color == "Red"){


          
          ##-----------
          ## map output
          ##-----------
          
          
          
          proxy %>% addCircleMarkers( radius = ~linear_dot_scale_cases(cases), color = "red",stroke = FALSE,
                                      fillOpacity = 0.5, group = "pnt") %>%
            addCircleMarkers(radius = ~linear_dot_scale_cases(cases),color = "black",stroke = FALSE, 
                             fillOpacity = 0.01, popup = content)


          
          
          
          
        }

        }
        }
        }
      }
    }


  })






  output$selected_var <- renderText({
    paste(input$var)



  })



}

###-----------------------
### Launch
###-----------------------
app <- shinyApp(ui, server)


#rsconnect::setAccountInfo(name='jackbucknernrm',token='86D90A6E9237E6611A885F58DD020498',secret='LjeQ6BZtdFj0wqyMxZE02qflkViK/mc8PUasN7+U')
#rsconnect::deployApp('/Users/JohnBuckner/Documents/COVID_19_DATA_APP')




