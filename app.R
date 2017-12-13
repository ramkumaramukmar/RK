#read the csv file
x<-read.csv("states.csv",as.is = TRUE)
x

#percentage to numeric conversion
x$Uninsured.Rate..2010. <- as.numeric(sub("%","",x$Uninsured.Rate..2010.))/100 
x$Uninsured.Rate..2015. <- as.numeric(sub("%","",x$Uninsured.Rate..2015.))/100

x$Uninsured.Rate.Change..2010.2015.<-x$Uninsured.Rate..2015.-x$Uninsured.Rate..2010.

#manual calculation of uninsured rate 2020
x$a.2020<-x$Uninsured.Rate.Change..2010.2015.+x$Uninsured.Rate..2015.

#manual average population calculation
x$population<-x$Health.Insurance.Coverage.Change..2010.2015./x$Uninsured.Rate.Change..2010.2015.*-1

x$Average.Monthly.Tax.Credit..2016.<-as.numeric(sub("\\$","",x$Average.Monthly.Tax.Credit..2016.))

#linear modelling and prediction of uninsured rate 2020
ml<-lm(x$a.2020~x$Uninsured.Rate..2015.)
ml

summary(ml)
newdata<-data.frame(x$Uninsured.Rate..2015.)
x$newd<-predict(ml,newdata)
for(i in 1:52){
  if(x$newd[i]<0){
    x$newd[i]<--1*x$newd[i]
  }else{
    x$newd[i]<-x$newd[i]*100
  }
}


x$Uninsured.Rate..2010.<-100*x$Uninsured.Rate..2010.
x$Uninsured.Rate..2015.<-100*x$Uninsured.Rate..2015.

#library(reshape2)
#library(ggplot2)
#df<-data.frame(x$State,x$Uninsured.Rate..2010.,x$Uninsured.Rate..2015.,x$newd)
#df
#df.long<-melt(df)
#ggplot(df.long,aes(x.State,value,fill=variable))+
# geom_bar(stat="identity",position="dodge")+
#theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


# shiny app ---------------------------------------------------------------


library(shiny)

library(reshape2)
library(ggplot2)
df<-data.frame(x$State,x$Uninsured.Rate..2010.,x$Uninsured.Rate..2015.,x$newd)
df
df.long<-melt(df)

# ui code -----------------------------------------------------------------



ui<-fluidPage(
  h1("    health insurence dataset"),
  img(height=100,width=100,source="C:\\Users\\Lenovo\\Desktop\\ELEMENTS\\www"),
  sidebarLayout(
    sidebarPanel(
      selectInput("statename","Choose a state",choices =x$State),width=3
    ),
    mainPanel(
      tableOutput("y"),
      plotOutput("q")
    )
  ))



# servere code ------------------------------------------------------------



server<-shinyServer(function(input,output){
  
  output$y<-renderTable(
    sf<-subset(x,x$State==input$statename)
  )
  output$q<-renderPlot(
    
    ggplot(df.long,aes(x.State==input$statename,value,fill=variable))+
      geom_bar(stat="identity",position="dodge")+
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
    
    
  )
  
})
shinyApp(ui=ui,server=server)