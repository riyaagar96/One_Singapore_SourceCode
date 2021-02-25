library(tidyverse)
library(shiny)
library(shinydashboard)
library(plyr)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(highcharter)
library(reshape2)
library(waffle)
library(RColorBrewer)
library(plotly)

tracker_2020 <- read_csv("fund_tracker_2020.csv")
tracker_2019 <- read_csv("Fund_Tracker_2019.csv")
dem_2020 <- read_csv("One Singapore_Demographic Data_2020.csv")
dem_2020 <- dem_2020[!is.na(dem_2020$`Client ID`), ]
food_2020 <- read_csv("food_2020.csv")

#removes any rows where "ref no." column contains new funds
tracker_2020 <- tracker_2020[!grepl("New", tracker_2020$`Ref no.`),]
tracker_2020 <- tracker_2020[!is.na(tracker_2020$`Ref no.`),]
tracker_2020 <- tracker_2020 %>%
  transform(`Cost of Grant` = as.numeric(`Cost of Grant`)) %>%
  transform(Total.Days = as.numeric(Total.Days))

#any missing dates will be filled using the previous date so all applications have dates. 
#then convert dates from string to datetime objects and extract month. 
tracker_2020[tracker_2020 == "-"] <- NA
food_2020[food_2020 == "-"] <- NA

tracker_2020 <- tracker_2020 %>%
  fill(Date.of.Application)

food_2020 <- food_2020 %>%
  fill(`Delivery Date`)

tracker_2020 <- tracker_2020 %>%
  group_by(tracker_2020$Ref.no.) %>% 
  mutate(Date.of.Application = na.locf(Date.of.Application, na.rm = F)) %>%
  mutate(month_application = month(as.Date(Date.of.Application, "%d-%b")))

food_2020 <- food_2020 %>%
  group_by(food_2020$Ref.no.) %>% 
  mutate(month_application = month(as.Date(`Delivery Date`, "%d-%b")))

mymonths <- c("January","February","March","April","May","June","July","August","September","October","November","December")
mymonths = factor(mymonths, levels = month.name)
tracker_2020 <- tracker_2020 %>%
  mutate(month_application = mymonths[month_application])

#Separating the completely approved applications 
approvals <- tracker_2020 %>%
  filter(Approval == "Yes")

partial_approvals <- tracker_2020 %>%
  filter(Approval == "Partial")

rejections <- tracker_2020 %>%
  filter(Approval == "No")

#grouping by type of application
typeof_apps <- as.data.frame(count(tracker_2020, "Type.of.Request")) 
fresh_food <- data.frame("Fresh Food", nrow(food_2020))
names(fresh_food) <- c("Type.of.Request", "freq")
typeof_apps <- rbind(typeof_apps, fresh_food) %>%
  mutate(drilldown = tolower(Type.of.Request))

#separating type of requests out for the drilldown 
financial <- tracker_2020 %>%
  filter(Type.of.Request == "Financial") %>%
  select(Type.of.Request:Transportation)
financial <- data.frame(colSums(Filter(is.numeric, financial), na.rm = T))
financial <- cbind(Type = rownames(financial), financial)
rownames(financial) <- 1:nrow(financial)
colnames(financial) <- c("Type.of.Request", "freq")

in_kind <- tracker_2020 %>%
  filter(Type.of.Request == "In Kind") %>%
  select(Type.of.Request:Transportation)
in_kind <- data.frame(colSums(Filter(is.numeric, in_kind), na.rm = T))
in_kind <- cbind(Type = rownames(in_kind), in_kind)
rownames(in_kind) <- 1:nrow(in_kind)
colnames(in_kind) <- c("Type.of.Request", "freq")

#Applications per month (for sparklines in valueboxes)
monthly_apps <- as.data.frame(count(tracker_2020, "month_application"))
monthly_provals <- as.data.frame(count(approvals, "month_application"))
monthly_partials <- as.data.frame(count(partial_approvals, "month_application"))
monthly_rejections <- as.data.frame(count(rejections, "month_application"))

monthly_full_provals <- full_join(monthly_provals, monthly_partials, by = "month_application")
monthly_full_provals[is.na(monthly_full_provals)] <- 0
monthly_full_provals <- monthly_full_provals %>%
  mutate(freq = freq.x + freq.y) %>%
  select(month_application, freq)

#types of requests per month
monthly_fin <- tracker_2020 %>%
  filter(Type.of.Request == "Financial")
monthly_fin <- as.data.frame(count(monthly_fin, "month_application"))

fin_cost <- tracker_2020 %>%
  filter(Type.of.Request == "Financial") %>%
  group_by(month_application) %>%
  dplyr::summarize(mean = round(mean(Cost.of.Grant, na.rm=TRUE),2))

apps_cost <- approvals %>%
  group_by(month_application) %>%
  dplyr::summarize(mean = round(mean(Cost.of.Grant, na.rm=TRUE),2))

monthly_kind <- tracker_2020 %>%
  filter(Type.of.Request == "In Kind")
monthly_kind <- as.data.frame(count(monthly_kind, "month_application"))

kind_cost <- tracker_2020 %>%
  filter(Type.of.Request == "In Kind") %>%
  group_by(month_application) %>%
  dplyr::summarize(mean = round(mean(Cost.of.Grant, na.rm=TRUE),2))

partial_cost <- partial_approvals %>%
  group_by(month_application) %>%
  dplyr::summarize(mean = round(mean(Cost.of.Grant, na.rm=TRUE),2))

monthly_both <- tracker_2020 %>%
  filter(Type.of.Request == "Both")
monthly_both <- as.data.frame(count(monthly_both, "month_application"))

both_cost <- tracker_2020 %>%
  filter(Type.of.Request == "Both") %>%
  group_by(month_application) %>%
  dplyr::summarize(mean = round(mean(Cost.of.Grant, na.rm=TRUE),2))

rejects_cost <- rejections %>%
  group_by(month_application) %>%
  dplyr::summarize(mean = round(mean(Cost.of.Grant, na.rm=TRUE),2))

# monthly_food <- tracker_2020 %>%
#   filter(Type.of.Request == "Fresh Food")
monthly_food <- as.data.frame(count(food_2020, "month_application"))
monthly_food <- monthly_food %>%  
  mutate(month_application = as.factor(month_application))

# food_cost <- tracker_2020 %>%
#   filter(Type.of.Request == "Fresh Food") %>%
#   group_by(month_application) %>%
#   dplyr::summarize(mean = round(mean(Cost.of.Grant, na.rm=TRUE),2))

all_reqs <- full_join(monthly_fin, monthly_kind, by = "month_application")
all_reqs <- full_join(all_reqs, monthly_food, by = "month_application")
all_reqs <- full_join(all_reqs, monthly_both, by = "month_application")

# reqs_cost <- full_join(fin_cost, kind_cost, by = "month_application")
# reqs_cost <- full_join(reqs_cost, food_cost, by = "month_application")
# reqs_cost <- full_join(reqs_cost, both_cost, by = "month_application")

apps_avg_cost <- full_join(apps_cost, partial_cost, by = "month_application")

all_reqs <- all_reqs %>%
  mutate(Financial = freq.x,
         In_Kind = freq.y,
         Fresh_Food = freq.x.x,
         Both = freq.y.y)

# reqs_cost <- reqs_cost %>%
#   mutate(Financial = mean.x,
#          In_Kind = mean.y,
#          Fresh_Food = mean.x.x,
#          Both = mean.y.y)

apps_avg_cost <- apps_avg_cost %>%
  mutate(Approved = mean.x,
         Partials = mean.y)

all_reqs <- all_reqs %>%
  select(month_application, Financial, In_Kind, Fresh_Food, Both)
all_reqs[is.na(all_reqs)] <- 0
all_reqs_melt <- melt(all_reqs, id = "month_application")

# reqs_cost <- reqs_cost %>%
#   select(month_application, Financial, In_Kind, Fresh_Food, Both)
# reqs_cost[is.na(reqs_cost)] <- 0
# reqs_cost_melt <- melt(reqs_cost, id = "month_application")

apps_avg_cost <- apps_avg_cost %>%
  select(month_application, Approved, Partials)
apps_avg_cost[is.na(apps_avg_cost)] <- 0
apps_avg_cost_melt <- melt(apps_avg_cost, id = "month_application")

#creates the dataset for the streamgraph showing approvals/rejections per month
all_apps <- monthly_apps %>%
  mutate(Total = freq,
         Approved = monthly_provals$freq)

all_apps <- full_join(all_apps, monthly_partials, by = "month_application")
all_apps <- full_join(all_apps, monthly_rejections, by = "month_application")
all_apps <- all_apps %>%
  mutate(Partials = freq.y,
         Rejections = freq) 
  
all_apps[is.na(all_apps)] <- 0
all_apps <- all_apps %>%
  mutate(Others = Total - Approved - Partials - Rejections) %>%
  select(month_application, Approved, Partials, Rejections, Others)

all_apps_melt <- melt(all_apps, id = "month_application")

monthly_cost <- tracker_2020 %>%
  group_by(month_application) %>%
  dplyr::summarize(mean = round(mean(Cost.of.Grant, na.rm=TRUE),2))

monthly_total_spend <- tracker_2020 %>%
  group_by(month_application) %>%
  dplyr::summarize(sum = round(sum(Cost.of.Grant, na.rm=TRUE),2))

monthly_time <- tracker_2020 %>%
  group_by(month_application) %>%
  dplyr::summarize(mean = round(mean(Total.Days, na.rm=TRUE),2))

# monthly_total_spend <- monthly_total_spend %>%
#   mutate(sum = prettyNum(monthly_total_spend$sum, , big.mark=",",scientific=FALSE))

#variable that will be displayed as text on the dashboard
total_requests <- nrow(tracker_2020)
total_approvals <- nrow(approvals) + nrow(partial_approvals)
average_cost <- round(mean(tracker_2020$Cost.of.Grant, na.rm = TRUE),2)
average_time <- round(mean(tracker_2020$Total.Days, na.rm = TRUE),0)
total_paid <- prettyNum(as.numeric(sum(tracker_2020$Cost.of.Grant, na.rm = T)), big.mark=",",scientific=FALSE)

#add "title" parameter to the valuebox function
valueBox3 <- function(value, title, sparkobj = NULL, subtitle, icon = NULL,
                      color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

#creating themes for the sparklines 
hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "5px white")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "black",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            # list(0.50, "#FFFFFF7F"),
            list(1.00, "grey")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}

#creating sparklines for valueboxes 
hc_applications <- hchart(monthly_apps, 
                          "area", 
                          hcaes(month_application, freq), 
                          name = "total applications", 
                          color = "black",
                          ylim = 50)  %>% 
  hc_size(height = 100) %>%
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE) %>%
  hc_chart(backgroundColor = "transparent")

hc_approvals <- hchart(monthly_full_provals, 
                       "area", 
                       hcaes(month_application, freq), 
                       name = "total applications", 
                       color = "black",
                       ylim = 50)  %>% 
  hc_size(height = 100) %>%
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE) %>%
  hc_chart(backgroundColor = "transparent")

hc_cost <- hchart(monthly_cost, 
                  "column", 
                  hcaes(month_application, mean), 
                  name = "Average Cost", 
                  color = "grey",
                  ylim = 50)  %>% 
  hc_size(height = 100) %>%
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE) %>%
  hc_chart(backgroundColor = "transparent")

hc_time <- hchart(monthly_total_spend, 
                  "column", 
                  hcaes(month_application, sum), 
                  name = "Total Spend", 
                  color = "grey",
                  ylim = 50)  %>% 
  hc_size(height = 100) %>%
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE) %>%
  hc_chart(backgroundColor = "transparent")

#creating a tree map using highcharter
tree_colors <- c("#7294D4","#D6B3C8", "#E5EAF6","#A886B5")
#cols <- brewer.pal(4, "Set1")
# typeof_apps <- typeof_apps %>%
#   mutate(color = tree_colors)

# hc_typeof_apps <- typeof_apps %>%
#   hchart(
#     "treemap", 
#     hcaes(x = Type.of.Request, value = freq, color = freq),
#     # color = typeof_apps$color
#     ) %>%
#   hc_legend(enabled = F)
#   hc_colors(hex_to_rgba("#D6B3C8"))

#drilldown treemap code, saving for later
hc_typeof_apps <- highchart() %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(
    series = list(
      boderWidth = 0,
      dataLabels = list(enabled = TRUE)
      )
    ) %>%
  hc_colorAxis(stops = color_stops(n=length(tree_colors), colors = tree_colors)) %>%
  hc_add_series(
    data = typeof_apps,
    type = "treemap",
    hcaes(x = Type.of.Request, value = freq, color = freq),
    name = "Types of Requests",
    colorByPoint = FALSE,
    dataLabels = list(color = "white"),
    levels = list(
      list(
        level = 1,
        borderWidth = 1,
        dataLabels = list(
          enabled = TRUE,
          verticalAlign = "top",
          align = "left",
          style = list(fontSize = "14px", textOutline = FALSE),
          color = "white"
        )
      )
    )
  )

hc_typeof_apps <- hc_typeof_apps %>%
  hc_drilldown(
    allowDrillToNode = TRUE,
    levelIsConstant = FALSE,
    series = list(
      list(
        id = "financial",
        data = list_parse2(financial),
        type = "treemap"
        ),
      list(
        id = "in kind",
        data = list_parse2(in_kind),
        type = "treemap"
        )
      )
    )

#Creating the stream graphs to compare monthly applications etc.
monthly_cols <- c("slategray2","gray85", "#D6B3C8", "lightsalmon2") #"#7294D4")

hc_monthly_apps <- hchart(all_apps_melt, 
                          "streamgraph", 
                          hcaes(x = month_application, y = value, group = variable, opacity = 0.5)) %>%
  hc_colors(hex_to_rgba(monthly_cols)) %>%
  hc_yAxis(visible = FALSE, startOnTick = TRUE, endOnTick = TRUE) %>%
  hc_tooltip(table = TRUE, outside = TRUE) %>%
  hc_yAxis(title = list(text = "Total Applications")) %>%
  hc_xAxis(title = list(text = "Month"))

hc_monthly_reqs <- hchart(all_reqs_melt, 
                          "streamgraph", 
                          hcaes(x = month_application, y = value, group = variable, opacity = 0.5)) %>%
  hc_colors(hex_to_rgba(monthly_cols)) %>%
  hc_yAxis(visible = FALSE, startOnTick = TRUE, endOnTick = TRUE) %>%
  hc_tooltip(table = TRUE, outside = TRUE) %>%
  hc_yAxis(title = list(text = "Total Applications")) %>%
  hc_xAxis(title = list(text = "Month"))


#hc_reqs_cost <- hchart(reqs_cost_melt, 
#                          "column", 
#                          hcaes(x = month_application, y = value, group = variable)) %>%
#  hc_colors(hex_to_rgba(monthly_cols)) %>%
#  hc_tooltip(table = TRUE, outside = TRUE) %>%
#  hc_plotOptions(column = list(pointWidth = 15)) 

hc_apps_avg_cost <- hchart(apps_avg_cost_melt, 
                       "column", 
                       hcaes(x = month_application, y = value, group = variable)) %>%
  hc_colors(hex_to_rgba(c("slategray2", "lightsalmon2"))) %>%
  hc_tooltip(table = TRUE, outside = TRUE) %>%
  hc_plotOptions(column = list(pointWidth = 20)) %>%
  hc_yAxis(title = list(text = "Average Cost ($)")) %>%
  hc_xAxis(title = list(text = "Month"))

#creating value boxes for key statistics
vb_req <- valueBox3(
  value = total_requests,
  title = toupper("Total Applications"),
  sparkobj = hc_applications,
  subtitle = "Up 527% from last year",
  icon = icon("user-group"),
  width = 3,
  color = "red",
  href = NULL
)

vb_approval <- valueBox3(
  value = total_approvals,
  title = toupper("Approved Applications"),
  sparkobj = hc_approvals,
  subtitle = "Up 488% from last year",
  width = 3,
  color = "red",
  href = NULL
)

vb_cost <- valueBox3(
  value = paste("SGD $", average_cost, sep=""),
  title = toupper("Average Cost of Grant"),
  sparkobj = hc_cost,
  subtitle = "Down 3.5% from last year",
  width = 3,
  color = "red",
  href = NULL
)

vb_time <- valueBox3(
  value = paste("SGD $", total_paid, sep=""),
  title = toupper("Total Amount Spent (2020)"),
  sparkobj = hc_time,
  subtitle = "Up 542% from last year",
  width = 3,
  color = "red",
  href = NULL
)

income_wc <- c("$0"=133,"$1-$500"=35,"$501-$1000"=49,"$1001-$2000"=65,"$2001-$3000"=17,"$3000+"=4)
names(income_wc) = paste0(names(income_wc)," (",as.integer(income_wc/sum(income_wc)*100),"%)")
dem_income_wc <- waffle(income_wc, rows = 8,
                        colors = brewer.pal(n = 6, name = "Purples")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12)) + 
  guides(fill = guide_legend(nrow = 1)) 

app_gender <- as.data.frame(count(dem_2020, "Gender"), na.rm = TRUE)
app_gender <- app_gender[!is.na(app_gender$Gender), ]
fig_gen <- app_gender %>%
  plot_ly(labels = ~Gender, values = ~freq, marker = list(colors = brewer.pal(n = 3, name = "BuPu"))) %>%
  add_pie(hole = 0.7) %>%
  layout(showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
  layout(plot_bgcolor='transparent') %>% 
  layout(paper_bgcolor='transparent')

app_unemp <- dem_2020 %>%
  filter(Occupation == "Unemployed")

app_emp <- dem_2020 %>%
  filter(Occupation != "Unemployed")
emp <- c("Unemployed", "Employed")
value_emp <- c(nrow(app_unemp), nrow(app_emp))
emp_break <- data.frame(emp, value_emp, stringsAsFactors = FALSE)

fig_emp <-emp_break %>%
  plot_ly(labels = ~emp, values = ~value_emp, marker = list(colors = brewer.pal(n = 2, name = "PuBu"))) %>%
  add_pie(hole = 0.7) %>%
  layout(showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
  layout(plot_bgcolor='transparent') %>% 
  layout(paper_bgcolor='transparent')

app_parent <- as.data.frame(count(dem_2020, "Single_Parent"), na.rm = TRUE)
app_parent <- app_parent[!is.na(app_parent$Single_Parent), ]
app_parent <- app_parent %>%
  mutate(Title = c("Co-parents", "Single Parents"))
fig_par <- app_parent %>%
  plot_ly(labels = ~Title, values = ~freq, marker = list(colors = brewer.pal(n = 2, name = "PuRd"))) %>%
  add_pie(hole = 0.7) %>%
  layout(showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
  layout(plot_bgcolor='transparent') %>% 
  layout(paper_bgcolor='transparent')

app_covid <- as.data.frame(count(dem_2020, "COVID"), na.rm = TRUE)
app_covid <- app_covid[!is.na(app_covid$COVID), ]
app_covid <- app_covid %>%
  mutate(Title = c("Not COVID-related", "COVID-related"))
fig_cov <- app_covid %>%
  plot_ly(labels = ~Title, values = ~freq, marker = list(colors = brewer.pal(n = 2, name = "BuPu"))) %>%
  add_pie(hole = 0.7) %>%
  layout(showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
  layout(plot_bgcolor='transparent') %>% 
  layout(paper_bgcolor='transparent')

fig_income <- hchart(
  dem_2020$Income) %>%
#  hc_tooltip(table = TRUE, outside = TRUE) %>%
  hc_colors(hex_to_rgba(c("slategray2"))) %>%
  hc_yAxis(title = list(text = "Number of Applicants")) %>%
  hc_xAxis(title = list(text = "Income ($)"))

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
    ),
    tags$div(class="h1-wrapper", align = "center",
      tags$div(class="logo-wrapper",
                     tags$h1("ONE (SINGAPORE)")
              ),
#      img(src='Logo.png', align = "right"),
      tags$p("Key statistics and information regarding applications, items requested, cost etc.", align = "center"),
      tags$img(src='Logo.png')
    ),
    fluidRow(class="top-row-shrink",
      valueBoxOutput("vbox_req", 3),
      valueBoxOutput("vbox_app", 3),
      valueBoxOutput("vbox_cost", 3),
      valueBoxOutput("vbox_time", 3)
    ),
    fluidRow(
      column(
        width = 12,
        column(width = 05, 
               tags$h4("Types of Aid Requested"),
               tags$p("Financial Assistance (40%) and in-kind assistance (24%) made up the majority of requests in 2020.
                      Applicants requested financial assistance for 255 separate items, and in-kind assistance for 153 separate items."),
               highchartOutput("treemp", height = "500px")),
        column(width = 04,
               selectInput("drop",
                           "Select Metrics:",
                           c("Approvals/Rejections",
                             "Types of Requests",
                             "Approvals/Partials - Average Spend")
               )),
        column(width = 07,
               tags$h4("Monthly Statistics"),
               tags$p("Use the button above to click between applications, cost, types of requests"),
               highchartOutput("stream", height = "500px"))
      )
    ),
    fluidRow(
      column(
        width = 12,
        column(width = 02,
               tags$h4("Gender Breakdown", align = "center"),
               plotlyOutput("donut_gen")),
        column(width = 02,
               tags$h4("Employment Breakdown", align = "center"),
               plotlyOutput("donut_emp")),
        column(width = 02,
               tags$h4("Single Parents", align = "center"),
               plotlyOutput("donut_par")),
        column(width = 02,
               tags$h4("COVID-related", align = "center"),
               plotlyOutput("donut_cov")),
        column(width = 04,
               tags$h4("Income Distribution"),
               highchartOutput("hist_inc"))
      )
    )
  )
)

server <- function(input, output) {
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  output$vbox_req <- renderValueBox(vb_req)
  output$vbox_app <- renderValueBox(vb_approval)
  output$vbox_cost <- renderValueBox(vb_cost)
  output$vbox_time <- renderValueBox(vb_time)
  output$treemp <- renderHighchart(hc_typeof_apps)
  output$stream <- renderHighchart({
    data <- hc_monthly_apps
    if (input$drop == "Types of Requests") {
      data <- hc_monthly_reqs
    }
    if (input$drop == "Approvals/Partials - Average Spend") {
      data <- hc_apps_avg_cost
    }
    data
  })
  output$donut_gen <- renderPlotly(fig_gen)
  output$donut_emp <- renderPlotly(fig_emp)
  output$donut_par <- renderPlotly(fig_par)
  output$donut_cov <- renderPlotly(fig_cov)
  output$hist_inc <- renderHighchart(fig_income)
}

shinyApp(ui = ui, server = server)