# This script runs the CI Underwriting Turnover Dashboard # 

#### Packages #### 
# library(shiny)
# library(shinydashboard)
# library(plotly)
# library(stringr)
# library(DT)
# library(tidyr)
# library(dplyr)
# library(reshape2)
# library(shinyWidgets)
# library(V8)
# library(shinyjs)

options(shiny.sanitize.errors = FALSE) # so we can see error messages!

# library packages
source('global.R', local=TRUE)

user_permissions <- read.csv("user_permissions.csv")

#### Global #### 

curr_quarter <- "2018-9"

scoredNI <- read.csv("ni_scored.csv")
high_risk_terms <- read.csv("high_risk_terms.csv")
curr_terms <- read.csv("terms.csv")
high_risk_terms <- rbind(high_risk_terms,curr_terms)

scoredNI <- scoredNI %>%
  mutate(
    # Remove text after dashes in office names
    office_name_new = as.character(substr(x_office_name,1,regexpr('-',x_office_name)-1)),
    office_name = ifelse(office_name_new == "", as.character(x_office_name), as.character(office_name_new)),
    
    # quarter field
    quarter.text = sub(pattern = "2018-6", "2018-Q3",
                       sub(pattern = "2018-9", "2018-Q4",
                           sub(pattern = "2017-12", "2018-Q1", 
                               sub(pattern = "2018-3", "2018-Q2", quarter))))
  )

large_offices <- c("Boston, MA","Charlotte, NC","Chicago, IL","Fairfield, OH","Irving, TX",      
                   "Lawrenceville, GA", "New York, NY","San Francisco, CA", "Warrenville, IL", "Weston, MA" )
all_other_offices <- setdiff(names(table(scoredNI$office_name)),large_offices)

depts <- sort(unique(scoredNI$x_dept_name))

offices <- sort(unique(scoredNI$office_name))

# User form fields
fieldsAll <- c("track_option", "employee_name_input", "manager_name_input", "action_date", "action_type", "action_other", "action_comments")
responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}

# List Employee n number and manager n number for user form fields
name_lookup <- scoredNI %>%
  dplyr::filter(quarter == curr_quarter) %>%
  dplyr::select(EMPLOYEE_NUMBER, EMPLOYEE_FULL_NAME, x_m_name, x_m_employee_number) %>%
  unique()

m_name_lookup <- scoredNI %>%
  dplyr::filter(quarter == curr_quarter) %>%
  dplyr::select(x_m_name, x_m_employee_number) %>%
  unique()

#### **** Server **** #### 

server <- function(input, output, session) {
  
  # User N# ####  
  #' \code{user} is a reactive funciton that gets the username of the user who is currently logged is,
  #'             passed htorugh the sesison object of Shiny Server. If ran locally and no user is passed in,
  #'             the username is retrieved from the system environment
  #'             
  #' @return username
  #'
  user <- reactive({
    
    if(!is.null(session$user)){
      return(tolower(session$user))
    }
    
    return(Sys.getenv("USERNAME"))
    
  })
  
  # User Tracking ####
  observe(priority = 5,{
    user()
    
    user_tracking <- read.csv('tracking.csv', header=T, stringsAsFactors=FALSE)
    
    nnumberchar <- as.character(user())
    
    user_tracking <- rbind(user_tracking,
                           data.frame(n_number = nnumberchar,
                                      date = as.character(Sys.time()),
                                      access = ifelse(!user() %in% user_permissions$employee_id, "denied","granted")))
    
    write.csv(user_tracking, 'tracking.csv', row.names=FALSE)
  })
  
  # User Permissions ####
  observe(priority = 4,{
    user()
    print(user())
    
    if(!user() %in% user_permissions$employee_id){
      print("User does not have permission to access.")
      permtoload <<- FALSE
      stopApp("User does not have permission to access.")
    }
    else{
      print("Granting access...")
      permtoload <<- "granted"
    }
  })
  # 
  # For filtering data on Level 3
  get_user_permissions <- function(){
    print(user())
    if(user() %in% user_permissions$employee_id){
      username <- user_permissions[which(user_permissions$employee_id == user()),
                                   c("GRS.NI.Casualty","GRS.NI.Middle.Market", "GRS.NI.National.Property", "GRS.NI.Specialty.Accounts")]
      # c("CI-NI-Casualty", "CI-NI-Middle Market", "CI-NI-National Property", "CI-NI-Specialty Accounts")]
      permission_filter <- colnames(username)[username == 1]
      print(permission_filter)
      return(permission_filter)
    }
    else{
      return(NULL)
    }
  }
  
  permission_filter <- reactive({
    perms <- get_user_permissions()

    perms <- gsub("GRS.NI.Casualty","GRS-NI-Casualty", perms)
    perms <- gsub("GRS.NI.Middle.Market","GRS-NI-Middle Market", perms)
    perms <- gsub("GRS.NI.National.Property","GRS-NI-National Property", perms)
    perms <- gsub("GRS.NI.Specialty.Accounts","GRS-NI-Specialty Accounts", perms)

    return(perms)
  })

  dashdata <- reactive({
    validate(
      need(permtoload == "granted","You do not have permission to access")) # wait for permissions to be validated
    return(scoredNI)
  })
  
  # Filter dataset ####
  dfFltr <- reactive({
    sel_offices <- input$office
    
    if("All Other Offices" %in% sel_offices){
      sel_offices <- c(sel_offices[!sel_offices=="All Other Offices"],all_other_offices)}
    
    scoredNI <- dashdata()
    
    df <- scoredNI %>% 
      dplyr::filter(curren_pcm_bu_orig %in% input$pcm &
                      college_hire %in% input$college_hire &
                      rehire_flag %in% input$rehire &
                      office_name %in% sel_offices &
                      x_level_3 %in% input$level_3) 
  })
  
  # Filter dataset for Employee and Manager views based on user_permissions.csv
  dfFltrNames <- reactive({
    dfFltr()
    
    # For filtering data on determined Level (3, 4, or 5)
    if(user() %in% user_permissions$employee_id){
      
      # Which Level to filter at
      filter_at <- user_permissions[which(user_permissions$employee_id == user()), c("filter_names_at")]

      # Values for that filter (e.g. "NI Casualty UW - East")
      filter_value <- user_permissions[which(user_permissions$employee_id == user()), c("filter_value")]
      filter_value_list <- unlist(strsplit(as.character(filter_value), ", "))
      
      print("filter at")
      print(filter_at)
      print("on")
      print(filter_value_list)
      
      # Start with user-filtered dataset (doing it in this order so the overall tab is only filtered at level 3)
      dfFltr <- dfFltr()

      # Filter appropriate column by appropriate values
      # df <- dfFltr[dfFltr[colnames(dfFltr)==as.character(filter_at)]==filter_value_list,]
      df <- dfFltr[apply(dfFltr[colnames(dfFltr)==as.character(filter_at)], 1, function(x) x %in% filter_value_list),]

      return(df)
    }
    else{
      return(NULL)
    }
    
  })
  
  
  # Scatter Plot ####
  # 
  # output$riskScatter <- renderPlotly({
  #   view <- input$scatterSelect
  #   
  #   df <- dfFltr() %>% 
  #     dplyr::filter(quarter == curr_quarter) %>%
  #     dplyr::group_by_(view) %>%
  #     dplyr::summarise(n=n(),
  #                      avg_risk = round(mean(risk_score),2)
  #     ) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::mutate(relative_risk = round(avg_risk/mean(avg_risk),2))
  #   
  #   names(df)[names(df)==view] <- "label"
  #   
  #   plot_ly(data = df,
  #           x=~n,
  #           y=~relative_risk,
  #           # name = "Risk Relativity",
  #           type= "scatter",
  #           marker=list(
  #             color=~relative_risk,
  #             colorbar=list(title='Risk Scale'),
  #             colorscale='YlOrRd',
  #             # colors=c("green","red")
  #             reversescale=T
  #           ),
  #           mode="markers",
  #           # text = paste(df$label),
  #           text=~paste(label, "<br> n=", n, "<br> Risk Relativity: ", relative_risk), 
  #           hoverinfo=text,
  #           textposition="top right",
  # 
  #           # For identifying click activity
  #           key=~label, 
  #           source="riskClick") %>%
  #     # add_text(textfont = list(family = "sans serif", size=14, color = "#000000"),
  #     #          textposition="auto", showlegend = FALSE) %>%
  #     add_annotations(x=~n, #jitter(n),
  #                     y=~relative_risk,
  #                     text= ~label,
  #                     xanchor='left',
  #                     showarrow=FALSE,
  #                     showlegend=FALSE) %>%
  #     layout(title="Risk Scatter",
  #            xaxis = list(title="Number of Employees"),
  #            yaxis = list(title="Risk Relativity",
  #                         range=c(0,2)),
  #            legend = list(xanchor = "center",
  #                          x = 0.4, y = -.5),
  #            shapes=list(list(type='line',x0=0, x1=max(df$n)+5, y0=1, y1=1,line=list(dash='dot', width=1)))
  #            ) #close layout
  # })
  
  scatterData <- reactive({
    view <- input$scatterSelect
    
    df <- dfFltr() %>%
      dplyr::filter(quarter == curr_quarter) %>%
      dplyr::group_by_(view) %>%
      dplyr::summarise(n=n(),
                       avg_risk = round(mean(risk_score),2)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(relative_risk = round(avg_risk/mean(avg_risk),2)) %>%
      as.data.frame()
    
    names(df)[names(df)==view] <- "label"
    
    return(df)
  })
  
  output$scatter2 <- renderPlot({
    
    df <- scatterData()
    
    ggplot(df, aes(x=n,y=relative_risk)) + 
      geom_point(stat="identity", aes(colour=relative_risk,size=2)) + 
      geom_hline(yintercept=1, linetype=3) +
      geom_label_repel(label=df$label) +
      scale_colour_gradient(low = "#A9F5A9", high = "#FA5858",
                            space = "Lab", na.value = "grey50", guide = "colourbar") +
      theme_bw() +
      ggtitle("Risk Scatter") +
      ylab("Risk Relativity")+ xlab("Number of Employees") +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5)) +
      guides(size = FALSE,
             color=guide_colourbar(title="Risk Scale"))
    
  })
  
  # Selected Risk Area ####
  
  # Select All button to deselect 
  observeEvent(input$deselect,{
    js$resetClick()
  })
  
  output$riskselTxt <- renderText({
    s <- shiny::event_data("plotly_click",source="riskClick")
    
    if (is.null(s)){text <- "Selected Population: Overall"} 
    else{text <- paste("Selected Population: ",s[["key"]])}
  })
  
  output$riskselTxt2 <- renderText({
    scatterdf <- scatterData()
    
    s <- nearPoints(df=scatterdf, coordinfo=input$scatter_click, xvar="n", yvar="relative_risk", threshold = 10, maxpoints = 1,
                    addDist = FALSE)

    if (nrow(s)>0){text<- paste("Selected Population: ", s$label)}
    else{text <- paste("Selected Population: Overall")}
    
  })
  
  # filter dataset to user's selection
  dfFltrSel <- reactive({
    view <- input$scatterSelect
    s <- plotly::event_data("plotly_click",source="riskClick")
    
    if (is.na(s)){df <- dfFltr()} 
    else{df <- dfFltr() %>% dplyr::filter(s[["key"]]==!!as.name(view))}
    
    return(df)
  })
  
  # filter dataset to user's selection
  dfFltrSel2 <- reactive({
    view <- input$scatterSelect
    scatterdf <- scatterData()
    
    s <- nearPoints(df=scatterdf, coordinfo=input$scatter_click, xvar="n", yvar="relative_risk", threshold = 10, maxpoints = 1,
                    addDist = FALSE)
    
    if (nrow(s)>0){df <- dfFltr() %>% dplyr::filter(s$label==!!as.name(view))}
    else{df <- dfFltr()}
    
    return(df)
  })
  
  # Trend Plot ####
  output$riskTrendPlot <- renderPlotly({
    
    df <- dfFltrSel2()
    
    df <- df %>%
      dplyr::group_by(quarter.text) %>%
      dplyr::summarise(n=n(),
                       atrisk=sum( risk_bucket == "High"),
                       pctrisk = round(atrisk/n,2),
                       pctxrisk = 1-pctrisk)
    
    plot_ly(df,
            x=~quarter.text,
            y=~pctxrisk*100,
            type="bar",
            name="Low Risk",
            marker = list(color = '#A9F5A9')) %>%
      add_trace(y=~pctrisk*100,
                name="High Risk",
                text=~paste0(pctrisk*100,"%",'<br> n=', atrisk),
                textposition="auto",
                textfont=list(size=14,color='#000000'),
                marker = list(color = '#FA5858')) %>%
      layout(title="Quarterly Risk Trend",
             yaxis = list(title = "Percent at Risk"), 
             xaxis = list(title = "Quarter"),
             barmode="stack")
  })
  
  # Description of Top Model Drivers ####
  output$driverPct <- renderPlotly({
    df <- dfFltrSel2() %>% 
      filter(quarter == curr_quarter) %>%
      dplyr::summarise(n=n(),
                       break_age=sum(age<44),
                       break_los=sum(los_months > 8.5 & los_months < 56),
                       break_tij=sum(tij_tomed > -12),
                       break_poapps=sum(po_apps_r12_ct_bu!="None"),
                       break_pcmbelow=sum(current_pcm_bu=="Below Expectations"),
                       break_teammix=sum(losnh_60m_cc_pct>50),
                       break_m_los=sum(m_los_mo < 103),
                       break_rangepen=sum(range_pen<.5 & range_pen>.2),
                       pct_age=round(break_age/n,2),
                       pct_los=round(break_los/n,2),
                       pct_tij=round(break_tij/n,2),
                       pct_poapps=round(break_poapps/n,2),
                       pct_pcmbelow=round(break_pcmbelow/n,2),
                       pct_teammix=round(break_teammix/n,2),
                       pct_rangepen=round(break_rangepen/n,2),
                       pct_mlos=round(break_m_los/n,2)
      ) %>% 
      select(#pct_age, 
        pct_los, pct_pcmbelow, pct_teammix, pct_tij, pct_poapps, pct_mlos #,pct_rangepen
      ) %>%
      melt()
    
    
    df$labels<-c(#"Age < 44",
      "Length of Service between 9 mo and 5 yrs",
      "PCM Below Expectations",
      "Lower tenured cost center",
      "Time in job relative to peers >1 yr",
      "Posted out at least once in last yr",
      "Manager Length of Service < 9 yrs"
      #,"Range Penetration between 0.2 and 0.5"
    )
    
    df$ratex<-c(#'(1.8x)', 
      '(2.1x)', '(2.1x)','(2.0x)','(1.1x)','(1.9x)','(1.3x)') #,'(1.1x)')
    
    # Order by REVERSE!  importance? or 2.0x 
    ylist <- list(title="", 
                  categoryorder = "array", 
                  categoryarray=c("Time in job relative to peers >1 yr",
                                  #"Range Penetration between 0.2 and 0.5",
                                  "Manager Length of Service < 9 yrs",
                                  #"Age < 44",
                                  "Posted out at least once in last yr",
                                  "Lower tenured cost center",
                                  "PCM Below Expectations",
                                  "Length of Service between 9 mo and 5 yrs")
    )
    
    plot_ly(df,
            x=~value*100,
            y=~labels,
            text=~paste0(value*100,'%'), 
            type="bar",
            marker = list(color = '#ffa470')) %>%
      add_text(textfont=list(size=14,color='#000000'), textposition="middle-right") %>%
      add_annotations(text=~ratex, xref="paper", x=1, y=~labels,  showarrow=FALSE) %>%
      layout(title="Percent of Selected Population with Top Risk Characteristics",
             xaxis = list(title="Percent of Selected Population",
                          range=c(0,100)),
             yaxis = ylist,
             margin=list(l=325, r=50, b=50, t=50, pad=4),
             showlegend=FALSE)
    
  })
  
  # Employees ####
  
  # Table of high risk employees
  output$riskEmp <- DT::renderDataTable({
    df <- dfFltrNames() %>% 
      filter(x_dept_name %in% input$dept &
               quarter == curr_quarter &  # fix this later
               risk_bucket == "High" &
               x_level_3 %in% permission_filter()) %>%
      select(x_employee_number,EMPLOYEE_FULL_NAME, x_m_name, office_name, 
             current_pcm_bu, current_pcm_re, college_hire, grade_equivalent, age, # add employee name 
             los_months, tij_tomed,  po_apps_r12_ct_bu, losnh_60m_cc_pct, m_los_mo, range_pen, # top variables
             risk_score, risk_bucket
      ) %>%
      mutate(pcm = ifelse(current_pcm_bu == "Below Expectations", "X", ""),
             los = ifelse(los_months > 8.5 & los_months < 56, "X", ""),
             tij = ifelse(tij_tomed > -12, "X", ""),
             age = ifelse(age<44,"X",""),
             poapps = ifelse(po_apps_r12_ct_bu!="None","X",""),
             teammix = ifelse(losnh_60m_cc_pct>50,"X",""),
             mlos=ifelse(m_los_mo <  103,"X",""),
             rpen =ifelse(range_pen<.5 & range_pen>.2,"X","")
      ) %>%
      arrange(-risk_score)
    
    df$x_employee_number <- str_pad(df$x_employee_number, 7, pad = "0")
    df$x_employee_number <- str_pad(df$x_employee_number, 8, pad = "n")
    
    # Descriptive
    df$`Employee Number` = df$x_employee_number
    df$`Employee Name` = df$EMPLOYEE_FULL_NAME
    df$`Manager Name` = df$x_m_name
    df$`Office` = df$office_name
    df$`College Hire` = df$college_hire
    df$`Grade` = df$grade_equivalent
    df$`PCM` = df$current_pcm_re
    
    # Top Drivers
    df$`PCM Below Expectations` = df$pcm
    df$`Lower tenured cost center` = df$teammix
    df$`Posted out at least once in last yr` = df$poapps
    df$`Age < 44` = df$age
    df$`Length of Service between 9 mo and 5 yrs` = df$los
    df$`Time in job relative to peers >1 yr`= df$tij
    df$`Manager Length of Service < 9 yrs` = df$mlos
    df$`Range Penetration between 0.2 and 0.5` = df$rpen
    
    
    df$`Risk Bucket` = df$risk_bucket
    
    df <- df[,c("Risk Bucket","Employee Name", "Employee Number","Manager Name", "Office", "College Hire", "Grade", "PCM",
                "Length of Service between 9 mo and 5 yrs", "PCM Below Expectations", "Lower tenured cost center", 
                #"Posted out at least once in last yr", 
                #"Age < 44", 
                "Manager Length of Service < 9 yrs",
                #"Range Penetration between 0.2 and 0.5",
                "Time in job relative to peers >1 yr")]
    
    DT::datatable(df, rownames=FALSE, 
                  filter = 'top',
                  caption = "The table below shows high-risk employees and their possible risk characteristics. 
                  Please note that only a subset of risk characteristics are shown in the table below. 
                  Also note that checked risk characteristics are not equally weighted. 
                  For example, employees with 5 characteristics checked may not necessarily be at 
                  higher risk than employees with 1 characteristic checked.",
                  options = list(
                    columnDefs = list(list(className = 'dt-center', targets = 8:12)),
                    scrollX = TRUE),
                  container = htmltools::withTags(table(
                    class = 'display',
                    thead(
                      tr(
                        # th(rowspan = 2, 'Species'),
                        th(style="background-color:#9ec1d8",colspan = 8, 'High Risk Employees'),
                        th(style="background-color:#629cc1;", colspan = 5, 'Possible Risk Characteristics')#,
                        # th(colspan = 2, 'Petal')
                      ),
                      tr(
                        lapply(c("Risk Bucket","Employee Name", "Employee Number","Manager Name", "Office", "College Hire", "Grade", "PCM",
                                 "Length of Service between 9 mo and 5 yrs", "PCM Below Expectations", "Lower tenured cost center", 
                                 #"Posted out at least once in last yr", 
                                 #"Age < 44", 
                                 "Manager Length of Service < 9 yrs",
                                 #"Range Penetration between 0.2 and 0.5",
                                 "Time in job relative to peers >1 yr"), th)
                      )
                    )
                  )),
                  callback = JS("
                                var tips = ['Based on model score', 
                                'Employee Name', 'Employee Number', 'Manager Name','Assigned office',
                                'Liberty Gateway College Hire Indicator','Grade','Current PCM',
                                'Employee current length of service', 'Current PCM 6 or below',
                                'Over 50% of employees in cost center have <5 years length of service',
                                'Manager current length of service at Liberty',
                                'Time in job code as compared to similarly tenured UW'],
                                header = table.columns().header();
                                for (var i = 0; i < tips.length; i++) {
                                $(header[i]).attr('title', tips[i]);
                                }
                                ")
                  ) %>%
      formatStyle(
        c("Risk Bucket","Length of Service between 9 mo and 5 yrs", "PCM Below Expectations", "Lower tenured cost center", 
          #"Posted out at least once in last yr", 
          #"Age < 44", "Range Penetration between 0.2 and 0.5",
          "Manager Length of Service < 9 yrs", "Time in job relative to peers >1 yr"),
        backgroundColor = styleEqual(c("High","","X"), c("#FA5858","#d1d1d1",'#ffda6d')))
  })

  # Manager  ####
  output$mgrBar <- renderDataTable({
    
    df <- dfFltrNames() %>% filter(quarter == curr_quarter)
    
    df <- df %>% 
      group_by(x_m_name) %>% 
      summarise(n=n(),
                highperfatrisk = sum(curren_pcm_bu_orig %in% c("Very High Performer","Above Expectations") &
                                       risk_bucket == "High"),
                pct_highperfatrisk = round(highperfatrisk/n,2)*100
      ) %>%
      arrange(-highperfatrisk)
    
    # Descriptive
    df$`Manager Name` = df$x_m_name
    df$`Team Size` = df$n
    df$`Count of Top Performers at Risk` = df$highperfatrisk
    df$`Percent of Team (Top Perf + At Risk)` = df$pct_highperfatrisk
    
    df <- df[,c("Manager Name", "Team Size", "Count of Top Performers at Risk", "Percent of Team (Top Perf + At Risk)")]
    DT::datatable(df, rownames=FALSE)
    # , options = list(
    # columnDefs = list(list(className = 'dt-center', targets = 6:11)))) %>%
    # formatStyle(
    #   c("PCM Below Expectations", "Lower tenured cost center", "Posted out at least once in last yr",
    #     "Age < 44", "Length of Service between 9 mo and 9 yrs","More time in job than peers"),
    #   backgroundColor = styleEqual(c("","X"), c("#d1d1d1",'#ffda6d')))
    
  })
  
  # List of employees on Manager's team
  output$mgr_empl_tbl <- renderDataTable({
    # s <- event_data("plotly_click",source="mgrplot")
    validate(
      need(input$mgr,"Processing..."))
    
    m_name <- input$mgr
    df <- dfFltr() %>% 
      filter(x_m_name == m_name & 
               # filter(x_m_name %in% s[["key"]] & 
               quarter == curr_quarter &
               x_level_3 %in% permission_filter()) %>%
      mutate(x_employee_number = str_pad(x_employee_number, 7, pad = "0")) %>%
      select("Manager Name" = x_m_name,"Employee Name" = EMPLOYEE_FULL_NAME, "Employee Number" = x_employee_number, 
             "PCM" = current_pcm_re, "Grade" = grade_equivalent,
             "Risk Bucket" = risk_bucket)
    
    DT::datatable(df) %>%
      formatStyle(
        c("Risk Bucket","PCM"),
        backgroundColor = styleEqual(c("High","Med","Low",
                                       "10","11","12","13","14","15"), 
                                     c("#FA5858","#ffd68c",'#abffa4',
                                       "#fffda5","#fffda5","#fffda5","#fffda5","#fffda5","#fffda5")))
    
    
  })
  
  # ... mgr Trend plot ####
  output$mgr_trend <- renderPlotly({
    
    validate(
      need(input$mgr,"Processing..."))
    
    # s <- event_data("plotly_click",source="mgrplot")
    m_name <- input$mgr
    df <- dfFltr() %>% 
      dplyr::filter(x_m_name == m_name) #%>%
    # filter(x_m_name %in% s[["key"]]) %>%
    # mutate(yyyy_mm = paste0(x_data_year, "-", month))
    
    df <- df %>% 
      dplyr::group_by(x_m_name,quarter.text) %>% 
      dplyr::summarise(n=n(),
                       highperfatrisk = sum(curren_pcm_bu_orig %in% c("Very High Performer","Above Expectations") &
                                              risk_bucket == "High"),
                       pct_highperfatrisk = round(highperfatrisk/n,2)*100
      )
    
    # df$yyyy_mm <- factor(df$yyyy_mm, levels = c("2017-6", "2017-9","2017-12","2018-3"))
    # df <- df %>% arrange(yyyy_mm)
    
    plot_ly(data = df,
            x=~quarter.text,
            y=~n,
            name = "Direct Report Count",
            type= "bar",
            marker = list(color = '#405c8a'), showlegend = TRUE) %>%
      add_trace(y=~pct_highperfatrisk, name = "Percent High Performers at Risk", type="scatter", mode = 'lines+markers', yaxis="y2", marker = list(color = '#232528'), showlegend = TRUE) %>%
      # add_trace(y=~round(pct_high_risk*100,2), name = "High Risk", type="scatter", mode = 'lines', yaxis="y2", marker = list(color = '#232528'), showlegend = TRUE) %>%
      layout(title="Team Trend",
             xaxis = list(title="Quarter"), 
             yaxis = list(title="Count of UW", side = "left"),
             yaxis2 = list(title = "Percent of Team", side = "right", range = c(0,100), overlaying = "y", showgrid=FALSE),
             margin=list(l=50, r=50, b=100, t=50, pad=4),
             legend = list(xanchor = "center",
                           x = .5, y = -.5,
                           orientation = 'h'))
  })
  
  
  output$mgrSelection <- renderPlotly({
    s <- event_data("plotly_click",source="mgrplot")
    if (length(s)) {
      print(s)
      key <- s[["key"]]
      vars <- c(s[["x"]],s[["y"]])
      df <- setNames(by_mgr[vars], c("x","y"))
      
      plot_ly(df, x=~x) %>%
        add_markers(y=~y) %>%
        layout(xaxis = list(title = s[["x"]]),
               yaxis = list(title = s[["y"]]),
               showlegend = FALSE)
      
    } else{
      plotly_empty()
    }
  })
  
  # List of managers -- maybe include manager pcm + los? or top 3 drivers of risk score?
  output$mgr_tbl <- renderTable({
    df <- by_mgr %>% 
      filter(top_quad_flag == "Regrettable")
    
    df$pct_high_risk <- df$pct_high_risk*100
    df$pct_high_perf <- df$pct_high_perf*100
    
    return(df)
  })
  
  # Observed TO ####
  observed <- reactive({
    fltr <- dfFltrNames() #%>% filter(quarter == curr_quarter)
    custom_choices <- as.character(sort(unique(fltr$x_level_4)))
    print("observed filter")
    print(custom_choices)
    
    high_risk_terms %>% 
      dplyr::filter(LEVEL_4 %in% custom_choices)
  })
  
  output$observedTO <- DT::renderDataTable({
    df <- observed()
    
    df$x_employee_number <- str_pad(df$x_employee_number, 7, pad = "0")
    df$x_employee_number <- str_pad(df$x_employee_number, 8, pad = "n")
    df <- df %>% dplyr::arrange(desc(TERMINATION_DATE))
    
    if(dim(df)[1]==0){
      return(NULL)
    }
    
    df$`Risk Bucket` <- "High"
    df$`Risk Quarter` <- df$risk_quarter
    df$`Termination Date` <- df$TERMINATION_DATE
    df$`Employee Name` <- df$EMPLOYEE_FULL_NAME
    df$`Employee Number` <- df$x_employee_number
    df$`PCM` <- df$CURRENT_YEAR_PERF_RATING
    df$`Office Name` <- df$OFFICE_NAME
    df$`Manager Name` <- df$MANAGER_NAME
    df$`Department Name` <- df$DEPT_NAME
    
    
    df1 <- df[c("Risk Bucket", "Risk Quarter", "Termination Date", "Employee Name", "Employee Number", "PCM","Office Name", "Manager Name", "Department Name")]
    DT::datatable(df1, rownames=FALSE, 
                  filter = 'top',
                  caption = "The table below shows employees who were identified as High Risk and have since Voluntarily Terminated."
    ) %>%
      formatStyle(
        c("Risk Bucket", "PCM"),
        backgroundColor = styleEqual(c("High",
                                       10,11,12,13,14,15), 
                                     c("#FA5858",
                                       "#fffda5","#fffda5","#fffda5","#fffda5","#fffda5","#fffda5")))

  })
  
  output$TO_table <- renderUI({
    if(dim(observed())[1]==0){
      return("Nothing to show")
    }
    
    dataTableOutput("observedTO")
  })
  
  ### UI Filters ####
  output$level3_filter <- renderUI({
    print(permission_filter())
    checkboxGroupInput("level_3", "Level 3",
                       choices = permission_filter(),
                       selected = permission_filter())
  })
  
  
  # dept filter for employee tab
  output$deptFilter <- renderUI({
    df <- dfFltrNames() %>% filter(quarter == curr_quarter)
    custom_choices <- as.character(sort(unique(df$x_dept_name)))
    
    pickerInput("dept","Filter by Department", multiple=TRUE,
                width="50%",
                choices = custom_choices,
                selected = custom_choices,
                options = list('actions-box' = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"))
  })

  # manager dropdown selection
  output$mgrSelect <- renderUI({
    df <- dfFltrNames()
    df <- df %>% filter(quarter == curr_quarter)
    custom_choices <- sort(unique(df$x_m_name))
    print(length(custom_choices))
    
    selectInput("mgr","Select a Manager", 
                choices=custom_choices,
                multiple = FALSE)
  })
  
  # 
  # output$losSlide <- renderUI({
  #   sliderInput("losSlide","Length of Service (yrs)",
  #               min=0, max=10, value=2)
  # })
  # 
  # output$tijSlide <- renderUI({
  #   sliderInput("tijSlide","Time in Job Code vs Peers (yrs)",
  #               min=0, max=10, value=2)
  # })
  # 
  # output$teammixSlide <- renderUI({
  #   sliderInput("mixSlide", "Percentage of New Hires on Team", 
  #                min=0,max=100, value=50)
  # })
  # 
  # output$mgrLosSlide <- renderUI({
  #   sliderInput("mlosSlide", "Manager Length of Service (yrs)", 
  #               min=0,max=50, value=9)
  # })
  # 
  # output$rangePenSlide <- renderUI({
  #   sliderInput("rpenSlide", "Range Penetration", 
  #               min=0,max=1, value=.4)
  # })
  # 
  
  ## Action tracking UI ##
  # Employee names for action tracking
  output$track_employee_names <- renderUI({
    df <- dfFltrNames()
    df <- df %>% filter(quarter == curr_quarter)
    custom_choices <- sort(unique(df$EMPLOYEE_FULL_NAME))
    selectInput("employee_name_input","Employee Name",
                choices=custom_choices,
                multiple=FALSE)
  })
  
  # Manager names for action tracking
  output$track_manager_names <- renderUI({
    df <- dfFltrNames()
    df <- df %>% filter(quarter == curr_quarter)
    custom_choices <- sort(unique(df$x_m_name))
    selectInput("manager_name_input","Manager Name",
                choices=custom_choices,
                multiple=FALSE)
  })
  
  action_list_options <- reactive({
    switch(input$track_option,
           "Employee" = c("Stay Interview", "Skip-level Meeting", "Other"),
           "Manager" = c("Discussion", "Other"))
  })
  
  output$track_action_type <- renderUI({
    list <- action_list_options()
    
    selectInput("action_type", "Select the action taken", list)
  })
  
  # User action form data
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    
    # data$action_date <- as.Date(data$action_date, origin="1970-01-01")
    
    if(input$track_option == "Employee"){
      # data$employee_number <- name_lookup$EMPLOYEE_NUMBER[match(data$employee_name_input, name_lookup$EMPLOYEE_FULL_NAME)] ## does not work. don't know why.
      data$employee_number <- name_lookup$EMPLOYEE_NUMBER[match(data[2], name_lookup$EMPLOYEE_FULL_NAME)]
      data$employee_number <- str_pad(str_pad(data$employee_number,width=7, side="left",pad="0"),
                                      width=8, side="left",pad="n")
      data$manager_name_input <- ""
      data$m_employee_number <- ""
    }
    
    if(input$track_option == "Manager"){
      data$employee_name_input <- ""
      data$employee_number <- ""
      data$m_employee_number <- m_name_lookup$x_m_employee_number[match(data$manager_name_input, m_name_lookup$x_m_name)]
      data$m_employee_number <- str_pad(str_pad(data$m_employee_number,width=7, side="left",pad="0"),
                                        width=8, side="left",pad="n")
    }
    
    data <- c(data, user= user(), timestamp = as.character(Sys.time()))
    print(data)
    data$action_date <- as.Date(as.numeric(data$action_date), origin="1970-01-01")
    data
  })
  
  # Function to Save action form data
  humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
  saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        user(),
                        humanTime())
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = c(1,2,3,5,6,7,8,9))
  }
  
  # action to take when submit button is pressed
  observeEvent(input$submit, {
    withProgress({
      setProgress(message = "Saving...")
      saveData(formData())
      showNotification("Thank you for your response!", type="message", duration=5)
    })
  })
  
  
  ### User Materials ####
  output$stayPDF <- renderUI({
    tags$iframe(style="height:800px; width:100%", src="leader_guide.pdf")
    
    # tags$iframe(style="height:400px; width:100%; scrolling=yes", 
    #             src="https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf")
  })
  
  output$jobaidPDF <- renderUI({
    tags$iframe(style="height:800px; width:100%", src="job_aid.pdf")
  })
  
  
  output$downloadStay <- downloadHandler(
    filename <- function() {
      paste("Stay_Interview_Guide.docx")
    },
    
    content <- function(file) {
      file.copy("stay_interview_form.docx", file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  
  
} # end server


#### **** UI **** #### 

ui <- dashboardPage(
  dashboardHeader(title = "NI UW Turnover"),
  dashboardSidebar(
    tags$head(
      tags$style(HTML(".sidebar {
                      height: 90vh; overflow-y: auto;
                      }"
      ) # close HTML       
      )            # close tags$style
      ),             # close tags#Head
    sidebarMenu(
      id = "menu",
      # menuItem("Introduction", tabName = "intro", icon=icon("info-circle")),
      menuItem("Leader Playbook",tabName = "materials", icon=icon("clipboard")),
      menuItem("Technical Job Aid",tabName = "jobaid", icon=icon("info")),
      # menuSubItem("Stay Interview Guide", tabName = "stayguide")),
      menuItem("Dashboard", icon=icon("tachometer"),
               menuSubItem("Overview", tabName = "riskloc", icon = icon("map-marker", class = NULL, lib = "glyphicon")),
               menuSubItem("Employee View", tabName = "employees", icon = icon("users")),
               menuSubItem("Manager View", tabName = "managers", icon = icon("briefcase", class = NULL, lib = "glyphicon")),
               menuSubItem("High Risk Terminations", tabName = "observed", icon = icon("sign-out")),
               menuSubItem("Action Tracking Form", tabName = "action", icon=icon("pencil"))
      ),
      
      # filters
      conditionalPanel(condition = "input.menu != 'materials' & input.menu != 'jobaid' & input.menu != 'action'",
                       uiOutput("level3_filter")),
      conditionalPanel(condition = "input.menu != 'materials' & input.menu != 'jobaid' & input.menu != 'observed' & input.menu != 'action'",
                       checkboxGroupInput("college_hire","College Hire Indicator", 
                                          choices = list("Yes"="Y", "No"="N"), 
                                          selected = c("Y", "N"),
                                          inline=TRUE),
                       checkboxGroupInput("pcm","Performance Bucket (PCM)", 
                                          choices = list("Missing (<1 yr LOS)" = "Missing",
                                                         "Very High Performer (13-15)"="Very High Performer", 
                                                         "Above Expectations (10-12)"="Above Expectations",
                                                         "Meets Expectations (7-9)"="Meets Expectations",
                                                         "Below Expectations (<=6)"="Below Expectations"), 
                                          selected = c("Missing", "Very High Performer","Above Expectations","Meets Expectations", "Below Expectations")),
                       pickerInput("office","Offices", multiple=TRUE,
                                   choices = c(large_offices,"All Other Offices"),
                                   selected = c(large_offices,"All Other Offices"),
                                   options = list('actions-box' = TRUE,
                                                  size = 10,
                                                  `selected-text-format` = "count > 5")),
                       checkboxGroupInput("rehire", "Rehire Indicator", choices = c("Yes","No"),
                                          selected = c("Yes", "No"), inline=TRUE)
      ), # end conditionalpanel
      hr(),
      a(actionButton(inputId = "email1", label = " Contact Us", 
                     icon = icon("envelope", lib = "font-awesome"),
                     style = "color: #fff; background-color: #616265;",
                     width= '135px'),
        href="mailto:TA_ProductDevelopment@LibertyMutual.com"),
      a(downloadButton("downloadStay", label="Stay Interview Guide",style = "color: #fff; background-color: #06748c; margin-left: 15px;",
                       width= '145px'))
    ) # end Sidebarmenu
      ), # end side bar
  
  dashboardBody(
    useShinyjs(),
    # code to reset plotlys event_data() to NULL -> executed upon action button click
    # note that "A" needs to be replaced with plotly source string if used
    extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-riskClick', 'null'); }"),
    tabItems(
      
      # Tab: Materials ####
      tabItem(tabName = "materials",
              uiOutput("stayPDF")),
      # tags$iframe(style="height:400px; width:100%; scrolling=yes", 
      #             src="https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf")
      
      # Tab: Job Aid ####
      tabItem(tabName = "jobaid",
              uiOutput("jobaidPDF")),
      
      # Tab: Risk Location ####
      tabItem(tabName = "riskloc",
              
              box(width= NULL,
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, collapsed = FALSE,
                  title = "Scatter Plot",
                  radioButtons("scatterSelect","Select a View", 
                               choices = c("Office"="office_name","Department"="x_dept_name","Manager"="x_m_name"), 
                               selected = "office_name", inline = TRUE),
                  # plotlyOutput("riskScatter"),
                  # hr(),
                  # textOutput("permissionText"),
                  h5("Click a point on the plot to view trending and risk characteristics below."),
                  plotOutput("scatter2", click="scatter_click")
                  # fluidRow(
                  #   column(4,
                  #          # h4(textOutput("riskselTxt")),
                  #          actionButton("deselect","Select Overall Population", icon=icon("circle-o-notch"))
                  #   ))
              ), 
              
              box(width = NULL,
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, collapsed = FALSE,
                  title = textOutput("riskselTxt2"),
                  h4("Quarterly Risk Trend"),
                  h5("Quarterly distribution of high risk employees within the selected population."),
                  plotlyOutput("riskTrendPlot"),
                  
                  hr(),
                  h4("Top Characteristics"),
                  h5("Characteristics are ranked by voluntary turnover risk as determined by model and observed 2017 turnover rates. 
                     (Example: In 2017, employees who had a 'Below Expectations' PCM had a turnover rate 2.1x the average rate.)"),
                  br(),
                  plotlyOutput("driverPct"),
                  br(),
                  h5("Note: Observed rates don't account for variable interaction in the model. 
                     Some variables in isolation are less predictive than when layered with other predictors. 
                     (Example: Time in job relative to peers)"))
              
                  ), # end Tab: Risk Location
      
      # Tab: Employees ####
      tabItem(tabName = "employees",
              column(width=12,
                     box(width = NULL,
                         title = "High Risk Employees",
                         solidHeader = TRUE,
                         collapsible = TRUE, status = "primary",
                         uiOutput("deptFilter"),
                         hr(),
                         bsPopover(id = "riskEmp", title = "Risk Characteristics",
                                   content = "<b>Length of Service between 9 mo and 5 yrs</b>: \n
                                   <b> Age < 44 </b>
                                   PCM Below Expectations
                                   Lower tenured cost center
                                   Time in job relative to peers >1 yr
                                   Posted out at least once in last yr
                                   Manager Length of Service < 9 yrs
                                   Range Penetration between 0.2 and 0.5
                                   ", 
                                   placement = "right", trigger = "hover"
                                   # ,       options = list(container = "body")
                         ),
                         dataTableOutput("riskEmp")
                     ))#,
              # fluidRow(
              #   column(width=4,
              #          box(width=NULL,
              #              title="Risk Characteristic Customization",
              #              status="primary",
              #              solidHeader = TRUE,
              #              collapsible = FALSE,
              #              uiOutput("losSlide"),
              #              uiOutput("tijSlide"),
              #              uiOutput("teammixSlide"),
              #              uiOutput("mgrLosSlisde"),
              #              uiOutput("rangePenSlide")
              #          )),
              #   column(width =8,
              #          box(width=NULL,
              #              title="Turnover Risk",
              #              status="primary",
              #              solidHeader = TRUE,
              #              collapsible = FALSE,
              #              plotlyOutput("driver_risk"))))
      ), 
      
      # Tab: Managers ####
      tabItem(tabName = "managers", 
              
              box(width= NULL,
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, collapsed = FALSE,
                  title ="Regrettable Risk by Manager",
                  h5("Top performers are defined by a current PCM of 10+"),
                  hr(),
                  dataTableOutput("mgrBar")#,
                  # column(4,sliderInput("teamsize", "Team Size Range", min = 1, max = 10, value = c(7,10)))
                  # ,
                  # column(12,plotlyOutput("mgrScatter"))
              ),
              
              box(title = "Details",
                  width = NULL, status = "primary", solidHeader = TRUE,
                  id = "mgr_tables",
                  collapsible = TRUE, collapsed = FALSE,
                  tabPanel("Selected Manager",
                           # tableOutput("mgrSelect"),
                           uiOutput("mgrSelect"),
                           
                           br(),
                           h4("Selected Manager's Team"),
                           dataTableOutput("mgr_empl_tbl"),
                           br(),
                           h4("Quarterly Trend"),
                           plotlyOutput("mgr_trend"))
              )
      ), # end managers tab
      
      # Tab: High Risk Terminations ####
      tabItem(tabName = "observed",
              box(title = "High Risk Terminations",
                  width = NULL, status = "primary", solidHeader = TRUE,
                  uiOutput("TO_table"))
      ), # end high risk turnover tab
      
      # Tab: Action Tracking ####
      tabItem(tabName = "action",
              box(title = "Action Tracking Form",
                  width = NULL, status = "primary", solidHeader = TRUE,
                  h5("Use this form to log any actions taken as a result of the information in this dashboard."),
                  div(
                    id = "form",
                    column(12,
                           radioButtons("track_option","Select the Employee or Manager you talked to", choices=c("Employee","Manager"), selected="Employee", inline=TRUE)),
                    column(3,
                           conditionalPanel(condition="input.track_option=='Employee'",
                                            uiOutput("track_employee_names")),
                           conditionalPanel(condition="input.track_option=='Manager'",
                                            uiOutput("track_manager_names"))),
                    column(2,
                           dateInput("action_date", "Date of action", value = Sys.Date(), min = "2018-06-30",
                                     format = "yyyy-mm-dd", startview = "month",
                                     language = "en", width = NULL)),
                    column(3, 
                           # selectInput("action_type", "Select the action taken",
                           #      c("Stay Interview", "Skip-level Meeting", "Discussion with a Manager", "Other")),
                           uiOutput("track_action_type"),
                           conditionalPanel(condition="input.action_type == 'Other'",
                                            textInput("action_other","Please explain the action taken",""))),
                    column(4,
                           textAreaInput("action_comments", "Additional comments (optional)", width="100%", height="150px", value="")),
                    column(12, 
                           actionButton("submit", "Submit", class = "btn-primary"), align="right") 
                    )
                  ) 
              )   
      )             
    )     
  )


#### APP #### 
shinyApp(ui, server)
