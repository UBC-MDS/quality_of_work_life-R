library(dash)
library(dashBootstrapComponents)
library(dashHtmlComponents)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(purrr)
library(dplyr)
library(tidyr)

qwl_df <- readr::read_csv("./data/bei_vita_qwl_assessment.csv")
qwl_df$residence <- qwl_df$`Country of Residence`
residence_list <- unique(qwl_df[c("residence")])$residence
print(residence_list)
qwl_df$ts <- as.character(qwl_df$"Total score")

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
    dbcContainer(
        list(
        dbcRow(
            list(
                dbcCol(
                    list(
                        htmlBr(),
                        htmlH2("Bei Vita"),
                        #TODO Maybe change to markdown?
                        htmlH3("Visualization to represent how scores for quality of
                                work life are distributed")
                    ), 
                    width = 4
                ),
                dbcCol(
                    list(
                        htmlH1("Quality of Work Life"),
                        htmlH2("Client: HDBC Bank")
                    ), width = 6
                    )
                )
            ),
        
        
            dbcRow(
                list(
                    dbcCol(
                        list(
                            htmlDiv(
                                list(
                                    htmlH6("Country of Residence"),
                                    dccDropdown(
                                        id="col-select",
                                        options = residence_list %>% purrr::map(function(col) list(label = col, value = col)),
                                        value = "HK & Macau"
                                    ),
                                    dccGraph(id="lineplot")
                                )
                            )
                        ), width = 12
                    )
                )
            ),
        
        
            dbcRow(
                list(
                    dbcCol(
                        list(
                            # Berkay's Plot
                            dccGraph(id="hbarplot")
                        )
                    ),
                    dbcCol(
                        list(
                            # Morgan's Code
                            dccGraph(id='plot')
                        )
                    )
                    
                )
            )
        
        ) # Outer List Close
    ) # Container Close
) # Layout Close

# Kyle's Function
app$callback(
    output("lineplot", "figure"),
    list(input("col-select", "value")),
    function(xcol) {
        qwl_df_ts <- qwl_df %>% 
            filter(residence == xcol) %>%
            group_by(ts) %>% 
            count()
        
        qwl_df_ts$ts <- as.numeric(qwl_df_ts$ts)
        
        
        p <- ggplot(qwl_df_ts, aes(x = ts)) +
            geom_histogram(color = "lightblue") +
            scale_x_continuous(name="Total Score", limits=c(0, 41)) + 
            scale_y_continuous(name="Number of Employees", limits=c(0, 8)) +
            ggtitle("How healthy are the employees feeling overall?") +
            ggthemes::scale_color_tableau()
        
        ggplotly(p)
    }
)

# Berkay's Function
app$callback(
    output("hbarplot", "figure"),
    list(input("col-select", "value")),
    function(xcol) {
        col_name = "17. I experience MEANINGFULNESS at work ... (e.g. inspired, trusted, respected, purpose, seen and heard, acknowledged, fulfilled, growth, contribution to something greater, etc.)"
        
        plot_data <- qwl_df %>%
            rename(analysis_col = col_name) %>%
            filter(residence == xcol) %>%
            count(analysis_col)
        
        p <- ggplot(data=plot_data, aes(x=analysis_col, y=n)) +
            geom_bar(stat = 'identity') +
            ggtitle("How frequently do employees find meaningfulness at work?") +
            ggthemes::scale_color_tableau()
        
        ggplotly(p)
    }
)


# Morgan's Function
app$callback(
    output("plot", "figure"),
    list(input("col-select", "value")),
    function(c) {
        workshop_topics <- c('Stress Optimization',
                     'Mindset Coaching',
                     'Sleep Strategies',
                     'Social Wellbeing',
                     'Leadership and Teamwork',
                     'Physical Health & Fitness',
                     'Nutrition & Gut Health')
        
        qwl_df_temp <- qwl_df
        names(qwl_df_temp)[5] = "Country"
        
        temp <- qwl_df_temp[,56:62]
        colnames(temp) <- workshop_topics
        temp$Country = qwl_df_temp$Country
        
        temp <- temp %>%
            pivot_longer(!Country, names_to = "Workshop", values_to = "Preference")
        
        temp <- temp %>% filter(Country == c)
        p <- ggplot(temp)+
            aes(x = Workshop, y = Preference, color = Workshop) +
            geom_boxplot(aes(middle = mean(Preference))) + 
            theme(axis.text.x = element_text(angle = 90)) +
            ggtitle("What topics do your employees want support with (lower is better)?") +
            ggthemes::scale_color_tableau()
        ggplotly(p)
    }
)

app$run_server(host = '0.0.0.0')
