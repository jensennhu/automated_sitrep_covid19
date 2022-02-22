
# test colors
cols <- c("Cases higher and rising" = "#CD4071FF", 
          "Cases lower and rising" = "#FD9567FF",
          "Cases higher and decreasing" = "#26828EFF",
          "Cases lower and decreasing" = "#29999AAA", 
          "Cases higher and staying" = "#9F2F7FFF",
          "Cases lower and staying" = "#FD115FFF")

# other colors
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"
fixedWidth = 100 
fixedgWidth = 180

# format state text
state_formatter <- formatter(
  "span", style = ~ style(color = "grey",font.weight = "bold"))
# format trend
trend_formatter <- formatter("span", 
                                   style = x ~ style(font.weight = "bold", 
                                                     color = ifelse(x == "decreasing", customGreen, 
                                                                    ifelse(x == "rising", customRed, "black"))), 
                                   x ~ icontext(ifelse(x == "rising", 
                                                       "arrow-up", 
                                                       "arrow-down"), "")
)
# format icu stress and inpatient hospital usage with color text
color_code <- formatter("span", 
                        style = x ~ style(font.weight = "bold", 
                                          color = ifelse(x == "extreme", "#C70039", 
                                                         ifelse(x == "high", "#FF5733", 
                                                                ifelse(x == "moderate", "#FFC300" , 
                                                                       ifelse(x == "low", "#DAF7A6",
                                                                              "#29999AAA"))))
                        ))
# format icu stress and inpatient hospital usage with color tiles
test_color <- formatter("span",
                        style = function(x) style(display = "inline-block", 
                                                  direction = "rtl", 
                                                  `border-radius` = "4px",
                                                  `padding-right` = "1px", 
                                                  `background-color` = ifelse(x == "extreme",customRed, 
                                                                              ifelse(x == "high", "#FFA53B",
                                                                                     ifelse(x == "moderate","#B3C3D1", 
                                                                                            ifelse(x == "low", "#29999AAA",
                                                                                                   "#29999AAA")))),
                                                  width = "70px"
                        ))

# format bar proportional, using fixedwidth - prevents odd email effects.
bar_formatter <- formatter("span",
                           style = function(x) style(display = "inline-block", 
                                                     direction = "rtl", 
                                                     `border-radius` = "4px",
                                                     `padding-right` = "1px", 
                                                       `background-color` = "pink",
                                                     width = paste(fixedWidth*proportion(x),"px", sep=""))
)
# format bar proportional, using fixedwidth - prevents odd email effects.
bar_rformatter <- formatter("span",
                           style = function(x) style(display = "inline-block", 
                                                     direction = "rtl", 
                                                     `border-radius` = "4px",
                                                     `padding-right` = "1px", 
                                                     `background-color` = "pink",
                                                     width = paste(fixedgWidth*proportion(x),"px", sep=""))
)

bar_gformatter <- formatter("span",
                           style = function(x) style(display = "inline-block", 
                                                     direction = "rtl", 
                                                     `border-radius` = "4px",
                                                     `padding-right` = "1px", 
                                                     `background-color` = "#2EC26D",
                                                     width = paste(fixedgWidth*proportion(x),"px", sep=""))
)


font_and_grad <- formatter("span",
                           style = x ~ style(
                             display = "inline-block",
                             direction = "rtl",
                             "border-radius" = "4px",
                             "padding-right" = "2px",
                             "background-color" = csscolor(gradient(as.numeric(x),min.color="yellow",max.color="darkorange")),
                             "font-family" = "verdana",
                             "font-size" = 10,
                             width = "50px"))


# table function for interactive
table_fun <- function(area){
  
  state_metrics_cat_consec %>% 
    filter(state %in% {area}) %>% 
    select(state, cases_per_capita, trend, icu_stress, inpatient_covid_stress) %>% 
    arrange(desc(cases_per_capita)) %>% 
    rename("State" = state,
           "Cases per 100,000" = cases_per_capita,
           "Trend" = trend,
           "ICU Stress" = icu_stress,
           "Inpatient Stress" = inpatient_covid_stress) %>% 
    formattable(
      align = c("l", "r", "c", "c", "c"),
      list(
        `State` = state_formatter,
        `Cases per 100,000` = bar_rformatter,
        `Trend`= trend_formatter,
        area(col = 4:5) ~ test_color
      ))
}

# table function for email
table_consec_fun <- function(var){
  
  state_metrics_cat_consec %>% 
    filter(trend == {var},
           days >= 1) %>% 
    select(state, cases_per_capita, days) %>% 
    arrange(desc(cases_per_capita)) %>% 
    rename("State" = state,
           "Cases per 100,000" = cases_per_capita,
           "Consecutive Days" = days) %>% 
    formattable(
      align = c("l", "r", "c"),
      list(
        `State` = state_formatter,
        `Cases per 100,000` = if ({var} == "rising"){
          bar_rformatter
        } else {bar_gformatter},
        `Consecutive Days` = font_and_grad
      ))
}



# function for mapping us regions - cases per 100,000
map_func <- function(area){
  plot_usmap(
    data = state_metrics_cat_consec,
    include = {area},
    values = "cases_per_capita", 
    labels = TRUE) +
    scale_fill_continuous(name = "Cases per 100,000 individuals",
                          low = "white", 
                          high = "red") +
    theme_void()
}

# function for mapping us regions - trend based on categories
map_trend_func <- function(area){
  plot_usmap(
    data = state_metrics_cat_consec,
    include = {area},
    values = "categories", 
    labels = TRUE) +
    scale_fill_manual(values = cols,
                      name = "")+
    theme_void() 
}

