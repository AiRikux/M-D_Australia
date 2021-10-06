library(shiny)
library(leaflet)

navbarPage(
  # App title
  "Marital in Australia",
           tabPanel(
             # add tab name
             "Overview",
                    fluidPage(h1("Marriage and Divorce in Australia"),
                              sidebarLayout( position = "right",
                                sidebarPanel(
                                  h3("Student"),
                                  p("Name: Nabilah Anuwar")
                                ),
                                mainPanel(
                                  p(strong("This is a assessment requirement for a subject in my first year in Monash University")),
                                  p("Marriage is an important aspect in life. It marks new change and new beginnings with the person you love the most. However, there is also the opposite of that, divorces. Divorces is when relationship ends yet in a way it is a new beginning. Uncovering the statistics behind marriage and divorce may bring light interesting things, especially the behaviour of people in the region."),
                                  p("Marriage is a messy thing there are many factors that underlies it. Dates, type of wedding, birth country, previous marital status, and age. This platform wants to highlight interesting findings from analyzing these factors."),
                                  p("Here we try to answer:"),
                                  p("- How has marriage (divorce) rate changed over time?"),
                                  p("- What are noticeable trends related to marriage in Australia? E.g.: marriage celebrant, marital status, age group, ethnicity."),
                                  br(),
                                  p("Please be aware representation may not match description of findings as visualisation is simplified. Though visualisation should give the main finding across. Trends and Comparisons are especially limited to only a few vicualisations to prevent confusion, so only main findings are presented."),
                                  br(),
                                  h5("Analysis"),
                                  p("There seems to be no trend in marriages nor divorces that is happening these past few years, contrary to the common folk beliefs. They both fluctuate throughout the years and have no correlation to one another. Marriage factors have changed, though slightly, throughout the years. Less people have preferred marriage by ministry of religion. Also, marriages between people from different birth countries have increased, though marriages between Australians still dominate it is to be expected as out data is from Australia. Males in Australia are more likely to marry in a later age than females. Both are likely to have their first marriage in their late twenties. Throughout the 31 dates available throughout the year, the 13th day is when couples want their wedding the least. Couples may want to avoid bad weather for their wedding thus choosing a month where the weather is similar to spring or autumn is preferable. NSW is the state with the highest number of marriages in Australia. Compared to other countries, Australia has a medium level marriage rate per 1000."),
                                  h5("Reflection"),
                                  p("If I had more time, I would want to compare the number of marriages to the number of education and unemployement. I would also want to compare to questionaires from people of different ages thinking what the perfect time and method would to have a marriage to see if there is a difference in the way 20th century compared to 90th century think."),
                                  p("In my original proposal I wanted to compare Australia marriage over the past few years to other countries. However, there was a lack of resources to find necessary crude number for the countries. I can only get their marriage rates per 1000 people and none other to compare further about the factors."),
                                  br()
                                )
                              )

                              )
                    ),
           tabPanel(
             # add tab name
             "M&G",
                    fluidPage(
                      h2("Marriage and Divorce Over the Years"),
                      sidebarLayout(position = "right",
                        sidebarPanel(width = 3,
                          # insert list
                          selectInput("type1", "Select Output Format", choices = list("Count" = 1, "Rate" = 2)),
                          verbatimTextOutput("info1")
                        ),
                        mainPanel(
                          # plot graph
                          plotOutput("plot1", click = "click1"),
                          strong("Key Points: "),
                          br(),
                          strong("- Marriage numbers is consistent till 2012 yet fluctuates onwards"),
                          br(),
                          strong("- Number of divorces is significantly lower than marriages"),
                          br(),
                          strong("- There seem to be no correlation between marriage occurences and divorce occurences"),
                          br(),
                          br(),
                          p("As seen here the number of marriages was steadily increasing from 2008 to 2012. Then, it plummets in 2013 almost to the same count in 2008. It increases halfway, almost the same as 2010 count. It then dropped significantly in 2015 and increase halfway again in 2016 only for it to plummet lower in 2017 and increase again in 2018. It is very interesting how in the first four years the numbers have been consistent yet from 2013 onwards they fluctuates."),
                          p("Continuing, we then look at the number of divorce occurrence over the years. We see that the number of divorces has been fluctuating over the years even previous to 2012, different to marriages. It is noticable that the number increase and decrease consecutively. However, there is a constant movement of decrease/increase where it is the same as the previous year (decrease was followed by a decrease, and otherwise). These years are 2010, 2014, and 2018. It is interesting that every four years it follows the behaviour of the previous year. It can mostly be coincidental however, further study in human behaviour and social trends would help uncover the factor that correlates to it."),
                          p("It is noticable from this graph is that the number of divorces is almost half of the number of marriages registered. Though there seem to be no correlation to the numbers of divorce and marriages except some instances on where it negatively correlate and other positively correlate. Positive correlation seems to happen in the years 2008 to 2013 as one mimic the other’s movement. Negative correlation happens for a couple of times in the years 2014 to 2018. We further test this by using RStudio pearson correlation test (STHDA, n.d.). Our test shows that there is no significant correlation between the two as their p-value of 0.6755 is not below 0.05 significant value. Thus, no correlation."),
                          p("Next is we try to see the connection between the rate of marriage and divorces. First, lets look at the marriage rates. Though its count was increasing, their growth rate was decreasing from 2009 to 2011. Their rate has been fluctuating since 2008. In the end of 2018, their rate is higher than ever before. In the beginning, divorce rate seems to decrease yet in 2014 onwards it fluctuates more extreme than marriage rates. 2018 divorce rate only seems to lower the rate to almost zero rather than an extreme negative rate different to its previous year. Seeing both together, again we see some kind of correlation as they have some similar movement. However, when we test it using pearson correlation we do not get a p-value within the 0.05 significant level. If there is a significant value, their correlation would be negative as their correlation coefficient is -0.3576."),
                          br(),
                          p("STHDA. (n.d.). Correlation Test Between Two Variables in R. Retrieved from http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r"),
                          br()
                        )
                      )
                    )
                    ),
           navbarMenu(
             # add tab name
             "Trends",
             # listed since trends have many factors
                      tabPanel(
                        # add tab name
                        "Birth Country",
                               fluidPage(
                                 h2("Marriage based on Birth Country"),
                                 plotOutput("plot2"),
                                 p("Since 2008, marriage between Australians is what approximately 50% of the marriage occurred. This is expected as our target location is indeed Australia. Marriage from different birth countries has increased over time as well as those who are both from overseas countries. This could mark that the number of visitors that settled from another country has increased overtime.")
                               )
                               ),
                      tabPanel(
                        # add tab name
                        "Previous Marital Status",
                               fluidPage(
                                 h2("Marriage based on Marital Status"),
                                 plotOutput("plot3"),
                                 p("There seems to be no significant changes in previous marital status in marriage. Both partners that marry for their first time dominate throughout the years. Followed by one of the partners already married to have a slightly larger amount than if both partners remarry. This may mean that people who marry once may not have the tendency to marry again or the number of people marrying once is just higher.")
                               )),
                      tabPanel(
                        # add tab name
                        "Type of Celebrant",
                               fluidPage(
                                 h2("Marriage based on Celebrant"),
                                 plotOutput("plot4"),
                                 p("In this modern era people are more open minded to each other’s beliefs. This tendency may be the cause of why the number of marriages that is have a ministry of religion as its celebrant decreased over time. Easy Wedding (n.d.) has mentioned in their article that civil celebrants wedding is much simpler. People would prefer simplicity over complexity; thus, they might be more interested in having a civil wedding. It can also be possible that the number of marriages with people of different faith affecting the increase of civil marriage."),
                                 br(),
                                 p("Easy Wedding. (n.d.). Religious or civil? Which type of wedding ceremony suits you? Retrieved from https://www.easyweddings.com.au/articles/religious-or-civil-which-type-of-ceremony-suits-you/")
                               )),
                      tabPanel(
                        # add tab name
                        "Marriage by Age",
                               fluidPage(
                                 h2("Marriage based on Age"),
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                selectInput("type2", "Select Pre-Marital Status", choices = list("First Marriage" = 1, "Remarriage" = 2))
                                   ),
                                   mainPanel(
                                     plotOutput("plot5"),
                                     p("Female and male have the tendency to marry at their late twenties. Though for males, it is not as high as females as their number is reaching to 40,000 average. Female aged 16 to 24 are more likely to have their first marriage than that of male. However, male in their thirties are more likely to have their first marriage than that of the women. This can be affected by the belief that women should marry before the age of 30."),
                                     p("Females in their early forthies have the highest tendency to remarry, whilst men are in their late forthies. The age gap is not much of a difference for men and women. The number of males remarry after the age of 50 is significantly higher than that of the female. The number of those that are in 60 to over age group have around twice the size of female in the same age group. The numbers for male in their fifthies are also significantly higher than of female."),
                                     p("From both graph we can see that males have the tendency to have a marriage at a later age than females.")
                                   )
                                 )
                               )
                               ),
                      tabPanel(
                        # add tab name
                        "Marriage by Time of the Year",
                               tabsetPanel(
                                 tabPanel(
                                   # add tab name
                                   "Date Number",
                                          fluidPage(
                                            h2("Marriage by Date Number"),
                                            plotOutput("plot6"),
                                            p("Marriage is a sacred thing and getting the perfect date is very important to some people. Made with Tableau, the graph of average marriage occured by date, we can see that there aren’t many people who would like their wedding to be in the 13th. This may be caused due to superstitious beliefs that the numbr 13 is unlucky. We can see that the date 10th is the most preferable and second to it is 12th. Second least preferable is the 30th where is is near end of the month.")
                                          )
                                          ),
                                 tabPanel(
                                   # add tab name
                                   "Month",
                                          fluidPage(
                                            h2("Marriage by Month"),
                                            plotOutput("plot7"),
                                            p("Looking at the average marriage occurrence by month we can see that there is some kind of ‘peak season’ and ‘low season’. October seems to be the highest, followed by November and March. The low season are found in between May to August, with July as its lowest month. Weather may affect this decision as December to February is summer, too hot, and May to August is winter, too cold, in Australia. Couples may want their wedding to be in weather such as spring and autumn to avoid chance of bad weather in their important day.")
                                          )
                                          ),
                                 tabPanel(
                                   # add tab name
                                   "Days in the Year",
                                          fluidPage(
                                            h2("Marriage by Days through the Year"),
                                            plotOutput("plot8"),
                                            p("This graph shows us the average of weddings in a certain date within a month. Different to our previous graph we can see that there are some dates that is highly preferable than others in their month. In their order is 10th of October, 14th of February, and 8th of August. Other than 14th of February, Valentine’s Day, the other dates do not have any special meaning to weddings. Though many couples would like to have the same date as their month number, which we can see its reasoning in 10-October (10-10) and 8-August (08-08)."),
                                            p("Two noticable dates are less prefered to couples as their date to get married. They are 1st of January and 25th of December. They are indeed special days, yet they are not prefered for date of marriage. This may be caused by their guest availability. Many couples would have to consider when can their guest attend. If they have a wedding, they would want guest to be available and show up. Many would like to spend time with their families in those public holidays. However, having a wedding in those dates is still possible. That is why the number is not zero."),
                                            p("It is also interesting to point out how the curve moves along similarly to how the average marriage occurrence by month graph is shaped. It seems that as the season changes the preference for each date of the month also changes overtime.")
                                          )
                                          )
                               ))
                      ),
           navbarMenu(
             # add tab name
             "Compare",
             # again, listed to differenctiate discussion
                      tabPanel("Compare by States",
                               tabsetPanel(
                                 tabPanel("Summary",
                                          fluidPage(
                                            h2("Compare Average Numbers by States 2008 - 2018"),
                                            sidebarLayout(position = "right",
                                              sidebarPanel(width = 3,
                                                           selectInput("type3", "Select Colour Format", choices = list("Marriage" = 1, "Civil Celebrant" = 2))
                                              ),
                                              mainPanel(

                                                leafletOutput("plot12"),
                                                br(),
                                                p("This should show the average of each state and ranked based on the given variables."),
                                                p("We can see in the Marriage output map that New South Wales have the highest average yearly number of marriage compared to other states. When changing to the Civil Celebrant output, New South Wales also have the highest number of people who uses Civil Celebrant as their marriage method.")
                                                )
                                            )
                                          )
                                          ),
                                 tabPanel("Marriage",
                                          fluidPage(
                                            h2("Marriage by States"),
                                            plotOutput("plot9"),
                                            p("We present a line representation of the number of marriages within the states and territory. Northern Territory has the lowest of all time and New South Wales have the highest of all time. Overall, there seems to be no changes to the number of marriages within the states. However, Queensland seems to have a decreasing line over the years. It is the most noticeable of tall states. In 2017, there seems to be a slight decrease, although for NSW it is quite significant, yet ACT seems to have an increasing spike. Other than a few, there seems no common spikes that is related to all the states and territories.")
                                          )
                                          ),
                                 tabPanel("Civil Celebrant",
                                          fluidPage(
                                            h2("Civil Celebrants by States"),
                                            plotOutput("plot10"),
                                            p("As previous analysis, civil celebrant number has increased in the past few years. We see by state that, though sometimes decrease, their movement seems to overall increase. An interesting significant increase in VIC 2015 to 2016 and it is not followed by a significant decrease means that there is a change in behaviour in VIC. Though not as much another significant increase is in NSW from 2017 to 2018. The movement of these lines ra very similar to the movement of total marriage above. This may be because they are used in most of the weddings in the states and territories, making their movement very similar.")
                                          )
                                          )
                               )
                               ),
                      tabPanel("Compare by Country",
                               fluidPage(
                                 h2("Compare Marriage Rate by Countries"),
                                 leafletOutput("plot11"),
                                 br(),
                                 p("Visual representation of average rates per 1000 people. Australia is deemed to be in medium level as Turkey has the darker colors (OECD, n.d.). Since it is an average, there is not much to see other than how the marriage rates compared to one another. Overall, there seems to be no trend going on that affect all the countries."),
                                 p("We then decide to ask ourselves which countries are have their growth increased and which one decrease from 2008 to 2017. Using RStudio, we calculated the change in rate by subtracting 2017 rate with 2008 rate. In the result, Australia’s rate is negative with -0.9, which is in accordance to our previous analysis. Hungary has the highest growth since 2008 with 1.2 and Turkey has the lowest growth with -1.9 rate change. This is interesting since Turkey has the highest average of 7.92 and Slovenia with the lowest of 3.19. Australia has an average of 5.18."),
                                 br(),
                                 p("OECD. (n.d.). OECD Family Database. Retrieved from http://www.oecd.org/els/family/database.htm")
                               )
                               )
                      )
)







