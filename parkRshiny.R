#install.packages('shiny')
#install.packages('leaflet')
#install.packages('dplyr')

library(shiny)
library(leaflet)
library(dplyr)

### 데이터 불러오기 
score <- read.csv(file = 'C:/Rshiny/score_weight.csv')
park <- read.csv(file = 'C:/Rshiny/df_drop.csv')

### score와 park merge
park_latlon <- park %>% select(num,l_address,lat,lon) # 위도 경도 공원번호 공원주소만 가져옴
score <- rename(score, num = park_num)      # merge를 하기위해 column명을 맞춰줌
score_latlon <- merge(score, park_latlon, by = 'num') #merege <- 공원번호 기준 
score_latlon %>% head(1)

# 영어 columns명 한글화 
score_rename<- rename(score_latlon, 공원번호 = num, 공원이름 = name, 면적_점수 = area_score, 공원주소 = l_address,
                      전체점수 = total,
                      공원접근성 = access_score, 면적_점수 = area_score, 거주지근접도_점수 = population_score, 타슈_점수 = tashu_score,
                      공원안전성 = safe_score, 조명등_점수 = light_score, 파출소근접도_점수 = police_score,
                      휴게 = rest_score, 카페_점수 = cafe_score,음식점_점수 = food_score,편의점_점수 = convenience_score,화장실_점수 = toilet_score, 위도 =lat,경도=lon)

score_rename %>% head(1)
#필요없는 변수 제거 
score_rename <- score_rename[  ,c(-2,-3) ]
score_rename <- score_rename[order(score_rename$전체점수,decreasing=TRUE),] 
row.names(score_rename) <- NULL
score_rename <- score_rename[, c(1,2,16,
                                 15,12,3,11,8,
                                 13, 9, 10,
                                 14, 6, 5, 4, 7, 17, 18)]
################ 휴게
rest <- score_rename %>% select(공원번호,공원이름,공원주소,휴게,카페_점수,음식점_점수,편의점_점수,화장실_점수, 위도,경도) 
rest <- rest[order(rest$휴게,decreasing=TRUE),] 
row.names(rest) <- NULL

# 편의점
rest_convenience <- score_rename %>% select(공원번호,공원이름,공원주소,편의점_점수, 위도, 경도)
rest_convenience <- rest_convenience[order(rest_convenience$편의점_점수,decreasing=TRUE),] 
row.names(rest_convenience) <- NULL
# 카페
rest_cafe <- score_rename %>% select(공원번호,공원이름,공원주소,카페_점수, 위도,경도)
rest_cafe <- rest_cafe[order(rest_cafe$카페_점수,decreasing=TRUE),]
row.names(rest_cafe) <- NULL
# 음식점
rest_food <- score_rename %>% select(공원번호,공원이름,공원주소,음식점_점수,  위도,경도)
rest_food <- rest_food[order(rest_food$음식점_점수,decreasing=TRUE),]
row.names(rest_food) <- NULL
# 화장실
rest_toilet <- score_rename %>% select(공원번호,공원이름,공원주소, 화장실_점수, 위도,경도)
rest_toilet <- rest_toilet[order(rest_toilet$화장실_점수,decreasing=TRUE),]
row.names(rest_toilet) <- NULL

################# 안전
safe <- score_rename %>% select(공원번호,공원이름,공원주소,공원안전성,파출소근접도_점수, 조명등_점수, 위도,경도)
safe <- safe[order(safe$공원안전성,decreasing=TRUE),] 
View(safe)
row.names(safe) <- NULL

#파출소
safe_police <- score_rename %>% select(공원번호,공원이름,공원주소,파출소근접도_점수, 위도,경도)
safe_police <- safe_police[order(safe_police$파출소근접도_점수,decreasing=TRUE),]
row.names(safe_police) <- NULL
#조명등
safe_light <- score_rename %>% select(공원번호,공원이름,공원주소,조명등_점수, 위도,경도)
safe_light <- safe_light[order(safe_light$조명등_점수,decreasing=TRUE),]
row.names(safe_light) <- NULL

################# 활동
play <- score_rename %>% select(공원번호,공원이름,공원주소,공원접근성,면적_점수,거주지근접도_점수, 타슈_점수, 위도,경도)
play <- play[order(play$공원접근성,decreasing=TRUE),] 
row.names(play) <- NULL


area<- play %>% select(공원번호,공원이름,공원주소,면적_점수,위도,경도)
area <- area[order(area$면적_점수,decreasing=TRUE),] 
row.names(area) <- NULL

play_population <- score_rename %>% select(공원번호,공원이름,공원주소,거주지근접도_점수, 위도,경도)
play_population <- play_population[order(play_population$거주지근접도_점수,decreasing=TRUE),]
row.names(play_population) <- NULL

play_tashu <- score_rename %>% select(공원번호,공원이름,공원주소,타슈_점수,위도,경도)
play_tashu <- play_tashu[order(play_tashu$타슈_점수,decreasing=TRUE),]
row.names(play_tashu) <- NULL

#ui
ui <- fluidPage(
  
  # App title ----
  titlePanel("대전 공원 평가지표 정보"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "caption",
                label = "Caption:",
                value = "대전 공원 정보"),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "평가 지표:",
                  choices = c("전체점수",
                              "접근성","접근성1 : 면적",'접근성2 : 거주지근접도' ,'접근성3 : 타슈',
                              '안전성', '안전성1 : 야외조명등', '안전성2 : 파출소_근접도',
                              '휴게','휴게1 : 카페','휴게2 : 음식점','휴게3 : 편의점','휴게4 : 화장실')),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 5)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Output: HTML table with requested number of observations ----
      leafletOutput("view", height = 600, width = '100%'),
      # Output: Verbatim text for data summary ----
      tableOutput("summary")
    )
  )
)
#server
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset,
           "전체점수" = score_rename,
           "접근성" = play, '접근성1 : 면적'= area, '접근성2 : 거주지근접도' = play_population, '접근성3 : 타슈' = play_tashu,
           '안전성' = safe, '안전성1 : 야외조명등' = safe_light, '안전성2 : 파출소_근접도' = safe_police,
           '휴게' = rest, '휴게1 : 카페' = rest_cafe, '휴게2 : 음식점' = rest_food, '휴게3 : 편의점' = rest_convenience,
           '휴게4 : 화장실' = rest_toilet
    )
  })
  
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('Hydda.Full') %>%
      setView(lat = 36.35053444968825 , lng = 127.38482942630057, zoom = 12) %>% 
      addTiles() %>% 
      addMarkers(data = datasetInput()[1:input$obs,], lat = ~ 위도, lng = ~ 경도, 
                 popup=paste("공원이름 : ",datasetInput()[1:input$obs,]$공원이름,"<br>",
                             "주소 : ",datasetInput()[1:input$obs,]$공원주소,"<br>",
                             "공원번호 : ",datasetInput()[1:input$obs,]$공원번호 ))   #내림차순, 오름차순
  })
  
}

shinyApp(ui, server)