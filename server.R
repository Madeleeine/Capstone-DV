server <- function(input,output){
  
  output$valuebox_1 <- renderValueBox({
    g1<-df_crop_iso %>% 
      filter(Country==input$input_3) %>% 
      group_by(Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      filter(Subject=="Maize") %>% 
      pull(Total)
    valueBox(
      prettyNum(paste0(round(g1,1)), big.mark = ","), "Total Maize Produced ", icon = icon("seedling"),
      color = "purple"
    )
  })
  output$valuebox_2 <- renderValueBox({
    g2<-df_crop_iso %>% 
      filter(Country==input$input_3) %>% 
      group_by(Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      filter(Subject=="Wheat") %>% 
      pull(Total)
    valueBox(
      prettyNum(paste0(round(g2,1)),big.mark = ","), "Total Wheat Produced ", icon = icon("leaf"),
      color = "red"
    )
  })
  output$valuebox_3 <- renderValueBox({
    g3<-df_crop_iso %>% 
      filter(Country==input$input_3) %>% 
      group_by(Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      filter(Subject=="Soybean") %>% 
      pull(Total)
    valueBox(
      prettyNum(paste0(round(g3,1)),big.mark = ","), "Total Soybean Produced ", icon = icon("envira"),
      color = "green"
    )
  })
  output$valuebox_4 <- renderValueBox({
    g4<-df_crop_iso %>% 
      filter(Country==input$input_3) %>% 
      group_by(Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      filter(Subject=="Rice") %>% 
      pull(Total)
    valueBox(
      prettyNum(paste0(round(g4,1)),big.mark = ","), "Total Rice Produced ", icon = icon("pagelines"),
      color = "blue"
    )
  })
  
  output$rankbox_1 <- renderValueBox({
    r1<-df_crop_iso %>% 
      filter(Year==2019) %>% 
      filter(Subject=="Wheat") %>% 
      mutate(rank = dense_rank(desc(Total))) %>% 
      filter(Country==input$input_3) %>% 
      pull(rank)
    valueBox(
      paste0(round(r1,1)), "Wheat Producer", icon = icon("medal"),
      color = "yellow"
    )
  })
  
  output$rankbox_2 <- renderValueBox({
    r1<-df_crop_iso %>% 
      filter(Year==2019) %>% 
      filter(Subject=="Rice") %>% 
      mutate(rank = dense_rank(desc(Total))) %>% 
      filter(Country==input$input_3) %>% 
      pull(rank)
    valueBox(
      paste0(round(r1,1)), "Rice Producer", icon = icon("medal"),
      color = "yellow"
    )
  })
  
  output$rankbox_3 <- renderValueBox({
    r1<-df_crop_iso %>% 
      filter(Year==2019) %>% 
      filter(Subject=="Maize") %>% 
      mutate(rank = dense_rank(desc(Total))) %>% 
      filter(Country==input$input_3) %>% 
      pull(rank)
    valueBox(
      paste0(round(r1,1)), "Maize Producer", icon = icon("medal"),
      color = "yellow"
    )
  })
  
  output$rankbox_4 <- renderValueBox({
    r1<-df_crop_iso %>% 
      filter(Year==2019) %>% 
      filter(Subject=="Soybean") %>% 
      mutate(rank = dense_rank(desc(Total))) %>% 
      filter(Country==input$input_3) %>% 
      pull(rank)
    valueBox(
      paste0(round(r1,1)), "Soy Producer", icon = icon("medal"),
      color = "yellow"
    )
  })
  
  
  output$rankbox_5 <- renderValueBox({
    r1<-df_meat_iso %>% 
      filter(Year==2019) %>% 
      filter(Subject=="Beef") %>% 
      mutate(rank = dense_rank(desc(Total))) %>% 
      filter(Country==input$input_4) %>% 
      pull(rank)
    valueBox(
      paste0(round(r1,1)), "Beef Producer", icon = icon("medal"),
      color = "yellow"
    )
  })
  
  output$rankbox_6 <- renderValueBox({
    r1<-df_meat_iso%>% 
      filter(Year==2019) %>% 
      filter(Subject=="Poultry") %>% 
      mutate(rank = dense_rank(desc(Total))) %>% 
      filter(Country==input$input_4) %>% 
      pull(rank)
    valueBox(
      paste0(round(r1,1)), "Poultry Producer", icon = icon("medal"),
      color = "yellow"
    )
  })
  
  output$rankbox_7 <- renderValueBox({
    r1<-df_meat_iso %>% 
      filter(Year==2019) %>% 
      filter(Subject=="Pork") %>% 
      mutate(rank = dense_rank(desc(Total))) %>% 
      filter(Country==input$input_4) %>% 
      pull(rank)
    valueBox(
      paste0(round(r1,1)), "Poultry Producer", icon = icon("medal"),
      color = "yellow"
    )
  })
  
  output$rankbox_8 <- renderValueBox({
    r1<-df_meat_iso %>% 
      filter(Year==2019) %>% 
      filter(Subject=="Sheep") %>% 
      mutate(rank = dense_rank(desc(Total))) %>% 
      filter(Country==input$input_4) %>% 
      pull(rank)
    valueBox(paste0(round(r1,1)), "Sheep Producer", icon = icon("medal"),
             color = "yellow")
  })
  
  
  
  
  
  
  output$valuebox_5 <- renderValueBox({
    g5<-df_meat_iso %>% 
      filter(Country==input$input_4) %>% 
      group_by(Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      filter(Subject=="Beef") %>% 
      pull(Total)
    valueBox(
      prettyNum(paste0(round(g5,1)),big.mark = ",") ,"Total Beef Consumed ", icon = icon("bacon"),
      color = "purple"
    )
  })
  
  output$valuebox_6 <- renderValueBox({
    g6<-df_meat_iso %>% 
      filter(Country==input$input_4) %>% 
      group_by(Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      filter(Subject=="Poultry") %>% 
      pull(Total)
    valueBox(
      prettyNum(paste0(round(g6,1)),big.mark = ",") , "Total Poultry Consumed ", icon = icon("drumstick-bite"),
      color = "red"
    )
  })
  output$valuebox_7 <- renderValueBox({
    g7<-df_meat_iso %>% 
      filter(Country==input$input_4) %>% 
      group_by(Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      filter(Subject=="Sheep") %>% 
      pull(Total)
    valueBox( prettyNum(paste0(round(g7,1)),big.mark = ",") , "Total Sheep Consumed ", icon = icon("bacon"),
              color = "green")
  })
  output$valuebox_8 <- renderValueBox({
    g8<-df_meat_iso %>% 
      filter(Country==input$input_4) %>% 
      group_by(Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      filter(Subject=="Pork") %>% 
      pull(Total)
    valueBox(
      prettyNum(paste0(round(g8,1)),big.mark = ",") , "Total Pork Consumed ", icon = icon("piggy-bank"),
      color = "blue"
    )
  })
  

  
  
  output$plot_p11<-renderPlotly({
    p11<-df_meat_iso %>% 
      filter(Country==input$input_4) %>% 
      mutate(keterangan = paste0(Subject,"\n Total Consumed : ",round(Total,1)," thousand tonnes")) %>% 
      ggplot(aes(x=Year,y=Total,fill=Subject,color=Subject,text=keterangan,group=1))+geom_line()+theme_dark()+
      scale_x_continuous(breaks = seq(1990, 2019, 5))+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),axis.text.y  = element_blank(),axis.title.x = element_blank(),
            panel.background = element_rect(fill = '#f4f4f4'),
            plot.background=element_rect(fill="#f4f4f4", colour=NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.ticks = element_blank(),legend.title =element_blank())+scale_color_pander()
    ggplotly(p11,tooltip="text")
  })
  
  output$plot_p12<-renderPlotly({
    p12<-df_crop_iso %>% 
      filter(Country==input$input_3) %>% 
      mutate(keterangan = paste0(Subject,"\n Total Produced : ",round(Total,1)," thousand tonnes")) %>% 
      ggplot(aes(x=Year,y=Total,color=Subject,text=keterangan,group=1))+geom_line()+theme_dark()+
      scale_x_continuous(breaks = seq(1990, 2019, 5))+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),axis.text.y  = element_blank(),axis.title.x = element_blank(),
            panel.background = element_rect(fill = '#f4f4f4'),
            plot.background=element_rect(fill="#f4f4f4", colour=NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.ticks = element_blank(),legend.title =element_blank())
    
    ggplotly(p12,tooltip="text")
    
    
  })
  output$range <- renderPrint({ input$slider2 })
  output$check<- renderPrint({ input$checkGroup1})
  output$value <- renderPrint({ input$slider1 })
  
  
  
  output$plot_p13<-renderPlotly({
    negara<-df_meat_iso %>% 
      filter(Year%in%input$slider2) %>% 
      filter(Subject%in%input$checkGroup) %>% 
      group_by(Country) %>% 
      summarise(jumlah=sum(Total)) %>% 
      top_n(input$slider1,jumlah) %>% 
      pull(Country)
    Jumlah<-df_meat_iso %>% 
      filter(Year%in%input$slider2) %>% 
      filter(Subject%in%input$checkGroup) %>% 
      group_by(Country) %>% 
      summarise(jumlah=sum(Total)) %>% 
      summarise(mean=mean(jumlah)) %>% 
      pull(mean)
    
    
    p13<-df_meat_iso %>% 
      filter(Country %in% negara,Year%in%input$slider2) %>% 
      filter(Subject%in%input$checkGroup) %>% 
      group_by(Country,Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      mutate(keterangan = paste0(Subject,"\n Total Produced : ",round(Total,1)," thousand tonnes")) %>%
      ggplot(aes(fill=Subject,x=Total,y=reorder(Country,Total)))+
      geom_bar(position="stack",stat="identity",aes(text=keterangan))+
      theme_classic()+
      scale_fill_economist()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.x = element_blank(),axis.title.y = element_blank(),
            panel.background = element_rect(fill = '#f4f4f4'),
            plot.background=element_rect(fill="#f4f4f4", colour=NA),legend.title =element_blank())
    
    ggplotly(p13,tooltip = "text")
  })
  
  
  
  output$range <- renderPrint({ input$slider3 })
  output$check<- renderPrint({ input$checkGroup})
  output$value <- renderPrint({ input$slider4 })
  
  
  output$plot_p14<-renderPlotly({
    negara<-df_meat_iso %>% 
      filter(Year%in%input$slider3) %>% 
      filter(Subject%in%input$checkGroup) %>% 
      group_by(Country) %>% 
      summarise(jumlah=sum(Total)) %>% 
      top_n(input$slider4,jumlah) %>% 
      pull(Country)
    Jumlah<-df_meat_iso %>% 
      filter(Year%in%input$slider3) %>% 
      filter(Subject%in%input$checkGroup1) %>% 
      group_by(Country) %>% 
      summarise(jumlah=sum(Total)) %>% 
      summarise(mean=mean(jumlah)) %>% 
      pull(mean)
    
    
    p14<-df_crop_iso %>% 
      filter(Country %in% negara,Year%in%input$slider3) %>% 
      filter(Subject%in%input$checkGroup1) %>% 
      group_by(Country,Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      mutate(keterangan = paste0(Subject,"\n Total Produced : ",round(Total,1)," thousand tonnes")) %>%
      ggplot(aes(fill=Subject,x=Total,y=reorder(Country,Total)))+
      geom_bar(position="stack",stat="identity",aes(text=keterangan))+
      geom_vline(xintercept=Jumlah,color="red",alpha=0.5)+
      geom_text(x=Jumlah, label="\nGlobal Mean", y=4, colour="black",size = 3)+
      theme_classic()+
      scale_fill_economist()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.x = element_blank(),axis.title.y = element_blank(),
            panel.background = element_rect(fill = '#f4f4f4'),
            plot.background=element_rect(fill="#f4f4f4", colour=NA),legend.title =element_blank())
    
    ggplotly(p14,tooltip = "text")
  })
  output$value<- renderPrint({ input$checkmap})
  output$value <- renderPrint({ input$slidermap })
  
  output$plot_p15<-renderLeaflet({
    df_1 %>% 
      mutate(log_val=2*log(Total)) %>% 
      filter(Subject==input$checkmap) %>% 
      filter(Year==input$slidermap) %>% 
      leaflet()%>% 
      addTiles()%>% 
      addCircleMarkers(radius=~log_val,color="blue",label=~paste0(Country,"\n",":",Total,"\n","Thousand Tonnes")) 
    
  })
  
  
  output$value<- renderPrint({ input$checkmap2})
  output$value <- renderPrint({ input$slidermap2 })
  
  output$plot_p16<-renderLeaflet({
    df %>% 
      mutate(log_val2=2*log(Total)) %>% 
      filter(Subject==input$checkmap2) %>% 
      filter(Year==input$slidermap2) %>% 
      leaflet()%>% 
      addTiles()%>% 
      addCircleMarkers(radius=~log_val2,color="blue",label=~paste0(Country,"\n",":",Total,"\n","Thousand Tonnes")) 
    
  })
  output$infobox_wheat<-renderInfoBox({
    w<-df_crop_iso %>% 
      group_by(Country,Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      arrange(desc(Total)) %>% 
      group_by(Subject) %>% 
      top_n(1,Total) %>% 
      filter(Subject=="Wheat")
    
    infoBox(
      "Top Wheat", prettyNum(paste0(round(w$Total,0)),big.mark = ",") ,paste0(w$Country), icon = icon("trophy"),
      color = "purple",fill=TRUE
    )
  })
  output$infobox_soybean<-renderInfoBox({
    s<-df_crop_iso %>% 
      group_by(Country,Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      arrange(desc(Total)) %>% 
      group_by(Subject) %>% 
      top_n(1,Total) %>% 
      filter(Subject=="Soybean")
    
    infoBox(
      "Top Soy",prettyNum(paste0(round(s$Total,0)),big.mark = ","),paste0(s$Country), icon = icon("trophy"),
      color = "blue",fill=TRUE
    )
  })
  output$infobox_rice<-renderInfoBox({
    r<-df_crop_iso %>% 
      group_by(Country,Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      arrange(desc(Total)) %>% 
      group_by(Subject) %>% 
      top_n(1,Total) %>% 
      filter(Subject=="Rice")
    
    infoBox(
      "Top Rice", prettyNum(paste0(round(r$Total,0)),big.mark = ","),paste0(r$Country), icon = icon("trophy"),
      color = "red",fill=TRUE
    )
  })
  
  output$infobox_maize<-renderInfoBox({
    m<-df_crop_iso %>% 
      group_by(Country,Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      arrange(desc(Total)) %>% 
      group_by(Subject) %>% 
      top_n(1,Total) %>% 
      filter(Subject=="Maize")
    
    infoBox(
      "Top Maize", prettyNum(paste0(round(m$Total,0)),big.mark = ","),paste0(m$Country), icon = icon("trophy"),
      color = "yellow",fill=TRUE
    )
  })
  
  output$infobox_beef<-renderInfoBox({
    b<-df_meat_iso %>% 
      group_by(Country,Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      arrange(desc(Total)) %>% 
      group_by(Subject) %>% 
      top_n(1,Total) %>% 
      filter(Subject=="Beef")
    
    infoBox(
      "Top Beef", prettyNum(paste0(round(b$Total,0)),big.mark = ","),paste0(b$Country), icon = icon("trophy"),
      color = "blue",fill=TRUE
    )
  })
  output$infobox_poultry<-renderInfoBox({
    p<-df_meat_iso %>% 
      group_by(Country,Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      arrange(desc(Total)) %>% 
      group_by(Subject) %>% 
      top_n(1,Total) %>% 
      filter(Subject=="Poultry")
    
    infoBox(
      "Top Poultry", prettyNum(paste0(round(p$Total,0)),big.mark = ","),paste0(p$Country), icon = icon("trophy"),
      color = "red", fill=TRUE
    )
  })
  
  output$infobox_sheep<-renderInfoBox({
    S<-df_meat_iso %>% 
      group_by(Country,Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      arrange(desc(Total)) %>% 
      group_by(Subject) %>% 
      top_n(1,Total) %>% 
      filter(Subject=="Sheep")
    
    infoBox(
      "Top Sheep", prettyNum(paste0(round(S$Total,0)),big.mark = ","),paste0(S$Country), icon = icon("trophy"),
      color = "red", fill=TRUE
    )
  })
  
  
  output$infobox_pork<-renderInfoBox({
    P<-df_meat_iso %>% 
      group_by(Country,Subject) %>% 
      summarise(Total=sum(Total)) %>% 
      arrange(desc(Total)) %>% 
      group_by(Subject) %>% 
      top_n(1,Total) %>% 
      filter(Subject=="Pork")
    
    infoBox(
      "Top Pork", prettyNum(paste0(round(P$Total,0)),big.mark = ","),paste0(P$Country), icon = icon("trophy"),
      color = "red",fill=TRUE
    )
  })
  
  
  output$plot_general<-renderPlotly({
    plot_general<-df_general_crop%>% 
      inner_join(df_general_meat,by=c("Country","Year")) %>% 
      pivot_longer(cols = c("Total_Crop","Total_meat")) %>% 
      mutate(name=sub("Total_Crop","Total Crop",name)) %>% 
      mutate(name=sub("Total_meat","Total Meat",name)) %>% 
      mutate(keterangan=paste0(name, " : ",value," Thousand Tonnes")) %>% 
      filter(Year==input$input_gen) %>% 
      ggplot(aes(value,reorder(Country,value)))+geom_col(aes(fill=name,text=keterangan))+
      theme_classic()+
      scale_fill_economist()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.x = element_blank(),axis.title.y = element_blank(),
            panel.background = element_rect(fill = '#f4f4f4'),
            plot.background=element_rect(fill="#f4f4f4", colour=NA),legend.title =element_blank())
    
    ggplotly(plot_general,tooltip="text")
  })
  
output$leaflet_general<-renderLeaflet({
  general<-df_general_crop%>% 
    inner_join(df_general_meat,by=c("Country","Year")) %>% 
    mutate(Total=Total_meat+Total_Crop) %>% 
    filter(Year==input$input_gen1) %>% 
    mutate(rank=dense_rank(Total))
  
  
  df_2 <- world.cities %>%
    filter(capital == 1) %>%
    dplyr::select(Country= country.etc, lat, lng = long) %>%
    left_join(general, by = "Country") %>% 
    drop_na()
  
  
  
  df_2 %>% 
    mutate(log_val=1.5*log(Total)) %>%  
    leaflet()%>% 
    addTiles()%>% 
    addCircleMarkers(radius=~log_val,color="blue",label=~paste0(Country,"\n",":",Total,"\n","Thousand Tonnes")) 
})
}