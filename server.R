#server.R

shinyServer(function(input, output,session) {
  
  # UI ----------------------------------------------------------------------
  
  USER <- reactiveValues(Logged = FALSE)
  
  ui1 <- function(){
    tagList(
      div(id = "login",
          wellPanel(style = "position: absolute; width: 45%; left: 11%; box-shadow: 10px 10px 15px grey;",
                    selectizeInput("ID", "Vali ettevõte:", NULL),
                    actionButton("submit", "Edasi")))
    )}
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$ID)) {
        if (input$submit > 0) {
          USER$Logged <- TRUE
        }
      }
    }
  })
  
  mina_koond <- reactive({
    andmed_filter %>% 
      filter(nimi == input$ID, aasta == ifelse(is.null(input$aasta), 2020, input$aasta))
  })
  
  observe({
    if (USER$Logged == FALSE) {
      updateSelectizeInput(session = session, inputId = 'ID', choices = c('', ettevotted), 
                           server = TRUE, 
                           options = list(onDropdownOpen = I('function() {this.clear();}'))
      )
      output$page <- renderUI({
        shinyjs::addCssClass(selector = "form.well", class = "hidden")
        div(class = "outer",do.call(bootstrapPage,c("",ui1())))#)
      })
    }
    if (USER$Logged == TRUE) {
      shinyjs::removeCssClass(selector = "form.well", class = "hidden")
      output$side <- renderUI({
        maakond1 <- isolate(mina_koond()$maakond)
        plusmiinus <- "\u00b1"
        Encoding(plusmiinus) <- "UTF-8"
        if (is.na(maakond1)) {
          maakonnad1 <- maakonnad
        } else {
          maakonnad1 <- maakonnad[maakonnad != maakond1]
        }
        aastad <- andmed_filter %>% filter(nimi == input$ID) %>% pull(aasta)
        tagList(
          h2("Sarnasuse näitajad"),
          useSweetAlert(),
          checkboxInput("sektor", label = "Sama sektor", value = FALSE),
          checkboxInput("emtak2", label = "Sama EMTAK2", value = FALSE),
          checkboxInput("maakond", label = "Sama maakond", value = FALSE),
          conditionalPanel(condition = "input.maakond == true",
                           selectInput("maakond2", "lisa ka", maakonnad1, multiple = TRUE)),
          checkboxInput("kaive", label = "Sarnane käive", value = FALSE),
          conditionalPanel(condition = "input.kaive == true",
                           numericInputIcon("dkaive", NULL, value = 20, min = 0,
                                            max = 100, icon = list(plusmiinus, icon("percent")), 
                                            help_text = "Arv peaks jääma 0 ja 100% vahele.")),
          checkboxInput("tootajad", label = "Sarnane töötajate arv", value = FALSE),
          conditionalPanel(condition = "input.tootajad == true",
                           numericInputIcon("dtootajad", NULL, value = 20, min = 0, 
                                            max = 100, icon = list(plusmiinus, icon("percent")), 
                                            help_text = "Arv peaks jääma 0 ja 100% vahele.")),
          conditionalPanel(condition = "input.kaive == true || input.tootajad == true",
                           selectInput("aasta", "Vali võrreldav aasta", aastad, selected = 2020)),
          actionButton("leia", "LEIA SARNASED"),
          hr(),
          htmlOutput("nouded")
        )
      })
      output$page <- renderUI({
        tagList(
          htmlOutput("seletus")
        )
      })
    }
  })
  observeEvent(input$leia, {
    output$page <- renderUI({
      tagList(
        h3("Sarnased ettevõtted"),
        htmlOutput("nimekiri"),
        br(),
        p("Vaata ettevõtteid lähemalt ",
          a("ajaloo võrdluse rakendusest", href="https://annegrete.ee/shiny/benchmarking_EMTA/", target="_blank"),"."),
        hr(),
        plotlyOutput("jooniskaive"),
        plotlyOutput("joonistootajad"),
        plotlyOutput("jooniskaive_tootaja"),
        plotlyOutput("joonistmaksud_tootaja"),
        plotlyOutput("joonisrmaksud_kaive"),
        plotlyOutput("joonistmaksud_kaive"),
        plotlyOutput("joonisrmaksud"),
        plotlyOutput("joonistmaksud"),
        htmlOutput("seletus")
      )
    })
  })
  
  # Jooniste andmed ------------------------------------------------------------------
  
  sarnased_reg <- eventReactive(input$leia, {
    #leia ettevõtted, kes on sarnased
    andmed_temp <- andmed_filter %>% filter(registrikood != mina_koond()$registrikood, aasta == input$aasta)
    if (input$sektor) {
      if (!is.na(mina_koond()$emtaktahttekst)){
        andmed_temp <- andmed_temp %>% filter(emtaktahttekst == mina_koond()$emtaktahttekst)
      }
    } 
    if (input$emtak2) {
      if (!is.na(mina_koond()$emtak2tekst)) {
        andmed_temp <- andmed_temp %>% filter(emtak2tekst == mina_koond()$emtak2tekst)
      }
    }
    if (input$maakond) {
      if(!is.na(mina_koond()$maakond) & length(input$maakond2) != 0) {
        andmed_temp <- andmed_temp %>% filter(maakond %in% c(mina_koond()$maakond, input$maakond2))
      }
    } 
    if (input$kaive) andmed_temp <- andmed_temp %>% 
        filter(kaive >= mina_koond()$kaive*(1-input$dkaive/100), 
               kaive <= mina_koond()$kaive*(1+input$dkaive/100))
    if (input$tootajad) andmed_temp <- andmed_temp %>% 
        filter(tootajad >= mina_koond()$tootajad*(1-input$dtootajad/100), 
               tootajad <= mina_koond()$tootajad*(1+input$dtootajad/100))
    andmed_temp %>% select(nimi, registrikood) %>% arrange(nimi)

  })
  andmed_joonis <- reactive({
    req(sarnased_reg())
    sarnased <- andmed_emta %>% 
      filter(registrikood %in% sarnased_reg()$registrikood) %>% 
      group_by(aeg) %>% 
      summarise_at(vars(rmaksud:tootajad, rmaksud_kaive:tmaksud_tootaja), mean, na.rm = TRUE) %>% 
      mutate(grupp = "Sarnased ettevõtted")
    mina <- andmed_emta %>% 
      filter(registrikood == mina_koond()$registrikood) %>% 
      mutate(grupp = mina_koond()$nimi) %>% 
      select(names(sarnased))
    rbind(sarnased, mina)
  })
  
  # Tekts outputid ----------------------------------------------------------
  
  output$nouded <- renderUI({
    str <- "Ettevõtted on sarnased, kui on täidetud järgmised nõuded:"
    if (input$sektor) {
      if (!is.na(mina_koond()$emtaktahttekst)){
        str <- paste(str, paste("<b>Sektor:</b>", mina_koond()$emtaktahttekst), sep = '<br/>')
      } else {
        str <- paste(str, "Andmetes ei ole ettevõtte <b>sektorit</b>, seda ei arvestata filtreerimisel.", sep = '<br/>')
      }
    } 
    if (input$emtak2) {
      if (!is.na(mina_koond()$emtak2tekst)) {
        str <- paste(str, paste("<b>EMTAK2:</b>", mina_koond()$emtak2tekst), sep = '<br/>')
      } else {
        str <- paste(str, "Andmetes ei ole ettevõtte <b>EMTAK2</b>, seda ei arvestata filtreerimisel.", sep = '<br/>')
      }
    }
    if (input$maakond) {
      if(!is.na(mina_koond()$maakond)) {
        str <- paste(str, paste("<b>Maakond:</b>", paste(c(mina_koond()$maakond, input$maakond2), collapse = " ")), sep = '<br/>')
      } else {
        str <- paste(str, "Andmetes ei ole ettevõtte <b>maakonda</b>, filtreerimisel arvestatakse ainult lisatud maakondi.", sep = '<br/>')
      }
    } 
    if(input$kaive) str <- paste(str, paste("<b>Käive:</b>", 
                                            paste(prettyNum(mina_koond()$kaive*(1-input$dkaive/100), big.mark = " "), 
                                                  prettyNum(mina_koond()$kaive*(1+input$dkaive/100), big.mark = " "), sep = " - ")), sep = '<br/>')
    if(input$tootajad) str <- paste(str, paste("<b>Töötajad:</b>",
                                               paste(mina_koond()$tootajad*(1-input$dtootajad/100), 
                                                     mina_koond()$tootajad*(1+input$dtootajad/100), sep = " - ")), sep = '<br/>')
    if(input$kaive | input$tootajad) str <- paste(str, paste("Võrdlusaastaks on", input$aasta), sep = '<br/>')
    
    HTML(str)
  })
  
  output$seletus <- renderUI({
    HTML("<h3> Lisainfo</h3><br/>
    Selles rakendused saab valida äriühingutest ettevõtte, mis deklareeris käivet/töötajaid 
         detsember 2019 - november 2020 (2020 I - IV kvartal 
         <a href = 'http://www.emta.ee/et/kontaktid-ja-ametist/maksulaekumine-statistika/tasutud-maksud-kaive-ja-tootajate-arv' target='_blank'>EMTA andmetes</a>). 
         EMTA andmetele on lisatud 
         <a href='https://www.stat.ee/et/esita-andmeid/andmete-esitamisest/ettevotete-uuringud/majandusuksuste-klassifitseerimise-abiinfo' target='_blank'>Statistikaametist</a> 
         (01.12.2020 seis) ettevõtete sektor ja EMTAK2 tase. Kui Statistikameti andmetes 
         ei ole seda ettevõtet, siis EMTAK2 on puudu ja sektor on võetud EMTA andmetest. <br/>
         Ettevõttele saab otsida enda sarnaseid ettevõtteid. Sarnasust saab defineerida 
         sama sektori, sama EMTAK2 taseme, sama maakonna (ja nt naabermaakondade), 
         aastase käibe ja/või aasta keskmise töötajate arvu järgi. Käibe ja töötajate 
         arvu korral saab valida ka võrreldav aasta. <br/>
         Kui on määratud sarnased ettevõttete nõuded, siis rakendus annab 
         sarnaste ettevõtete nimed ning joonistab graafikud valitud ettevõtte ja
         sarnaste ettevõtete keskmistest näitajatest.")
  })
  
  output$nimekiri <- renderUI({
    HTML(paste(sarnased_reg()$nimi, collapse = "<br/>"))
  })
  
  # Joonised ----------------------------------------------------------------
  output$jooniskaive <- renderPlotly({
    skaala <- kymne_aste(andmed_joonis()$kaive)
    p <- ggplot(andmed_joonis(), 
                aes(aeg, kaive/skaala$div, color = grupp, group = grupp,
                    text = paste0(grupp, "\n", aeg, "\nKäive: ", 
                                  prettyNum(kaive, big.mark = " "), " €"))) + 
      geom_line() + 
      geom_point(size = 0.8) + 
      theme_bw() + 
      labs(title = "Käive", x = "", y = paste0("€ ", skaala$label)) + 
      guides(color = guide_legend(title = NULL)) + 
      scale_x_yearqtr(format = "%Y Q%q") + 
      expand_limits(y = 0) 
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$joonistootajad <- renderPlotly({
    p <- ggplot(andmed_joonis(), 
                aes(aeg, tootajad, color = grupp, group = grupp,
                    text = paste0(grupp, "\n", aeg, "\nTöötajate arv: ", tootajad))) + 
      geom_line() + 
      geom_point(size = 0.8) + 
      theme_bw() + 
      labs(title = "Töötajate arv", x = "", y = "") + 
      guides(color = guide_legend(title = NULL)) + 
      scale_x_yearqtr(format = "%Y Q%q") + 
      expand_limits(y = 0) 
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$jooniskaive_tootaja <- renderPlotly({
    skaala <- kymne_aste(andmed_joonis()$kaive_tootaja)
    p <- ggplot(andmed_joonis(), 
                aes(aeg, kaive_tootaja/skaala$div, color = grupp, group = grupp,
                    text = paste0(grupp, "\n", aeg, "\nKäive töötaja kohta: ", 
                                  prettyNum(kaive_tootaja, big.mark = " "), " €"))) + 
      geom_line() + 
      geom_point(size = 0.8) + 
      theme_bw() + 
      labs(title = "Käive töötaja kohta", x = "", y = paste0("€ ", skaala$label)) + 
      guides(color = guide_legend(title = NULL)) + 
      scale_x_yearqtr(format = "%Y Q%q") + 
      expand_limits(y = 0) 
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$joonistmaksud_tootaja <- renderPlotly({
    skaala <- kymne_aste(andmed_joonis()$tmaksud_tootaja)
    p <- ggplot(andmed_joonis(), 
                aes(aeg, tmaksud_tootaja/skaala$div, color = grupp, group = grupp,
                    text = paste0(grupp, "\n", aeg, "\nTööjõumaksud töötaja kohta: ", 
                                  prettyNum(tmaksud_tootaja, big.mark = " "), " €"))) + 
      geom_line() + 
      geom_point(size = 0.8) + 
      theme_bw() + 
      labs(title = "Tööjõumaksud töötaja kohta", x = "", y = paste0("€ ", skaala$label)) + 
      guides(color = guide_legend(title = NULL)) + 
      scale_x_yearqtr(format = "%Y Q%q") + 
      expand_limits(y = 0) 
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$joonisrmaksud_kaive <- renderPlotly({
    p <- ggplot(andmed_joonis(), 
                aes(aeg, rmaksud_kaive, color = grupp, group = grupp,
                    text = paste0(grupp, "\n", aeg, "\nRiiklikud maksud käibest: ", 
                                  rmaksud_kaive, "%"))) + 
      geom_line() + 
      geom_point(size = 0.8) + 
      theme_bw() + 
      labs(title = "Riiklikud maksud käibest", x = "", y = "%") + 
      guides(color = guide_legend(title = NULL)) + 
      scale_x_yearqtr(format = "%Y Q%q") + 
      expand_limits(y = 0) 
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$joonistmaksud_kaive <- renderPlotly({
    p <- ggplot(andmed_joonis(), 
                aes(aeg, tmaksud_kaive, color = grupp, group = grupp,
                    text = paste0(grupp, "\n", aeg, "\nTööjõumaksud käibest: ", 
                                  tmaksud_kaive, "%"))) + 
      geom_line() + 
      geom_point(size = 0.8) + 
      theme_bw() + 
      labs(title = "Tööjõumaksud käibest", x = "", y ="%") + 
      guides(color = guide_legend(title = NULL)) + 
      scale_x_yearqtr(format = "%Y Q%q") + 
      expand_limits(y = 0) 
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$joonisrmaksud <- renderPlotly({
    skaala <- kymne_aste(andmed_joonis()$rmaksud)
    p <- ggplot(andmed_joonis(), 
                aes(aeg, rmaksud/skaala$div, color = grupp, group = grupp,
                    text = paste0(grupp, "\n", aeg, "\nRiiklikud maksud: ", 
                                  prettyNum(rmaksud, big.mark = " "), " €"))) + 
      geom_line() + 
      geom_point(size = 0.8) + 
      theme_bw() + 
      labs(title = "Riiklikud maksud", x = "", y = paste0("€ ", skaala$label)) + 
      guides(color = guide_legend(title = NULL)) + 
      scale_x_yearqtr(format = "%Y Q%q") + 
      expand_limits(y = 0) 
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  output$joonistmaksud <- renderPlotly({
    skaala <- kymne_aste(andmed_joonis()$toomaksud)
    p <- ggplot(andmed_joonis(), 
                aes(aeg, toomaksud/skaala$div, color = grupp, group = grupp,
                    text = paste0(grupp, "\n", aeg, "\nTööjõumaksud: ", 
                                  prettyNum(toomaksud, big.mark = " "), " €"))) + 
      geom_line() + 
      geom_point(size = 0.8) + 
      theme_bw() + 
      labs(title = "Tööjõumaksud", x = "", y = paste0("€ ", skaala$label)) + 
      guides(color = guide_legend(title = NULL)) + 
      scale_x_yearqtr(format = "%Y Q%q") + 
      expand_limits(y = 0) 
    ggplotly(p, tooltip = "text") #%>% config(displayModeBar = F)
  })
  
  
  
  
})
