# server.R

shinyServer(function(input, output, session) {

  # ---- company selector ----------------------------------------------------
  observeEvent(TRUE, {
    updateSelectizeInput(
      session = session,
      inputId = "company_id",
      choices = setNames(ettevotted$value, ettevotted$label),
      selected = "",
      server = TRUE,
      options = list(
        placeholder = "Alusta ettevõtte nime kirjutamist...",
        maxItems = 1,
        onDropdownOpen = I("function() {this.clear();}")
      )
    )
  })

  selected_company <- reactive({
    req(input$company_id)

    active_companies %>%
      filter(registrikood == input$company_id) %>%
      slice(1)
  })

   selected_company_years <- reactive({
    req(input$company_id)

    company_id <- as.character(input$company_id)

    years <- company_year_ds %>%
      filter(registrikood == company_id) %>%
      distinct(aasta) %>%
      collect() %>%
      filter(!is.na(aasta)) %>%
      arrange(desc(aasta)) %>%
      pull(aasta)

    req(length(years) > 0)

    years
  })

  selected_company_year <- reactive({
    req(input$company_id, input$aasta)

    company_id <- as.character(input$company_id)
    aasta_val <- as.integer(input$aasta)

    company_year_ds %>%
      filter(registrikood == company_id, aasta == aasta_val) %>%
      collect() %>%
      slice(1)
  })

  # ---- controls ------------------------------------------------------------
  output$similarity_controls <- renderUI({
    req(selected_company())

    company_years <- selected_company_years()

    plusmiinus <- "\u00b1"
    company_county <- selected_company()$maakond
    extra_counties <- if (is.na(company_county) || company_county == "") {
      maakonnad
    } else {
      setdiff(maakonnad, company_county)
    }

    tagList(
      h3("Sarnasuse näitajad"),
      checkboxInput("sektor", "Sama sektor", value = FALSE),
      checkboxInput("emtak2", "Sama EMTAK2", value = TRUE),
      checkboxInput("maakond", "Sama maakond", value = FALSE),
      conditionalPanel(
        condition = "input.maakond == true",
        selectInput("maakond2", "Lisa ka maakonnad", extra_counties, multiple = TRUE)
      ),
      checkboxInput("kaive", "Sarnane käive", value = TRUE),
      conditionalPanel(
        condition = "input.kaive == true",
        numericInputIcon(
          "dkaive", NULL, value = 20, min = 0, max = 100,
          icon = list(plusmiinus, icon("percent")),
          help_text = "Arv peaks jääma 0 ja 100% vahele."
        )
      ),
      checkboxInput("tootajad", "Sarnane töötajate arv", value = TRUE),
      conditionalPanel(
        condition = "input.tootajad == true",
        numericInputIcon(
          "dtootajad", NULL, value = 20, min = 0, max = 100,
          icon = list(plusmiinus, icon("percent")),
          help_text = "Arv peaks jääma 0 ja 100% vahele."
        )
      ),
      selectInput("aasta", "Vali võrreldav aasta", choices = company_years, selected = max(company_years, na.rm = TRUE)),
      actionButton("leia", "LEIA SARNASED", class = "btn-primary"),
      hr(),
      htmlOutput("nouded")
    )
  })

  # ---- similar companies ---------------------------------------------------
  similar_companies <- eventReactive(input$leia, {
    target <- selected_company_year()
    req(nrow(target) == 1)

    company_id <- as.character(input$company_id)
    aasta_val <- as.integer(input$aasta)

    candidates <- company_year_ds %>%
      filter(aasta == aasta_val) %>%
      collect() %>%
      filter(registrikood != company_id)

   if (isTRUE(input$sektor) && !is.na(target$emtak[[1]])) {
      sektor_val <- target$emtak[[1]]
      candidates <- candidates %>% filter(emtak == sektor_val)
    }

    if (isTRUE(input$emtak2) && "emtak2" %in% names(target) && !is.na(target$emtak2[[1]])) {
      emtak2_val <- target$emtak2[[1]]
      candidates <- candidates %>% filter(emtak2 == emtak2_val)
    } 

    if (isTRUE(input$maakond)) {
      counties <- unique(c(target$maakond[[1]], input$maakond2))
      counties <- counties[!is.na(counties) & counties != ""]
      if (length(counties) > 0) {
        candidates <- candidates %>% filter(maakond %in% counties)
      }
    }

    if (isTRUE(input$kaive) && !is.na(target$kaive[[1]])) {
      kaive_val <- target$kaive[[1]]
      candidates <- candidates %>%
        filter(
          kaive >= kaive_val * (1 - input$dkaive / 100),
          kaive <= kaive_val * (1 + input$dkaive / 100)
        )
    }

    if (isTRUE(input$tootajad) && !is.na(target$tootajad[[1]])) {
      tootajad_val <- target$tootajad[[1]]
      candidates <- candidates %>%
        filter(
          tootajad >= tootajad_val * (1 - input$dtootajad / 100),
          tootajad <= tootajad_val * (1 + input$dtootajad / 100)
        )
    }

    candidates %>%
      select(any_of(c("registrikood", "nimi", "aasta", "maakond", "emtak", "emtak2", "emtak2tekst", "kaive", "tootajad"))) %>%
      arrange(nimi)
  })

  chart_data <- reactive({
    sims <- similar_companies()
    target <- selected_company()
    req(input$aasta, nrow(target) == 1)

    aasta_val <- as.integer(input$aasta)
    needed_ids <- unique(c(target$registrikood, sims$registrikood))

    raw <- emta_quarterly_ds %>%
      filter(aasta >= aasta_val, registrikood %in% needed_ids) %>%
      select(registrikood, nimi, aasta, kvartal, rmaksud, toomaksud, kaive, tootajad) %>%
      collect() %>%
      add_derived_metrics()

    mine <- raw %>%
      filter(registrikood == target$registrikood) %>%
      mutate(grupp = target$nimi)

    similar_avg <- raw %>%
      filter(registrikood %in% sims$registrikood) %>%
      group_by(aasta, kvartal, aeg) %>%
      summarise(
        across(c(rmaksud, toomaksud, kaive, tootajad, kaive_tootaja, tmaksud_tootaja, rmaksud_kaive, tmaksud_kaive), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(grupp = "Sarnased ettevõtted")

    bind_rows(similar_avg, mine %>% select(names(similar_avg))) %>%
      arrange(aeg, grupp)
  })

  # ---- text outputs --------------------------------------------------------
  output$main_content <- renderUI({
    if (is.null(input$company_id) || input$company_id == "") {
      return(HTML("<h3>Lisainfo</h3><p>Vali ettevõte, seejärel määra sarnasuse tingimused.</p>"))
    }

    tagList(
      h3("Sarnased ettevõtted"),
      htmlOutput("nimekiri"),
      br(),
      htmlOutput("bench"),
      hr(),
      withSpinner(plotlyOutput("jooniskaive")),
      withSpinner(plotlyOutput("joonistootajad")),
      withSpinner(plotlyOutput("jooniskaive_tootaja")),
      withSpinner(plotlyOutput("joonistmaksud_tootaja")),
      withSpinner(plotlyOutput("joonisrmaksud_kaive")),
      withSpinner(plotlyOutput("joonistmaksud_kaive")),
      withSpinner(plotlyOutput("joonisrmaksud")),
      withSpinner(plotlyOutput("joonistmaksud")),
      htmlOutput("seletus")
    )
  })

  output$nouded <- renderUI({
    req(selected_company_year())
    target <- selected_company_year()

    txt <- "Ettevõtted on sarnased, kui on täidetud järgmised nõuded:"

    if (isTRUE(input$sektor)) {
      txt <- paste(txt, paste("<b>Sektor:</b>", safe_one(target$emtak)), sep = "<br/>")
    }
    if (isTRUE(input$emtak2)) {
      emtak2_label <- if ("emtak2tekst" %in% names(target)) safe_one(target$emtak2tekst) else safe_one(target$emtak2)
      txt <- paste(txt, paste("<b>EMTAK2:</b>", emtak2_label), sep = "<br/>")
    }
    if (isTRUE(input$maakond)) {
      txt <- paste(txt, paste("<b>Maakond:</b>", paste(unique(c(target$maakond, input$maakond2)), collapse = ", ")), sep = "<br/>")
    }
    if (isTRUE(input$kaive)) {
      txt <- paste(txt, paste0(
        "<b>Käive:</b> ",
        fmt_num(target$kaive * (1 - input$dkaive / 100)), " - ",
        fmt_num(target$kaive * (1 + input$dkaive / 100)), " €"
      ), sep = "<br/>")
    }
    if (isTRUE(input$tootajad)) {
      txt <- paste(txt, paste0(
        "<b>Töötajad:</b> ",
        fmt_num(target$tootajad * (1 - input$dtootajad / 100), 1), " - ",
        fmt_num(target$tootajad * (1 + input$dtootajad / 100), 1)
      ), sep = "<br/>")
    }

    txt <- paste(txt, paste("<b>Võrdlusaasta:</b>", input$aasta), sep = "<br/>")
    HTML(txt)
  })

  output$nimekiri <- renderUI({
    req(similar_companies())
    sims <- similar_companies()
    
    if (nrow(sims) == 0) {
      return(HTML("Sellistel tingimustel sarnaseid ei leitud."))
    }
    
    tagList(
      HTML(paste0("Leiti <b>", nrow(sims), "</b> sarnast ettevõtet.<br/><br/>")),
      tags$div(
        style = "
        column-count: 3;
        column-gap: 30px;
      ",
        lapply(head(sims$nimi, 100), function(x) {
          tags$div(
            style = "break-inside: avoid; margin-bottom: 4px;",
            x
          )
        })
      ),
      if (nrow(sims) > 100) HTML("<br/>...") else NULL
    )
  })

  output$bench <- renderUI({
    req(similar_companies())
    if (nrow(similar_companies()) > 0) {
      HTML("Vaata ettevõtteid lähemalt <a href='https://annegretemolloka.shinyapps.io/benchmarking_emta/' target='_blank'>ajaloo võrdluse rakendusest</a>.")
    }
  })

  output$seletus <- renderUI({
    HTML("<h3>Lisainfo</h3>
         <p>Rakendus otsib valitud ettevõttele sarnaseid ettevõtteid valitud aasta põhjal.
         Sarnasust saab piirata sektori, EMTAK2, maakonna, käibe ja töötajate arvu järgi.
         Joonistel võrreldakse valitud ettevõtet sarnaste ettevõtete keskmisega alates valitud aastast.</p>")
  })

  # ---- plots ---------------------------------------------------------------
  output$jooniskaive <- renderPlotly({
    plot_metric(chart_data(), "kaive", "Käive", unit = "€")
  })

  output$joonistootajad <- renderPlotly({
    plot_metric(chart_data(), "tootajad", "Töötajate arv")
  })

  output$jooniskaive_tootaja <- renderPlotly({
    plot_metric(chart_data(), "kaive_tootaja", "Käive töötaja kohta", unit = "€")
  })

  output$joonistmaksud_tootaja <- renderPlotly({
    plot_metric(chart_data(), "tmaksud_tootaja", "Tööjõumaksud töötaja kohta", unit = "€")
  })

  output$joonisrmaksud_kaive <- renderPlotly({
    plot_metric(chart_data(), "rmaksud_kaive", "Riiklikud maksud käibest", percent = TRUE)
  })

  output$joonistmaksud_kaive <- renderPlotly({
    plot_metric(chart_data(), "tmaksud_kaive", "Tööjõumaksud käibest", percent = TRUE)
  })

  output$joonisrmaksud <- renderPlotly({
    plot_metric(chart_data(), "rmaksud", "Riiklikud maksud", unit = "€")
  })

  output$joonistmaksud <- renderPlotly({
    plot_metric(chart_data(), "toomaksud", "Tööjõumaksud", unit = "€")
  })
})
