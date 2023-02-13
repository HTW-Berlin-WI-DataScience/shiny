library(DiagrammeR)

ui <- fluidPage(
  h1("Wie zuverlässig sind Testergebnisse?"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput("testKind", "Wähle einen Test:",
                  list("Covid-19", "HIV", "Schwangerschaft")
      ),
      uiOutput("relativePositives"),
      p("Die Sensitivität gibt an, wie häufig der Test bei tatsächlich Positiven korrekt ausfällt ist (richtig positiv)."),
      uiOutput("sensitivitaetOutput"),
      p("Die Spezifität gibt an, wie häufig der Test bei tatsächlich Negativen korrekt ausfällt (richtig negativ)."),
      uiOutput("spezifitaetOutput"),
      textOutput("quelleTestDaten")
      ),
    mainPanel(
      h2("Hinweise zur Nutzung des Tools"),
      p("Die Berechnungen des Tools illustrieren, dass die Aussagekraft von Covid-19-, HIV- und Schwangerschaftstests stark vom Anteil der tatsächlich Positiven unter den getesteten Personen (Vortestwahrscheinlichkeit) sowie von der Sensitivität und Spezifität der Tests abhängt."),
      textOutput("value"),
      p("In diesem Tool gibt es wie bereits oben erwähnt eine Auswahl von drei verschiedenen Tests (Covid-19-, HIV- und Schwangerschaftstest). Dazu gibt es jeweils eine voreingestellten Sensitivität und Spezifität, welches aus den entsprechenden Quellen entnommen wurde (s. Information). Zudem ist die Vortestwahrscheinlickeit angegeben als die relative Anzahl der Getesteten die tatsächlich positiv sind. Daraus resultieren entsprechende Werte für den positiven und den negativen Vorhersagewert."),
      tabsetPanel(
        tabPanel(title = "Tabellen",
                 tableOutput("tabelle"),
                 h3("Kennzahlen"),
                 p("Positiver Vorhersagewert: Eine Person hat ein positives Testergebnis. Wie wahrscheinlich ist sie tatsächlich positiv?"),
                 p("Negativer Vorhersagewert: Eine Person hat ein negatives Testergebnis. Wie wahrscheinlich ist sie tatsächlich negativ?"),
                 tableOutput("tabelleKennzahlen")
        ),
        tabPanel(title = "Baumdiagramm",
                 grVizOutput("baumdiagramm"),
        ),
        tabPanel(title = "Waffelplot",
                 plotOutput("waffelplot"),
        ),
        tabPanel(title = "Information",
                 h3("Abgabeinformationen"),
                 p("Die Werte in den Visualisierungungen werden aus den bedingten Wahrscheinlichkeiten mit dem Satz von Bayes mit einer Testgröße von 10.000 errechnet.
                   In dem Baumdiagramm stehen die Anzahl der jeweiligen Ergebnisse entlang der Kanten. 
                   Im Waffelplot werden die einzelnen Testergebnisse visualisiert. Ein farbiges Rechteck entspricht dabei einem Testergebnis."),
                 h4("Gruppenmitglieder"),
                 p("Konstantin Groll"),
                 p("Carmen Luu"),
                 p("Antoine Ma"),
                 h4("Quellen"),
                 p("Folgende Ressourcen wurden zur Erstellung der Abgabe und als Inspirationsquellen genutzt:"),
                 p("- https://rki-wiko.shinyapps.io/test_qual/"),
                 p("- https://rich-iannone.github.io/DiagrammeR/"),
                 p("- https://cran.r-project.org/web/packages/waffle/index.html"),
                 p("- https://www.moelab.de/hCG.html"),
                 p("- https://www.autotest-sante.com/de/autotest-VIH-par-AAZ-139.html#meancre6"),
                 p("- Foliensatz 1c - Bedingte Wahrscheinlichkeiten 1, Prof. Dr. Martin Spott")
        ),
      ),
    )
  )
)
