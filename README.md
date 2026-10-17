# Cashflow Model

**Interaktivni simulator denarnih tokov in dolgoročnih finančnih scenarijev.**

### [Odpri spletno aplikacijo →](https://01a14ac5-c78a-4d78-1589-237cc617ac8f.share.connect.posit.cloud/)

Aplikacija je objavljena na Posit Connect Cloud; povezavo lahko deliš neposredno.

Cashflow Model omogoča raziskovanje, kako enkratni in ponavljajoči se prilivi, odlivi ter časovno spremenljive stopnje donosnosti vplivajo na finančno stanje skozi čas. Namenjen je tako začetnikom kot uporabnikom, ki želijo oblikovati podrobnejše scenarije.

> Model je ponazoritev na podlagi uporabnikovih predpostavk, ne finančna napoved ali naložbeno priporočilo.

## Kaj omogoča

- **Poljubni denarni tokovi:** enkratni in ponavljajoči se prilivi ter odlivi.
- **Trije prikazi časa:** leta od začetka, starost ali koledarska leta.
- **Časovno spremenljiva donosnost:** tri nastavljive stopnje s prehodi med prelomnimi leti.
- **Interaktivni grafi:** stanje, kumulativni neto denarni tokovi, letni tokovi in donosnost.
- **Obratni izračuni:** potreben letni znesek ali obdobje za zastavljeni finančni cilj.
- **Pripravljeni primeri:** dolgoročno varčevanje, večji prihodnji izdatek in mešani denarni tokovi.
- **Shranjevanje in nalaganje scenarijev:** prenos načrta v JSON in ponovni uvoz brez uporabniškega računa.

## Hitri začetek

1. Izberi **pripravljeni scenarij** ali začni s praznim načrtom.
2. Določi, ali želiš čas prikazati kot leta od začetka, starost ali koledarska leta.
3. Dodaj ali spremeni denarne tokove (pozitivni zneski so prilivi, negativni odlivi).
4. Prilagodi stopnje donosnosti in spremljaj grafe ter ključne kazalnike.
5. Po želji načrt **shrani v JSON** in ga kasneje ponovno naloži.

**Opomba:** vneseni denarni tokovi so *letni zneski*. Za ponavljajoči se mesečni znesek vnesi njegov letni ekvivalent (12-kratnik). Leto 0 je začetek simulacije.

## Pripravljeni scenariji

| Primer | Vsebinska zasnova |
| --- | --- |
| Dolgoročno varčevanje | 5.000 € začetnega vložka in 2.400 € letno v naslednjih 40 letih |
| Večji prihodnji izdatek | 20.000 € začetnih prihrankov, 3.000 € letno in izdatek 35.000 € v 12. letu |
| Različni denarni tokovi | Vplačila, enkratni prejemek, strošek in kasnejši dvigi |

Primeri so ilustrativni in jih je mogoče spremeniti.

## Zagon v RStudiu

Zahtevan je R in dostop do CRAN. Odpri `cashflow_model.Rproj`, nato namesti potrebne pakete:

```r
install.packages(c(
  "shiny", "bslib", "plotly", "shinyalert",
  "rlist", "gridExtra", "tidyverse", "jsonlite", "testthat"
))
shiny::runApp()
```

### Samodejni testi

```r
testthat::test_file("tests/testthat/test-model.R")
```

Referenčni testi preverjajo osnovne finančne izračune, med drugim enkratni vložek, redna vplačila, spremenljivo donosnost in obratna izračuna. Uspešni osnovni testi še ne pomenijo popolne validacije vseh možnih scenarijev.

## Struktura projekta

```text
app.R                        # vmesnik in Shiny strežniška logika
R/model.R                    # finančne funkcije in priprava grafov
tests/testthat/test-model.R  # referenčni izračuni in testi
```

## Računske predpostavke

Denarni tok v posameznem letu se prišteje **po** obračunu donosnosti na stanje iz predhodnega obdobja. Začetni vložek pri času 0 zato lahko ustvarja donos od naslednjega obdobja naprej.

Kazalnik *učinek donosnosti* pomeni razliko med skupnim stanjem in kumulativnimi neto denarnimi tokovi. Učinek je lahko tudi negativen. Model v trenutni obliki ne vključuje inflacije, davkov, transakcijskih stroškov, stohastičnih donosov ali tveganja neplačila.

## Shranjevanje scenarijev

JSON vsebuje dogodke, trajanje načrta, način prikaza časovne osi ter predpostavke donosnosti. Obdobja dogodkov so shranjena relativno glede na začetek modela; sprememba med prikazom starosti, koledarskega leta in relativnih let ne spremeni njihovega ekonomskega časa.

Pri uvozu uporabljaj datoteke iz zaupanja vrednih virov.

## Spletna predstavitev

**[Zaženi Cashflow Model](https://01a14ac5-c78a-4d78-1589-237cc617ac8f.share.connect.posit.cloud/)** na Posit Connect Cloud.

## Razvoj

Projekt je nastal kot R Shiny prototip leta 2023, prenovljen za predstavitev v letu 2026. Nadaljnji razvoj naj izhaja iz povratnih informacij uporabnikov in dogovorjenega namena orodja.
