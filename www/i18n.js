(function () {
  "use strict";
  const pairs = [
    ["Moja finančna prihodnost","Cashflow Model"],
    ["Preveri, kako varčevanje, večji izdatki in čas vplivajo na tvoje prihranke.","Explore how cashflows, expenses and time shape your financial future."],
    ["Finančni načrt · preizkusna različica","Financial plan · preview"],
    ["Scenariji","Scenarios"],["Začni s pripravljenim primerom ali shrani svoj načrt.","Start with an example or save your own plan."],
    ["Pripravljeni primer","Example scenario"],["Izberi primer ...","Choose an example ..."],
    ["Dolgoročno varčevanje","Long-term saving"],["Večji prihodnji izdatek","Major future expense"],
    ["Različni denarni tokovi","Mixed cashflows"],["Naloži primer","Load example"],
    ["Shrani JSON","Save JSON"],["Naloži shranjeni JSON","Load saved JSON"],
    ["Nalaganje zamenja trenutni načrt. Datoteka ostane na tvoji napravi.","Loading replaces the current plan. The file stays on your device."],
    ["Časovna os","Time axis"],["Izberi oznake let. Izračun ostane enak.","Choose how years are displayed. Calculations remain unchanged."],
    ["Leta od začetka","Years from start"],["Starost","Age"],["Koledarska leta","Calendar years"],
    ["Trenutna starost","Current age"],["Začetno leto","Starting year"],["Trajanje načrta (let)","Planning horizon (years)"],
    ["Prihodki in izdatki","Inflows and outflows"],
    ["Vnesi letne zneske v evrih: prilivi so pozitivni, izdatki negativni. Za mesečni znesek ga najprej pomnoži z 12.","Enter an annual amount in euros: inflows are positive and outflows negative."],
    ["Kaj se zgodi?","Event name"],["Uredi vnos","Edit event"],["Od leta","From year"],["Do leta","To year"],
    ["Izračunaj leto","Solve year"],["Znesek je","Amount frequency"],["Letni","Annual"],["Mesečni","Monthly"],
    ["Letni znesek (€)","Amount (€)"],["Izračunaj znesek","Solve amount"],["Dodaj","Add"],["Počisti","Clear"],
    ["Časovnica se pripravi samodejno. Prazno polje »Do leta« pomeni enkratni dogodek.","The timeline is created automatically. Leave ‘To year’ blank for a one-off event."],
    ["Letna obrestna mera zadolževanja (%)","Annual borrowing interest rate (%)"],
    ["Negativno stanje se samodejno obravnava kot dolg. Obrestna mera dolga je stalna.","A negative balance is treated as debt automatically. The borrowing rate is fixed."],
    ["Neto učinek obrestovanja","Net effect of interest and returns"],
    ["Razporeditev naložb skozi čas","Investment allocation over time"],
    ["Delnice / obveznice: od začetka do konca načrta.","Stocks / bonds: from the start to the end of the plan."],
    ["Naložbena strategija","Investment strategy"],["Preprosto: delnice in obveznice","Simple: stocks and bonds"],
    ["Napredno: ročne stopnje donosnosti","Advanced: custom return rates"],
    ["Strategija","Strategy"],["Postopni prehod k obveznicam","Gradual shift towards bonds"],
    ["Samo delnice","Stocks only"],["Uravnoteženo (60 / 40)","Balanced (60 / 40)"],
    ["Samo obveznice","Bonds only"],["Začetni delež delnic (%)","Initial stock allocation (%)"],
    ["Končni delež delnic (%)","Final stock allocation (%)"],
    ["Ilustrativna predpostavka: delnice 7 %, obveznice 3 % letno. Donos ni zagotovljen.","Illustrative assumption: stocks 7%, bonds 3% annually. Returns are not guaranteed."],
    ["Ročne predpostavke donosnosti","Custom return assumptions"],["Predpostavljena letna donosnost se med izbranimi leti postopno spreminja. Donosnost ni zagotovljena.","Assumed returns change gradually between the selected years. Returns are not guaranteed."],
    ["Začetna donosnost","Initial return"],["Vmesna donosnost","Intermediate return"],["Končna donosnost","Final return"],
    ["Prvo leto","First breakpoint"],["Drugo leto","Second breakpoint"],["Tretje leto","Third breakpoint"],
    ["Prikaz rezultatov","Results display"],["V današnji kupni moči","In today's purchasing power"],
    ["Letna inflacija (%)","Annual inflation (%)"],["Shrani trenutno projekcijo za primerjavo","Save current projection for comparison"],
    ["Odstrani primerjavo","Remove comparison"],["Shranjena primerjava se ohrani do konca seje ali ponovnega shranjevanja.","Saved comparison stays in this session until replaced."],
    ["Pregled","Overview"],["Končno stanje","Final balance"],["Neto denarni tokovi","Net cashflows"],
    ["Učinek donosnosti","Effect of returns"],["Stanje, denarni tokovi in donosnost","Balance, cashflows and returns"],
    ["Primerjava skupnega stanja s kumulativnimi neto denarnimi tokovi. Razlika predstavlja učinek predpostavljene donosnosti.","Compare total balance and cumulative net cashflows. Their difference is the effect of assumed returns."],
    ["Letni prilivi in izdatki","Annual inflows and outflows"],["Predpostavljena donosnost","Assumed return"],
    ["Moji vnosi","My events"],["Vneseni dogodki","Recorded events"],
    ["Časovnica se ustvari samodejno in ni prikazana med dogodki.","The timeline is created automatically and hidden from events."],
    ["Dobro je vedeti","Good to know"],
    ["Rezultat je izračun na podlagi tvojih predpostavk, ne napoved. Model ne upošteva inflacije, davkov ali negotovosti donosov. Zato ga uporabljaj za primerjanje scenarijev.","Results depend on your assumptions, not a forecast. The model excludes taxes and uncertain returns. Inflation is only included when explicitly enabled."],
    ["Vnesi dogodek","Enter an event"],["Izberi dogodek","Choose event"],
    ["Izberi","Select"],["Prekliči","Cancel"],["Ni vnosov","No events"],
    ["Najprej dodaj prihodek ali izdatek.","Add an inflow or outflow first."],
    ["Preveri podatke","Check inputs"],["Manjkajo podatki","Missing data"],
    ["Scenarij naložen","Scenario loaded"],["Načrt je uspešno obnovljen.","Plan restored successfully."],
    ["Napaka","Error"],["Scenarija ni bilo mogoče naložiti","Could not load scenario"],
    ["Za primerjavo shrani trenutno projekcijo in spremeni predpostavke.","Save the current projection and change assumptions to compare."],
    ["Evri","Euros"],["Leto modela","Model year"],["Donosnost","Return"],
    ["Skupno stanje","Total balance"],["Neto tokovi","Net flows"],["Trenutni scenarij","Current scenario"],
    ["Primerjava","Comparison"],["Dogodek","Event"],
    ["npr. letno varčevanje, nakup avtomobila","e.g. annual savings, car purchase"],
    ["npr. -12000","e.g. -12000"]
  ];
  const toEnglish = new Map(pairs), toSlovene = new Map(pairs.map(([a,b])=>[b,a]));
  let current = "sl", busy = false;
  const ignore = new Set(["SCRIPT","STYLE","TEXTAREA","CODE","PRE"]);
  function rewrite(text, dictionary) {
    const trimmed = text.trim();
    if (!trimmed) return text;
    const direct = dictionary.get(trimmed);
    if (direct !== undefined) return text.replace(trimmed, direct);
    for (const [from,to] of dictionary.entries()) {
      if (trimmed.startsWith(from + ":")) return text.replace(trimmed, to + trimmed.slice(from.length));
    }
    return text;
  }
  function translate() {
    if (busy || current === "sl") return;
    busy = true;
    const dict = toEnglish;
    try {
      const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT);
      let node;
      while ((node = walker.nextNode())) {
        if (node.parentElement && !ignore.has(node.parentElement.tagName)) {
          const next = rewrite(node.nodeValue, dict);
          if (next !== node.nodeValue) node.nodeValue = next;
        }
      }
      document.querySelectorAll("input[placeholder]").forEach(input => {
        const next = rewrite(input.placeholder, dict);
        if (next !== input.placeholder) input.placeholder = next;
      });
      document.documentElement.lang = current;
    } finally { busy = false; }
  }
  function switchLanguage(next) {
    if (next !== "en" && next !== "sl") return;
    if (next === current) return;
    const dictionary = next === "en" ? toEnglish : toSlovene;
    busy = true;
    try {
      const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT);
      let node;
      while ((node = walker.nextNode())) {
        if (node.parentElement && !ignore.has(node.parentElement.tagName)) {
          const translated = rewrite(node.nodeValue, dictionary);
          if (translated !== node.nodeValue) node.nodeValue = translated;
        }
      }
      document.querySelectorAll("input[placeholder]").forEach(input => {
        input.placeholder = rewrite(input.placeholder,dictionary);
      });
      current = next;
      document.documentElement.lang = next;
    } finally { busy = false; }
  }
  // Shiny selectInput uses Selectize/jQuery events; listen to Shiny's
  // explicit server-to-browser message instead of relying on native change.
  function registerHandler() {
    if (window.Shiny && window.Shiny.addCustomMessageHandler) {
      window.Shiny.addCustomMessageHandler("cashflow-language", switchLanguage);
      return true;
    }
    return false;
  }
  if (!registerHandler()) {
    document.addEventListener("shiny:connected", registerHandler, {once:true});
  }
  // Also support keyboard/native changes when not running in Shiny.
  document.addEventListener("change", event => {
    if (event.target && event.target.id === "language") switchLanguage(event.target.value);
  });
  document.addEventListener("DOMContentLoaded", () => {
    const node=document.getElementById("language");
    if (node && node.value === "en") switchLanguage("en");
    new MutationObserver(() => { if (current === "en") translate(); })
      .observe(document.body, {childList:true,subtree:true,characterData:true});
  });
})();
