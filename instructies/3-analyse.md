# Fase 3: Statistische analyse

Gebruik R om aan de hand van de dataset jullie hypothese te analyseren met de gepaste statistische methoden. Alle code die jullie schrijven wordt bijgehouden in de directory `analyse/`.

Zorg er voor dat jullie resultaten *reproduceerbaar* zijn, d.w.z. dat deze zonder tussenkomst of manuele tussenstappen berekend worden uit de dataset. De originele dataset mag daarbij niet gewijzigd worden. In de directory `data/` vind je een R-Markdownbestand met enkele aanwijzingen hoe je dat moet aanpakken.

In de directory `analyse/` is al een bestand `analyse.Rmd` dat als startpunt kan dienen. Voeg in dit document alle nodige code toe voor de statistische verwerking van de dataset. Documenteer de code en jullie werkwijze in Markdown en voeg tussenin R-codeblokken toe. Structureer het document met tussentitels. Gebruik de R-Markdowndocumenten in de GitHub-repository van de cursus (onder [codevoorbeelden](https://github.com/HoGentTIN/onderzoekstechnieken-cursus/tree/master/codevoorbeelden)) als voorbeeld.

Als je in het team het werk verdeelt mag je meerdere .Rmd-bestanden toevoegen in deze directory. Geef een overzicht in de `README.md` van de directory `analyse/`. Vul ook telkens de juiste namen in in de hoofding.

Alle R-Markdownbestanden kunnen zonder fouten omgezet worden in HTML (met de Knit-functie). Wanneer de analyse is afgewerkt, wordt het HTML-bestand ook toegevoegd aan de repository.

Enkele aandachtspunten w.b. de inhoud:

- Vergeet niet de titel en auteursnamen aan te passen in de hoofding van alle .Rmd-bestanden!
- Ga ook op verkenning in de dataset door univariate analyse- en visualisatietechnieken toe te passen op de geselecteerde variabelen.
- Gebruik de correcte analyse- en visualisatietechnieken om na te gaan of er een verband bestaat tussen de gekozen variabelen. Je moet dit wellicht verschillende keren herhalen voor de verschillende combinaties van gekozen onafhankelijke en afhankelijke variabelen. Als je bv. 2 onafhankelijke en 2 afhankelijke variabelen gekozen hebt, kan je al 4 combinaties onderzoeken. Op die manier kan elk teamlid een bijdrage leveren.
- Trek een conclusie uit de behaalde resultaten. Is er effectief een verband tussen de onafhankelijke en afhankelijke variabelen?
- Reflecteer kritisch over de conclusie.
    - Had je dit resultaat verwacht?
    - Kan je het resultaat verklaren?
    - Zijn de resultaten tussen de verschillende combinaties van onafhankelijke/afhankelijke variabelen consistent?
    - Als je een verband vindt waar je er geen had verwacht, kan het dan gaan om "spurious correlation" of is het mogelijk dat er hier echt iets aan de hand is?

Voor deze fase is er geen tussentijdse deadline en deze wordt ook niet afzonderlijk beoordeeld. Je mag ter info wel een Release aanmaken (met Tag version `fase3`).
