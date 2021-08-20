# Fase 2: Literatuuronderzoek

In deze fase gaan jullie een literatuuronderzoek voeren naar bepalende factoren voor wiskundige geletterdheid. Je komt hierdoor te weten:

- Welk onderzoek er al is gedaan
- Wat de bevindingen hiervan waren
- Welke bevindingen elkaar tegenspreken
- Welk onderzoek er eventueel nog ontbreekt
- Wat jullie met je onderzoek toevoegen aan de bestaande literatuur

Voor een leidraad in het voeren van een literatuuronderzoek, het gebruik van een bibliografische databank, het gebruik van LaTeX, zie de cursus en ook [Borms & Vandroemme (2018)](../toelichting.md#Borms2018) en [Van Vreckem & Buysse (2018)](../toelichting.md#VanVreckem2018).

Om je resultaat in te dienen maak je via de Github-pagina van jullie project een nieuwe Release aan (blauwe link rechts "Create a new release") vóór de opgegeven deadline. Kies als Tag version `fase2`. Eén van de lectoren zal zo snel mogelijk feedback geven via een Github Issue.


## Publicaties opzoeken

Elk teamlid zoekt (minstens) **twee unieke publicaties** (artikels, enz.) over het onderwerp en vat dit kort samen. Verzamel ze in de bibliografische databank in jullie GitHub repository (het bestand `rapport/bibliografie.bib`. Gebruik JabRef om de bibliografische databank te bewerken.

Vul alle noodzakelijke velden aan (auteur(s), titel, jaar van publicatie, enz.) en schrijf ook telkens in enkele zinnen waarover het betreffende artikel gaat zodat je teamgenoten een idee hebben van wat je gevonden hebt (dat kan in het tabblad Comments). Gebruik de bachelorproefgids [(Van Vreckem & Buysse, 2018)](../toelichting.md#VanVreckem2018) om na te gaan hoe je bepaalde types van publicaties correct kan invoegen in JabRef.

## Reading group

Organiseer een **reading group** met je team waar je samen door de gevonden teksten gaat en die bespreekt. De bedoeling is dat iedereen de inhoud van de artikels begrijpt.

Stel jezelf volgende vragen:

- Wat vinden jullie van de artikels?
- Welke vinden jullie het interessantst en waarom?
- Welke artikels vinden jullie niet goed en waarom niet?
- Is iedereen het eens over de kwaliteit van de artikels? Doe de CRAP-test [(Gratz, 2015)](../toelichting.md#Gratz2015)!
- Zijn de experimenten representatief voor de onderzoeksvraag?

Hou een verslag bij van de reading group in `literatuuronderzoek/reading-group.md`.

## Literatuurstudie als onderdeel van het onderzoeksrapport

Elk wetenschappelijk artikel dat jullie gelezen hebben bevat normaal gezien ook een literatuurstudie. Dit is een samenvatting van de kennis die de onderzoekers hebben opgedaan over het onderwerp door wetenschappelijke/vakliteratuur over het onderwerp te lezen, net zoals jullie nu gedaan hebben.

Schrijf met jullie team een gelijkaardige tekst op basis van de gevonden publicaties en jullie kritische reflectie tijdens de reading group. Let op! Dit is niet zomaar een opsomming van de werken die jullie gelezen hebben en wat in elk staat. Probeer de stijl na te bootsen van literatuuroverzichten in de publicaties die jullie gelezen hebben.

Voeg deze tekst meteen toe aan het sjabloon voor het artikel (`rapport/wiskundige-geletterdheid.tex`) dat jullie in fase 4 zullen afwerken.

Let bij het uitschrijven van een literatuurstudie in het bijzonder op het volgende:

- Hanteer een **wetenschappelijke schrijfstijl**. Dat betekent dat je niet de eerste persoon gebruikt (dat impliceert een mening, geen objectieve feiten). Vermijd ook informele taal, bloemrijke adjectieven, enz.
- De literatuurstudie is een doorlopende tekst die een overzicht geeft van de huidige stand van zaken in het onderzoeksdomein.
- Verwijs op de gepaste plaats in de tekst naar de gebruikte bronnen (met het LaTeX-commando `\textcite{}` of `\autocite{}`). Enkel gerefereerde werken zullen in de literatuurlijst opgenomen worden. Dat moet ook zo: in een literatuurlijst mogen geen bronnen voorkomen waar niet naar verwezen wordt in de tekst.

Geef het artikel een gepaste titel en pas ook de namen van de auteurs aan. Verder hoef je nog niets te schrijven (dus geen abstract, methodologie, conclusie, enz.).

## Checklist

- Elk teamlid heeft &ge;2 publicaties aangeleverd gerelateerd aan het onderzoeksdomein en toegevoegd in `rapport/bibliografie.bib`
- Het team heeft een reading group gehouden en een verslag in de repository toegevoegd onder `literatuuronderzoek/reading-group.md`
- De kennis opgedaan in de gevonden publicaties is verwerkt tot een doorlopende tekst die toegevoegd is aan het artikelsjabloon onder `rapport`
