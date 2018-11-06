- - - - - - - - - - - - - -
Korisni direktorijumi:
- - - - - - - - - - - - - -   
  - PDFcorpus: sadrži validne PDF datoteke koje ce program mutirati. U ovaj direktorijum se po želji mogu smeštati nove i brisati postojeće datoteke
  - PDF_citaci: sadrži PDF čitače koji su ponuđeni u programu
  - heuristics: sadrži tekstualne datoteke sa vrednostima različitih tipova koje često izazivaju greške u programima

  Pri pronalasku greške u čitaču, informacije o tome se čuvaju u sledeća dva direktorijuma:
	- fuzzedPDFError: sadrži PDF datoteku koja je izazvala greške u radu čitača
    - results: sadrži tekstualnu datoteku u kojoj se nalaze informacije o standardnom izlazu, standardnom izlazu za greške i izlaznoj vrednosti čitača


	sudo apt-get install nilfs-tools

chcp 1250
java -jar .\fuzzer_jar.jar --help

Configuration instructions
Installation instructions
Operating instructions
A file manifest (list of files included)
Copyright and licensing information
Contact information for the distributor or programmer
Known bugs[3]
Troubleshooting[3]
Credits and acknowledgments
A changelog (usually for programmers)
A news section (usually for users)


-- Projekat: Program za rasplinuto testiranje PDF čitača
   Program služi za testiranje PDF čitača tehnikom rasplinutog testiranja. Implementirano je konkurentno i paralelno izvršavanje čime je
   svaka komponenta izvršavanja zadužena da generiše svoj test primer (PDF datoteku) kojim će testirati PDF čitač. Broj komponenata zavisi od
   izbora korisnika i slobodne memorije i broja jezgara procesora računara sa kog se vrši testiranje. Za više detalja o samom programu, 
   načinu njegovog rada i ulaznim parametrima program pokrenuti sa opcijom --help (opisano u nastavku)
  