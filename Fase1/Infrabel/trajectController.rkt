#lang racket


;;;Een traject kunnen maken a.d.h.v. loco-id en bestemming generate-traject(loco-id, destination) -> traject ADT

;;;Bij het maken van een traject kan het volgend probleem voorkomen
;;Zie bestand ProblemTraject
;;Stel nekeer dat het traject voor Loco impliceert dat het snelste manier om de bestemming te gaan is om
;;;via node1 - switch -node te gebruiken. Het probleem is dat als we van node1 komen niet naar node2 kunnen gaan men kan
;;kan wel langs switchpasseren maar niet naar node2omdat je die switch moet hersetten!
;;Oplossing node2 niet als een oplossing te hanteren
;;;1.Vooral dat we een node bekijken als een mogelijke oplossing moeten we kijken als de net bewaarde node een switch is
;;;In ons geval zal in traject (.......-node1 -switch)staan. als het een switch is dan kijkt men de eerder bewaarde
;;voor switch nl node1 en ziet als die overeenstemt met de node1 van switch als dit het geval is negeert men node2.
;;Als node1 overeenstemt met node0van switch dan betekent dat node2 eigenlijk de node1 van switch is of node2 van switch
;;Dus dan wel als parcour beschouwen!

;;;collision test zal gebeuren in deze volgorde:
;; A       L         track3     B   track8    C
;;;*-----------------------------*-------------*
;;;Op het moment dat de Loco volledig in track3 geraakt is wordt in de volgende stappen ,via traject van loco, bepaald:
;;;1.Als track8 gebruikt wordt om dus nu al te kunnen stoppen. Juist 1 keer is genoeg tot als de track3 verlaten werd.
;;;2.Als B een switchnode is. dan wordt de switch al geset!

