# Quatre en ratlla

Pràctica de Haskell per l'assignatura de Llenguatges de Programació(LP) durant l'Era del Confinament.


## Introducció

Consisteix en un programa en Haskell que permet jugar al Quatre en ratlla contra diferentes dificultats de l'ordinador.

![](4ratlla.png)

Es juga en un tauler `n` × `m`, on `n` i `m` es
defineixen al iniciar la partida. El cas més habitual és el 6×7, és a dir, 6
files i 7 columnes. Cada jugador té `n`×`m` fitxes d'un mateix color. Per
exemple, unes son vermelles i les altres grogues. Els jugadors introdueixen
alternativament una fitxa del seu color en una columna. La fitxa cau fins
dipositar-se al d'amunt de l'última fitxa que s'ha introduït en la mateixa
columna. Un jugador guanya quan després d'introduir una fitxa hi ha 4 fitxes
consecutives del seu color formant una línia horitzontal, vertical o diagonal.
La partida acaba quan un jugador guanya o s'omple el tauler.


## Estratègies

En aquesta pràctica, s'han dissenyat tres tipus d'estratègies que permetin a l'ordinador intentar guanya la partida. Aquestes estratègies s'han classificat com a tres dificultats diferents: estratègia Random(Fàcil), estratègia Greedy(Normal) i estratègia Smart(Avançat).

Cadascuna d'aquestes estratègies treballa de la següent manera:

- Estratègia **Random**: l'ordinador introdueix la fitxa en una columna a l'atzar.

- Estratègia **Greedy**: l'ordinador introdueix la fitxa en la columna més adient per, primer evitar que el jugador guanyi i després aconseguir tenir en ratlla el nombre més alt de fitxes pròpies en una columna, fila o diagonal.

- Estratègia **Smart**: l'ordinador introdueix la fitxa en la columna on té més possibilitats de guanyar en la següent ronda, primer de tot tornar a evitar que el jugador guanyi en el següent torn i després s'asegura quina columna o columnes són les més adients.


## Jugar a una partida

Per poder jugar a una partida s'han de fer aquests pasos:

- descomprimiu el fitxer ZIP
- compileu el codi amb la comanda `ghc joc.hs`
- executeu el joc amb la comanda `./joc`

Seguiu les instruccions per començar una partida:

- introduiu la `dificultat` de l'ordinador
- indiqueu la `mesura del tauler` (cas més habitual 6x7)
- seleccioneu `qui comença` i quin `color` sou

No oblideu disfruta de la partida i que no us guanyi l'ordinador, 
Bona Sort!
