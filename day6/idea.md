# Part 1

Izračune bomo izvedli na vsaki vrstici v data frejmu. map_df ali apply.

moramo ugotoviti kje je meja za držanje gumba.

## Koncept

1. Izračunamo za vsako sekundo kolikšen distanceTravelled ima (glej dol).
2. Izberemo tiste, ki so večje od rekorda
3. Dobimo length od možnosti

## Algoritem

npr. imamo [time = 4] sekunde.

- 1 s -> 3m
- 2 s -> 4m
- 3 s -> 3m

```
časDržanja x preostaliČas = distanceTravelled       (1)
```
```
1s x 3s = 3m
2s x 2s = 4m
3s x 1s = 3m
```

Če je rekord 3m, imamo eno možnost, da ga premagamo v tej rundi, torej je rezultat funkcije 1.

### postopek

1. len <- length(time) - 1
2. distanceTravelled <- c(1:len)
3. i x (len - i) na distanceTravelled; i je trenutna številka
4. return length(which(distanceTravelled > dst))

---

Postopek izvedemo na vsaki vrstici df-ja. Lahko naredimo `mutate(df, kombinacije = taFunkcija(...))` in nato produkt od kombinacij.

# Part 2

Ker je faking 50 miljonov sekund za pregledat in najt moram samo število tistih, ki so večje od rekorda, bom preveril od spredaj in od zadaj kje so meje za distance.

Kot je zgoraj 1 in 3 out of question. Tako bom našel prvo od spredaj, ki gre čez rekord in prvo od zadaj, ki gre čez rekord.

>(Ker se številke ponavljajo od sredine naprej, bi lahko tudi šli samo do sredine in množili z 2 rezultat)

## Koncept

1. Gremo čez sekunde od spredaj in sekunde od zadaj
2. Preverjamo ali je distanceTravelled več od rekorda
3. Najdemo obe meji
4. Izračunamo koliko števil je med točkama (npr. vektor dolg 10 mest, z mejama 3 in 7 ima 5 števil, right. to iščemo)

## Algoritem

Naj bo [time = 10].

```
časDržanja x preostaliČas = distanceTravelled       (1)
```

Računamo po [(1)]:

```
1s x 9s = 9m
2s x 8s = 16m
3s x 7s = 21m
4s x 6s = 24m
5s x 5s = 25m
6s x 4s = 24m
7s x 3s = 21m
8s x 2s = 16m
9s x 1s = 9m
```

Če je [rekord = 18], to pomeni, da od sekunde 3 do sekunde 7 imamo možnost za zmago. 7 in 3 sta meji.

```
abs(levaMeja - desnaMeja) = nasRezultat     (2)
```

### postopek

1. for i in 1:time
2. if i == length(time) - i, potem break ker smo na polovici
3. getDistance(time[i])
4. if dst1 > rekord, potem levaMeja = time[i]
5. getDistance(time[length(time) - i])
6. if dst2 > rekord, potem desnaMeja = time[length(time) - i]
