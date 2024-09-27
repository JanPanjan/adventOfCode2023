# Part 1

Izračune bomo izvedli na vsaki vrstici v data frejmu. map_df ali apply.

moramo ugotoviti kje je meja za držanje gumba.

## Koncept

1. Izračunamo za vsako sekundo kolikšen distanceTravelled ima (glej dol).
2. Izberemo tiste, ki so večje od rekorda
3. Dobimo length od možnosti

## Algoritem

npr. imamo [time = 4] sekunde. nikoli ne držimo 0 ali 4 sekunde.
izognemo se [0] in [time] vrednostim.

- 1 s -> 3m/s
- 2 s -> 4m/s
- 3 s -> 3m/s

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

