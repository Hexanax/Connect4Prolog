# PrologPuissance4
Projet Prolog - Jeu du puissance 4 dans le cadre du cours d'Approche logique de l’Intelligence Artificielle à l'INSA Lyon.

![image](https://user-images.githubusercontent.com/36091631/144425789-c81eab37-f7e7-4146-9dd9-106e06f049e2.png)


## Lancer l'application 
### Sur linux
1. Assurez vous que `swipl`est bien instalé, sinon installez le en executant
```bash
sudo apt-get install swi-prolog
```
2. dans une fenetre bash, lancez `swipl`:
```bash
swipl
```
3. Compilez le programme:
```prolog
[webserver].
```
4. Lancez le serveur à l'aide du prédicat`start.` (ou `server(8000).`). Puis accédez à [`http://localhost:8000/game`](http://localhost:8000/game) pour jouer !
```prolog
start.
```

### Sur windows
Nous vous conseillons de faire tourner le programme à l'aide de WSL. Vous pouvez installer WSL avec la commande `wsl --install` puis en redemarant l'ordinateur. Une fois WSL installé, suivez les instructions pour Linux.

## Infos Cours

### Hexanome 4112:
- Rahim BELATECHE 
- Matheus DE BARROS SILVA 
- Benoît DELEGLISE 
- Allan GUIGAL 
- Alexis METWALLI 
- Matthieu ROUX 
- Mathieu SAUGIER

### Sources d'inspiration

**Projet de base**: [SIGSWAG/PrologPuissance4](https://github.com/SIGSWAG/PrologPuissance4)

**Alpha beta utilisé** : [PeredurOmega/PrologPuissance4](https://github.com/PeredurOmega/PrologPuissance4)

**Inspiration alpha beta implémentation** :
- [PascalPons/connect4](https://github.com/PascalPons/connect4/blob/part4/solver.cpp)
- [pl:prolog:pllib:minimax_move](https://ai.ia.agh.edu.pl/pl:prolog:pllib:minimax_move)

## Les sources
Les fichiers sources sont séparés en deux parties : les prédicats "publics" (exportés par le module) et les prédicats "privés". Tandis que nous avons essayés de converser les prédicats publics très homogènes pour des raisons d'interfaçage et de partage du travail, les prédicats privés sont plus organisés selon le bon vouloir de chacun.
