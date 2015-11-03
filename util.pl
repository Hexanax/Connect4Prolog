%%%%%%%%%%%% util.pl %%%%%%%%%%%%

:- module(util, [
	nbLignes/1,
	nbColonnes/1,
	infinitePos/1,
	infiniteNeg/1,
	infinitePos/2,
	infiniteNeg/2,
	ennemi/2,
	incr/2,
	decr/2,
	sum/2,
	caseVide/2
]).

% Pour �valuer le plateau de jeu et r�utiliser les pr�dicats d�finis dans jeu, on suppose que les caseTest ont effectivement �t� jou�es.
% Cette astuce permet de traiter les cases de test (utilis�es par minimax) comme des cases normales, tout en simplifiant leur suppression.
% NE FONCTIONNE PAS, DUPLICATION DE CODE EN ATTENDANT UNE MEILLEURE SOLUTION !!
% :- assert(jeu:case(X,Y,J) :- minimax:caseTest(X,Y,J)).

%%%%%%%%%%%%%%%%
%% Constantes %%
%%%%%%%%%%%%%%%%

nbLignes(6).
nbColonnes(7).

infinitePos(10005).
infiniteNeg(-10005).

infinitePos(X,Rep):- Rep is X+10000.
infiniteNeg(X,Rep):- Rep is -10000-X.

%%%%%%%%%%%%%%%%%%%%%%%
%% Pr�dicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

ennemi(jaune,rouge).
ennemi(rouge,jaune).

% incr/2(+X, -X1)
% unifie X1 � X+1
% vrai pour X1 = X+1
incr(X,X1):- 
	X1 is X+1.

% decr/2(+X, -X1)
% unifie X1 � X-1
% vrai pour X1 = X-1
decr(X,X1):- 
	X1 is X-1.

% sum/2(+Liste, -Somme)
% Somme les termes de la liste.
% Somme s'unifie � la somme des termes de la liste.
sum([],0).
sum([X|Xs],N) :-
	sum(Xs,N1),
	N is N1+X.

% caseVide/2(+X, +Y)
% V�rifie si la case est vide.
% Vrai si la case n'a pas �t� remplie.
caseVide(X,Y) :-
	nonvar(X),
	nonvar(Y),
	not(case(X,Y,_)).