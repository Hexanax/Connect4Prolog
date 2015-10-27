%%%%%%%%%%%% util.pl %%%%%%%%%%%%

:- module(util, [
	nbLignes/1,
	nbColonnes/1,
	infinitePos/1,
	infiniteNeg/1,
	incr/2,
	decr/2,
	sum/2
]).

% Pour �valuer le plateau de jeu et r�utiliser les pr�dicats d�finis dans jeu, on suppose que les caseTest ont effectivement �t� jou�es.
% Cette astuce permet de traiter les cases de test (utilis�es par minimax) comme des cases normales, tout en simplifiant leur suppression.
:- assert(jeu:case(X,Y,J) :- minimax:caseTest(X,Y,J)).

%%%%%%%%%%%%%%%%
%% Constantes %%
%%%%%%%%%%%%%%%%

nbLignes(6).
nbColonnes(7).

infinitePos(10000).
infiniteNeg(-10000).

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