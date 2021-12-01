%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%						Méthode Greedy 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Code implémentant les algorithmes et les règles de Greedy
%%% Basé sur l'implémentation de l'algorithme Greedy du profil git : https://github.com/dimashky
%%% 


%%% Le but de cette méthode, à chaque coup, est d'effectuer le meilleur coup pour : 
%%% 	- soit gagner au coup actuel, si possible (attaque) 
%%%			-> implémenté dans possibilite_gagner(+Col,-Res,+CouleurJCourant)
%%% 	- soit chercher le meilleur coup pour empêcher l'adversaire de gagner (défense)
%%% 		-> implémenté dans meilleur_coup(+Col,-Res,-ColRes,+CouleurJoueur)




:- module(greedy, [glouton/2]).

:- dynamic top/2.


:- use_module(util).
:- use_module(jeu).
:- use_module(ia).


%%%%%%%%%%%%%%  Règles de l'algorithme Greedy  %%%%%%%%%%%%%%%


%traverse(+C,+R,+incC,+IncR,-V)
%Permet de trouver le nombre de jetons qui permettent de gagner à partir du jeton (C,R)
%dans toutes les directions (vertical, horizontal, diagonal)
%le sens de visite dépend de (IncC,IncR), si c'est (1,0) par exemple permet d'aller vers le haut 
traverse(C,R,IncC,IncR,Res):-
	NewC is C + IncC,
	NewR is R + IncR,
	case(C,R,C1),
	case(NewC,NewR,C2),
	C1 == C2,
	traverse(NewC,NewR,IncC,IncR,Res1),
	Res is Res1 + 1,!.
traverse(_,_,_,_,Res):-
 Res is 1.



%check(+Col,+Row)
%Cette méthode vérifie la situation de victoire à partir du jeton (X,Y)
%appelle traverse afin de visiter dans toutes les directions 
%renvoie vrai si cas de victoire, faux sinon
check(X,Y):-
 traverse(X,Y,1,0,R1),
 traverse(X,Y,-1,0,R2),
 R is R1 + R2 - 1  ,
 R >= 4,!.
%Vertical 
check(X,Y):-
 traverse(X,Y,0,1,R1),
 traverse(X,Y,0,-1,R2),
 R is R1 + R2 - 1,
 R >= 4,!.
%Main Diagonal Check
check(X,Y):-
 traverse(X,Y,1,1,R1),
 traverse(X,Y,-1,-1,R2),
 R is R1 + R2 - 1,
 R >= 4,!.
%Secondary Diagonal Check
check(X,Y):-
 traverse(X,Y,1,-1,R1),
 traverse(X,Y,-1,1,R2),
 R is R1 + R2 - 1,
 R >= 4,!.



%Cette méthode part du jeton (X,Y) et renvoie le nombre de jetons consécutifs de la même couleur 
%que le jeton initial 
get_max(X,Y,RES):-
	traverse(X,Y,1,0,R11),traverse(X,Y,-1,0,R12),R1 is R11 + R12 - 1,
	traverse(X,Y,0,1,R21),traverse(X,Y,0,-1,R22),R2 is R21 + R22 - 1,
	traverse(X,Y,1,1,R31),traverse(X,Y,-1,-1,R32),R3 is R31 + R32 - 1,
	traverse(X,Y,1,-1,R41),traverse(X,Y,-1,1,R42),R4 is R41 + R42 - 1,
	RES1 is max(R1,R2),RES2 is max(R3,R4),RES is max(RES1,RES2).



%possibilite_gagner(+Col,-Res,+CouleurJCourant)
%Permet de vérifier si l'on peut arriver à un état de victoire avec un seul coup 
possiblite_gagner(COL,RES,CouleurJCourant):-
	R is 6,
	top(COL,X),
	NX is X + 1,
	R == NX,
	NCOL is COL + 1,
	possiblite_gagner(NCOL,RES,CouleurJCourant),!.

possiblite_gagner(COL,RES,CouleurJCourant):-
	placerJeton(COL,Ligne,CouleurJCourant),
	%assert(top(COL,LigneIncr))
	top(COL,NX), %NX is height of COL
	check(COL,NX),
	RES is COL,
	remove(COL,Ligne,CouleurJCourant),!.

possiblite_gagner(COL,RES,CouleurJCourant):-
	top(COL,X),
	%NX is X - 1,
	%assert(top(COL,NX)),
	%retract(top(COL,X)),
	retract(case(COL,X,CouleurJCourant)),
	NCOL is COL + 1,
	possiblite_gagner(NCOL,RES,CouleurJCourant).


%top(+COL,-Ligne)
%Permet de renvoyer dans Ligne le nombre de jetons d'une colonne donnée 
top(COL,Ligne) :- calculPositionJeton(COL,1,X), Ligne is X-1.



%insert(COL,LigneIncr,Couleur) :- placerJeton(COL,LigneIncr,Couleur).


%enlève le jeton de la colonne COL et la ligne NY et de la couleur CouleurJCourant 
remove(COL,NY,CouleurJCourant) :- 
    retract(case(COL,NY,CouleurJCourant)).




col_max(X,Y,COL1,_,RES,COL):-
	X >= Y,
	RES is X,
	COL is COL1,!.

col_max(_,Y,_,COL2,RES,COL):-
	RES is Y,
	COL is COL2,!.



%meilleur_coup(+Col,-Res,-ColRes,+CouleurJoueur)
%Permet de trouver la meilleure case où mettre le jeton pour que l'adversaire gagne 
%Elle trouve la colonne qui a le plus grand nombre de pièces de l'adversaire 
%Elle met un jeton afin d'empêcher une suite de 4 jetons de l'adversaire 
meilleur_coup(COL,RES,_,_):-
	C is 7,
	COL > C,
	RES is 0,!.

meilleur_coup(COL,RES,COL_RES,CouleurJCourant):-
	R is 6,
	top(COL,X),
	NX is X + 1,
	R == NX,
	NCOL is COL + 1,
	meilleur_coup(NCOL,RES,COL_RES,CouleurJCourant),!.

meilleur_coup(COL,RES,COL_RES,CouleurJCourant):-
	ennemi(CouleurJCourant,CouleurAdverse),
	placerJeton(COL,Ligne,CouleurAdverse),
	top(COL,X),
	get_max(COL,X,RES1),
	remove(COL,Ligne,CouleurAdverse),
	NCOL is COL + 1,
	meilleur_coup(NCOL,RES2,COL_RES1,CouleurJCourant),
	col_max(RES1,RES2,COL,COL_RES1,RES,COL_RES).



%calculPositionJeton(+X,+LigneToCheck,-Ligne)
% calculPositionJeton/3(+Colonne,+LigneToCheck,-Ligne)
% Calcule la première ligne vide d'une colonne.
% Retourne l'indice de cette ligne vide.
calculPositionJeton(X,YCheck,YCheck) :-
	caseVide(X,YCheck),
	!.
calculPositionJeton(X,YCheck,Y) :-
	incr(YCheck, YCheck1),
	calculPositionJeton(X,YCheck1,Y).


%glouton(-Coup,+CouleurJCourant)
%Permet de renvoyer dans C la meilleure colonne où mettre le jeton afin de :
% - soit arriver à un état de victoire 
% - soit empêcher l'adversaire d'atteindre la victoire 
glouton(C, CouleurJCourant):-
	possiblite_gagner(1,C,CouleurJCourant),!.
glouton(C, CouleurJCourant):-
	meilleur_coup(1,_,C,CouleurJCourant).

