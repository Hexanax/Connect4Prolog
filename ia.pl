/************* 
 * INSA Lyon - Département informatique - 4IF
 * Hexanôme : H4412
 * Rahim BELATECHE
 * Matheus DE BARROS SILVA
 * Benoit DELEGLISE
 * Allan GUIGAL
 * Alexis METWALLI
 * Matthieu ROUX
 * Mathieu SAUGIER
 ******************/

%%%%%%%%%%%% ia.pl %%%%%%%%%%%%

%%% Code permettant d'appeler les différentes IA %%% 
%%% iaMinimaxOld impémente l'algo du même nom. Il est issu de la source : https://github.com/SIGSWAG/PrologPuissance4. Nous n'y avons apporté aucune modification.
%%% iaAlphabeta implémente un alpha beta pruning. Il a été développé dans la source : https://github.com/SIGSWAG/PrologPuissance4. Nous y avons simplement ajouté nos heuristics


:- module(ia, [iaAleatoire/1
			  ,iaMinimaxOld/7
			  ,poidsPuissance3/1
			  ,poidsPosition/1
			  ,poidsDensite/1
			  ,poidsAdjacence/1
			  ,initDepth/1
			  ,ennemiTest/1
			  ,iaAlphabeta/9
			  ,poidsDefensif/1
			  ,poidsOffensif/1
			  ,poidsCaseTableau/1
			  ,poidsPiegeSept/1
			  ,poidsOpening/1
			  ,poidsPiegeAdjacence/1]
).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(util).
:- use_module(alphaBetaDraw).
:- use_module(miniMax).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats dynamiques %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic poidsPuissance3/1.
:- dynamic poidsPosition/1.
:- dynamic poidsDensite/1.
:- dynamic poidsAdjacence/1.
:- dynamic ennemiTest/1.
:- dynamic initDepth/1.

%Nouveau poids utilisés pour les nouvelles heuristics
:- dynamic poidsDefensif/1.
:- dynamic poidsOffensif/1.
:- dynamic poidsCaseTableau/1.
:- dynamic poidsPiegeSept/1.
:- dynamic poidsOpening/1.
:- dynamic poidsPiegeAdjacence/1.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

iaAleatoire(Coup) :-
	nbColonnes(NBCOLONNES),
	Coup is random(NBCOLONNES)+1,
	coupValide(Coup).
% AI Aléatoire a choisi une colonne pleine, donc on la fait recommencer.
iaAleatoire(Coup) :-
	iaAleatoire(Coup).

get_best((Move,_), Move).

initCaseTest :- case(X,Y,Z), assert(caseTest(X,Y,Z)), false. %on assert une caseTest pour toutes les cases.
initCaseTest.

/**
 * iaAlphabeta implémente un alpha beta pruning. Il a été développé dans la source : https://github.com/SIGSWAG/PrologPuissance4. Nous y avons simplement ajouté nos heuristics
 * */
iaAlphabeta(JoueurCourant,Coup,Profondeur,PoidsCaseTableau,PoidsDefensif,PoidsOffensif,PoidsPiege,PoidsOpening, PoidsAdjacence) :-
	assert(poidsDefensif(PoidsCaseTableau)),
	assert(poidsOffensif(PoidsDefensif)),
	assert(poidsCaseTableau(PoidsOffensif)),
	assert(poidsPiegeSept(PoidsPiege)),
	assert(poidsOpening(PoidsOpening)),
	assert(poidsPiegeAdjacence(PoidsAdjacence)),
	assert(initDepth(Profondeur)),
	initCaseTest,
	ennemi(JoueurCourant,AutreJoueur),
	assert(ennemiTest(AutreJoueur)),
	Alpha is -99999,
	Beta is 99999,
	MaxMin is -1,
	alpha_beta(Profondeur,JoueurCourant, Alpha, Beta, Coup, _, MaxMin),
	retract(ennemiTest(AutreJoueur)),
	retract(initDepth(Profondeur)),
	retractall(caseTest(_,_,_)).

/**
 * iaMinimaxOld implémente l'algo du même nom. Il est issu de la source : https://github.com/SIGSWAG/PrologPuissance4. Nous n'y avons apporté aucune modification.
 * */
iaMinimaxOld(JoueurCourant,Coup,Profondeur,PoidsPosition,PoidsPuissance3,PoidsDensite,PoidsAdjacence) :-
		assert(poidsPosition(PoidsPosition)),
		assert(poidsPuissance3(PoidsPuissance3)),
		assert(poidsDensite(PoidsDensite)),
		assert(poidsAdjacence(PoidsAdjacence)),
		parcoursArbre(JoueurCourant,Profondeur,Coup,_).