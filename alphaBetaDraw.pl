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

%%%%%%%%%%%% alphaBetaDraw.pl %%%%%%%%%%%%

%%% Code implémentant les algorithmes minimax et élagage alpha beta réalisé dans la source : https://github.com/PeredurOmega/PrologPuissance4
%%% Nous n'utilisons pas leur min max.
%%% Code implémentant les heuristics que nous avons développé
%%% Une version de notre tentative d'implémentation de l'algorithme alpha beta est également disponible. Il est basé sur le code : https://github.com/PascalPons/connect4/blob/part4/solver.cpp
%%% et : https://ai.ia.agh.edu.pl/pl:prolog:pllib:minimax_move


:- module(alphaBetaDraw, [
	caseTest/3,
	evaluate_and_choose/6,
	minimax/5,
	evaluate_and_choose_ab/8,
 	alpha_beta/7
]).

:- use_module(jeu).
:- use_module(util).

:- dynamic caseTest/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     Alpha beta     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ponderate(MaxMin, 0, Value, Value1) :- 
	MaxMin < 0,
	Value1 is Value * MaxMin,!.

ponderate(MaxMin, 0, Value, Value) :- 
	MaxMin > 0,!.

ponderate(MaxMin, Depth, Value, Value1) :- 
	Depth > 0,
	Value1 is -Value.

evaluate_and_choose_ab([Move|Moves], InitPlayer, Depth, Alpha, Beta, Record, BestMove, MaxMin) :-
	move(Move, MaxMin, InitPlayer),
	alpha_beta(Depth, InitPlayer, Alpha, Beta, MoveX, Value, MaxMin),
	undo_move(Move, Color),
	ponderate(MaxMin, Depth, Value, Value1),
	cutoff(Move, Value1, Depth, Alpha, Beta , Moves ,InitPlayer, Record, BestMove, MaxMin).

evaluate_and_choose_ab([], InitPlayer, Depth, Alpha, Beta, Move, (Move, Alpha), MaxMin).


alpha_beta(0, InitPlayer, Alpha, Beta, Move, Value, MaxMin) :-
	value(InitPlayer, Value),
	!.

alpha_beta(Depth, InitPlayer, Alpha, Beta, Move, Value, MaxMin) :-
	findall(X, (between(1,7,X),coupValide(X)), Moves),
	size(Moves,L),
	L > 0,
	Alpha1 is -Beta,
	Beta1 is -Alpha,
	NewDepth is Depth-1,
	NewMaxMin is -MaxMin,
	evaluate_and_choose_ab(Moves, InitPlayer, NewDepth, Alpha1, Beta1, nil, (Move, Value), NewMaxMin).

alpha_beta(Depth, InitPlayer, Alpha, Beta, Move, Value, MaxMin) :-
	findall(X, (between(1,7,X),coupValide(X)), Moves),
	size(Moves,0),
	alpha_beta(0, InitPlayer, Alpha, Beta, Move, Value, MaxMin).

size([],0).

size([H|T],L) :-
	size(T,L1),
	L is 1 + L1.


cutoff(Move,Value,Depth,Alpha,Beta,Moves, InitPlayer, Move1,(Move1,Value), MaxMin):-
	Value >= Beta.

cutoff(Move,Value,Depth,Alpha, Beta , Moves, InitPlayer, Move1,BestMove, MaxMin) :- 
	Alpha < Value,
	Value < Beta,
	evaluate_and_choose_ab(Moves, InitPlayer, Depth, Value, Beta, Move, BestMove, MaxMin).

cutoff(Move,Value,Depth,Alpha, Beta , Moves, InitPlayer, Move1,BestMove, MaxMin):- 
	Value =< Alpha, 
	evaluate_and_choose_ab(Moves, InitPlayer, Depth, Alpha, Beta, Move1, BestMove, MaxMin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Min Max classique   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


evaluate_and_choose([Move|Moves], InitPlayer, Depth, MaxMin, Record, Best) :-
	move(Move, MaxMin, InitPlayer),
	minimax(Depth, InitPlayer, MaxMin, MoveX, Value),
	update(Move, Value, Record, Record1, MaxMin),
	undo_move(Move, Color),
	evaluate_and_choose(Moves, InitPlayer, Depth, MaxMin, Record1, Best).

evaluate_and_choose([], InitPlayer, Depth, MaxMin, Record, Record).

minimax(0, InitPlayer, MaxMin, Move, Value) :-
	value(InitPlayer, V),
	Value is V.

minimax(Depth, InitPlayer, MaxMin, Move, Value) :-
	Depth > 0,
	findall(X, (between(1,7,X),coupValide(X)), Moves),
	NewDepth is Depth-1,
	NewMaxMin is -MaxMin,
	evaluate_and_choose(Moves, InitPlayer, NewDepth, NewMaxMin, (nil, 1000*MaxMin), (Move, Value)).

update(Move, Value, (Move1, Value1), (Move1, Value1), MinMax) :-
	MinMax > 0,
	Value =< Value1,!.

update(Move, Value, (Move1, Value1), (Move, Value), MinMax) :-
	MinMax > 0,
	Value > Value1,!.

update(Move, Value, (Move1, Value1), (Move1, Value1), MinMax) :-
	MinMax < 0,
	Value > Value1,!.

update(Move, Value, (Move1, Value1), (Move, Value), MinMax) :-
	MinMax < 0,
	Value =< Value1.

move(Move, MinMax, InitPlayer) :- 
	InitPlayer==jaune, 
	MinMax < 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, rouge)),!.

move(Move, MinMax, InitPlayer) :- 
	InitPlayer==jaune, 
	MinMax > 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, jaune)),!.

move(Move, MinMax, InitPlayer) :- 
	InitPlayer==rouge, 
	MinMax > 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, rouge)),!.

move(Move, MinMax, InitPlayer) :- 
	InitPlayer==rouge, 
	MinMax < 0,
	calculPositionJeton(Move, 1, X),
	assert(caseTest(Move, X, jaune)).

calculPositionJetonTest(X,YCheck,YCheck) :- 
	caseVideTest(X,YCheck), !.
calculPositionJetonTest(X,YCheck,Y) :- 
	incr(YCheck, YCheck1), calculPositionJeton(X,YCheck1,Y).

undo_move(Move, Color) :-
	calculPositionJetonTest(Move, 1, X),
	LinePos is X-1,
	retract(caseTest(Move, LinePos, Color)).

value(InitPlayer, V) :-
	eval(InitPlayer, Score),
	V is Score.


%%%%%% Notre version du alphaBeta basé sur  les codes : https://github.com/PascalPons/connect4/blob/part4/solver.cpp
%%%%%% et : https://ai.ia.agh.edu.pl/pl:prolog:pllib:minimax_move
%%%%%% L'implémentation provoque une erreur d'évaluation dans le cas ou le score est issue d'un appel récusrif 
%%%%%% à la profondeur 0 -> les ia sont donc moins performante. 
%%%%%% L'utilisation de la pondaration comme dans le projet : https://github.com/PeredurOmega/PrologPuissance4
%%%%%% permet de résoudre cette erreur. Nous avons donc utilisé directement leur algo.
/*
alphaBeta(0,CouleurJoueur,Alpha,Beta , Move, Value,Maximizer):- 
    evaluate(Maximizer, Value, Move), !.

alphaBeta(Profondeur, CouleurJoueur, Alpha, Beta, Move, Value, Maximizer):- 
    Profondeur > 0,
    findall(X, (between(1,7,X),coupValide(X)), Moves),
    taille(Moves,Size),
    Size > 0,
    Profondeur1 is Profondeur - 1,
    evaluate_and_choose(Moves, CouleurJoueur,Profondeur1,Alpha,Beta,nil, (Move,Value),Maximizer).
alphaBeta(Profondeur, CouleurJoueur, Alpha, Beta, Move, Value, Maximizer):- 
    Profondeur > 0,
    findall(X, (between(1,7,X),coupValide(X)), Moves),
    taille(Moves,0),
    alphaBeta(0,CouleurJoueur,Alpha,Beta , Move, Value,Maximizer).

taille([],0).

taille([H|T],L) :-
	taille(T,L1),
	L is 1 + L1.

evaluate_and_choose([Move|Moves],CouleurJoueur,Profondeur,Alpha,Beta,Record,BestMove,Maximizer) :- 
	move(Move,CouleurJoueur,CouleurJoueurSuivant,Ligne),
    Alpha1 is -Beta,
    Beta1 is -Alpha,
    alphaBeta(Profondeur,CouleurJoueurSuivant,Alpha1,Beta1,MoveX,Value, Maximizer),
    undoMove(Move, Ligne, CouleurJoueur),
    Value1 is Value * -1,
    cutoff(Move,Value1,Profondeur,Alpha,Beta,Moves,CouleurJoueur,Record,BestMove, Maximizer).

evaluate_and_choose([],CouleurJoueur,Profondeur,Alpha,Beta,Move,(Move,Alpha),Maximizer).

cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,(Move,Value),Maximizer) :- 
    Value >= Beta.

cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,BestMove,Maximizer) :- 
    Alpha < Value,
	Value < Beta,
    evaluate_and_choose(Moves,CouleurJoueur,D,Value,Beta,Move,BestMove, Maximizer).

cutoff(Move,Value,D,Alpha,Beta,Moves,CouleurJoueur,Record,BestMove,Maximizer) :- 
    Value =< Alpha,
    evaluate_and_choose(Moves,CouleurJoueur,D,Value,Beta,Record,BestMove, Maximizer).
    
    
move(Move, CouleurJoueur, CouleurJoueurSuivant,Ligne):-
    CouleurJoueur == jaune,
    CouleurJoueurSuivant = rouge,
    insererJeton(Move, Y, CouleurJoueur),
    Ligne is Y,
    !.

move(Move, CouleurJoueur, CouleurJoueurSuivant, Ligne):-
    CouleurJoueur == rouge,
    CouleurJoueurSuivant = jaune,
    insererJeton(Move, Y, CouleurJoueur),
    Ligne is Y,
    !.

undoMove(Move, Ligne, CouleurJoueur):-
    retract(caseTest(Move, Ligne, CouleurJoueur)).

evaluate(CouleurJoueur, Value, Move):- 
    eval(CouleurJoueur, Score, Move),
    Value is Score.*/

% eval/2(+CouleurJoueur, -Score)
% Evalue le plateau en attribuant au joueur courant un score évalué à partir d'heuristics.
% Cela permet d'évaluer la qualité des coups joués.
eval(CouleurJoueur, Score):- 
    %Active l'heuristic si le poids est >=1
    poidsCaseTableau(PoidsCaseTableau),
    poidsDefensif(PoidsDefensif),
    poidsOffensif(PoidsOffensif),
    poidsPiegeSept(PoidsPiege),
    poidsOpening(PoidsOpening),
    poidsPiegeAdjacence(PoidsAdjacence),
    defensiveIA(CouleurJoueur, ScoreDefensif, PoidsDefensif),
    offensiveIA(CouleurJoueur, ScoreOffensif, PoidsOffensif),
    positionIA(CouleurJoueur, ScorePosition, PoidsCaseTableau),
    piege7IA(CouleurJoueur, ScorePiege, PoidsPiege),
    piegeAdjacence(CouleurJoueur, ScoreAdjacence, PoidsAdjacence),
    opening(CouleurJoueur, ScoreOpening, PoidsOpening),
    random_between(-5, 5, Perturbation),
    ScoreFinal is ScoreDefensif * PoidsDefensif
            + ScorePosition * PoidsCaseTableau
            + ScoreOffensif * PoidsOffensif
            + ScorePiege * PoidsPiege
            + ScoreOpening * PoidsOpening
            + ScoreAdjacence * PoidsAdjacence,
    Score is ScoreFinal * (1 + Perturbation/100).

% writefacts/2(+X, -Score)
% Permet d'ecrire des informations dans un fichier texte pour le debuggage
writefacts(X, Score):-
    open('evals.txt',append,Out),
    write(Out, X),
    write(Out, '   '),
    write(Out, Score),
    write(Out, '\n'),
    close(Out).


%%%%% placerJeton %%%%%
% coupValide/1(-Colonne)
% Vérifie si un jeton est jouable dans cette colonne.
% Vrai si le coup est valide.
coupValide(X) :- nbColonnes(NBCOLONNES), X=<NBCOLONNES, X>=1, nbLignes(NBLIGNES), caseVideTest(X,NBLIGNES).

caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).

% insererJeton/3(-Colonne, +Ligne, -Couleur)
% Insère, sans vérification, un jeton de la couleur donnée, dans la colonne donnée.
% Y s'unifie à la ligne d'insertion,
insererJeton(X,Y,C) :- calculPositionJeton(X, 1, Y), assert(caseTest(X,Y,C)).

% calculPositionJeton/3(+Colonne,+LigneToCheck,-Ligne)
% Calcule la première ligne vide d'une colonne.
% Ligne s'unfinie à l'indice de la première ligne vide de la colonne.
calculPositionJeton(X,YCheck,YCheck) :- caseVideTest(X,YCheck), !.
calculPositionJeton(X,YCheck,Y) :- incr(YCheck, YCheck1), calculPositionJeton(X,YCheck1,Y).

/****************************************************************
                    HEURISTIQUES D'ALIGNEMENT 
                    DEFENSIVE ET OFFENSIVES 

Evaluent pour chaque pion son intégration dans un alignement de plusieurs pions 
en ligne, en colonne ou en diagonale et attribuent un score. 
Plus il y a d'alignement et plus il y a de chance de réalisé un puissance4.
On a donc 3 facteurs d'alignement : 2 pions, 3pions et 4. 
L'heuristique de défense cherche donc à minimiser le score des alignements adverse.
L'heuristique de offensive cherche donc à maximiser le score des alignements courant.

*****************************************************************/

%% Donne la couleur inverse à celle du joueur courant
couleurAdverse(jaune, rouge).
couleurAdverse(rouge, jaune).

%%%%%%%%%%%

% defensiveIA/3(+CouleurJoueur,-ScoreDefensif,+PoidsDefensif)
% Donne une note d'autant plus forte que l'alignement du joueur 
% adverse est dangereux
defensiveIA(CouleurJoueur, ScoreDefensif, PoidsDefensif):- 
    PoidsDefensif > 0,
    findall(S, evalDangerAdverse(CouleurJoueur, S),Scores),
    sum(Scores, ScoreDefensifTot),
    ScoreDefensif is ScoreDefensifTot.
defensiveIA(_, 0, _).

% offensiveIA/3(+CouleurJoueur,-ScoreOffensif,+PoidsOffensif)
% Donne une note d'autant plus forte que l'alignement du joueur 
% courant est avantageux
offensiveIA(CouleurJoueur, ScoreOffensif, PoidsOffensif):- 
    PoidsOffensif > 0,
    findall(S, evalDangerJoueur(CouleurJoueur, S),Scores),
    sum(Scores, ScoreOffensiffTot),
    ScoreOffensif is ScoreOffensiffTot.
offensiveIA(_, 0, _).

% evalDangerAdverse/2(+CouleurJoueur,-Score)
% Donne le score d'évaluation d'alignement d'un pion adverse
evalDangerAdverse(CouleurJoueur, Score) :-
    couleurAdverse(CouleurJoueur, JoueurAdverse),
    caseTest(X,Y,JoueurAdverse),
    calculerScoreAlignement(X, Y, JoueurAdverse, S),
    Score is S * -1.

% evalDangerJoueur/2(+CouleurJoueur,-Score)
% Donne le score d'évaluation d'alignement d'un pion du joueur courant
evalDangerJoueur(CouleurJoueur, Score) :-
    caseTest(X,Y,CouleurJoueur),
    calculerScoreAlignement(X, Y, JoueurAdverse, S),
    Score is S.

% evalDangerJoueur/2(+X,+Y,+CouleurJoueur,-Score)
% Calcul le score d'alignement d'un pion dans toute les directions 
calculerScoreAlignement(X,Y, CouleurJoueur, Score ):-
    evaluerLigne(X,Y,CouleurJoueur,LigneGauche1, LigneGauche2, LigneGauche3, LigneDroite1, LigneDroite2, LigneDroite3),
    evaluerColonne(X,Y,CouleurJoueur,ColonneGauche1, ColonneGauche2, ColonneGauche3, ColonneDroite1, ColonneDroite2, ColonneDroite3),
    evaluerDiag1(X,Y,CouleurJoueur,Diag1Gauche1, Diag1Gauche2, Diag1Gauche3, Diag1Droite1, Diag1Droite2, Diag1Droite3),
    evaluerDiag2(X,Y,CouleurJoueur,Diag2Gauche1, Diag2Gauche2, Diag2Gauche3, Diag2Droite1, Diag2Droite2, Diag2Droite3),
    align4Pions(LigneGauche1, LigneGauche2, LigneGauche3, LigneDroite1, LigneDroite2, LigneDroite3, LigneScore4),
    align4Pions(ColonneGauche1, ColonneGauche2, ColonneGauche3, ColonneDroite1, ColonneDroite2, ColonneDroite3, ColonneScore4),
    align4Pions(Diag1Gauche1, Diag1Gauche2, Diag1Gauche3, Diag1Droite1, Diag1Droite2, Diag1Droite3, Diag1Score4),
    align4Pions(Diag2Gauche1, Diag2Gauche2, Diag2Gauche3, Diag2Droite1, Diag2Droite2, Diag2Droite3, Diag2Score4),
    findall(S, align3Pions(LigneGauche1, LigneGauche2, LigneGauche3, LigneDroite1, LigneDroite2, LigneDroite3, S),ListLigneScore3),
    sum(ListLigneScore3, LigneScore3),
    findall(S, align3Pions(ColonneGauche1, ColonneGauche2, ColonneGauche3, ColonneDroite1, ColonneDroite2, ColonneDroite3, S),ListColonneScore3),
    sum(ListColonneScore3, ColonneScore3),
    findall(S, align3Pions(Diag1Gauche1, Diag1Gauche2, Diag1Gauche3, Diag1Droite1, Diag1Droite2, Diag1Droite3, S),ListDiag1Score3),
    sum(ListDiag1Score3, Diag1Score3),
    findall(S, align3Pions(Diag2Gauche1, Diag2Gauche2, Diag2Gauche3, Diag2Droite1, Diag2Droite2, Diag2Droite3, S),ListDiag2Score3),
    sum(ListDiag2Score3, Diag2Score3),
    findall(S, align2Pions(LigneGauche1, LigneGauche2, LigneGauche3, LigneDroite1, LigneDroite2, LigneDroite3, S),ListLigneScore2),
    sum(ListLigneScore2, LigneScore2),
    findall(S, align2Pions(ColonneGauche1, ColonneGauche2, ColonneGauche3, ColonneDroite1, ColonneDroite2, ColonneDroite3, S),ListColonneScore2),
    sum(ListColonneScore2, ColonneScore2),
    findall(S, align2Pions(Diag1Gauche1, Diag1Gauche2, Diag1Gauche3, Diag1Droite1, Diag1Droite2, Diag1Droite3, S),ListDiag1Score2),
    sum(ListDiag1Score2, Diag1Score2),
    findall(S, align2Pions(Diag2Gauche1, Diag2Gauche2, Diag2Gauche3, Diag2Droite1, Diag2Droite2, Diag2Droite3, S),ListDiag2Score2),
    sum(ListDiag2Score2, Diag2Score2),
    Score is LigneScore4 + ColonneScore4 + Diag1Score4 + Diag2Score4 
            + LigneScore3 + ColonneScore3 + Diag1Score3 + Diag2Score3
            + LigneScore2 + ColonneScore2 + Diag1Score2 + Diag2Score2.


/******************************************/
%         Liste des configurations         %
%         qui augmentent le score :        %
%le pion étudié peut être n'importe quel x %
/******************************************/
%%Configuration victorieuse
%x,x,x,x%
align4Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, Droite3, Score):-
    (  
        1+Droite1+Droite2+Droite3 >= 4, Score is 1000,!;
        1+Gauche1+Droite1+Droite2 >= 4,Score is 1000,!;
        1+Gauche1+Gauche2+Droite1 >= 4,Score is 1000,!;
        1+Gauche1+Gauche2+Gauche3 >= 4,Score is 1000,!);
    (Score is 0).

%%Configuration : 
%_,x,x,x%
align3Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, _, Score):-
    (  
        Gauche1==0, Droite1==1, Droite2==1, Score is 200;
        Gauche1==1, Gauche2==0, Droite1==1, Score is 200;
        Gauche1==1, Gauche2==1, Gauche3==0, Score is 200);
    (Score is 0).

%%Configuration : 
%x,_,x,x%
align3Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==0, Droite2==1, Droite3==1, Score is 200;
        Gauche1==0, Gauche2==1, Droite1==1, Score is 200;
        Gauche1==1, Gauche2==0, Gauche3==1, Score is 200);
    (Score is 0).    

%%Configuration : 
%x,x,_,x%
align3Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==1, Droite2==0, Droite3==1, Score is 200;
        Gauche1==1, Droite1==0, Droite2==1, Score is 200;
        Gauche1==0, Gauche2==1, Gauche3==1, Score is 200);
    (Score is 0).

%%Configuration : 
%x,x,x,_%
align3Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==1, Droite2==1, Droite3==0, Score is 100;
        Gauche1==1, Droite1==1, Droite2==0, Score is 100;
        Gauche1==1, Gauche2==1, Droite1==0, Score is 100);
    (Score is 0).    

%%Configuration : 
%_,_,x,x%
align2Pions(Gauche1, Gauche2, Gauche3, Droite1, _, _, Score):-
    (  
        Gauche1==0, Gauche2==0, Droite1==1, Score is 10;
        Gauche1==1, Gauche2==0, Gauche3==0, Score is 10);
    (Score is 0).    
  
%%Configuration :   
%_,x,_,x%
align2Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, _, Score):-
    (  
        Gauche1==0, Droite1==0, Droite2==1, Score is 10;
        Gauche1==0, Gauche2==1, Gauche3==0, Score is 10);
    (Score is 0).    

%%Configuration : 
%_,x,x,_%
align2Pions(Gauche1, Gauche2, _, Droite1, Droite2, _, Score):-
    (  
        Gauche1==0, Droite1==1, Droite2==0, Score is 10;
        Gauche1==1, Gauche2==0, Droite1==0, Score is 10);
    (Score is 0).
 
%%Configuration : 
%x,_,_,x%
align2Pions(Gauche1, Gauche2, Gauche3, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==0, Droite2==0, Droite3==1, Score is 10;
        Gauche1==0, Gauche2==0, Gauche3==1, Score is 10);
    (Score is 0).

%%Configuration : 
%x,_,x,_%
align2Pions(Gauche1, Gauche2, _, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==0, Droite2==1, Droite3==0, Score is 10;
        Gauche1==0, Gauche2==1, Droite1==0, Score is 10);
    (Score is 0).  

%%Configuration :    
%x,x,_,_%
align2Pions(Gauche1, _, _, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==1, Droite2==0, Droite3==0, Score is 10;
        Gauche1==1, Droite1==0, Droite2==0, Score is 10);
    (Score is 0).  

% evaluerLigne(+X,+Y,+Joueur,-Gauche1,-Gauche2,-Gauche3,-Droite1,-Droite2,-Droite3)
% Permet de déterminer l'état des 3 cases à droite et à gauche d'un pion en ligne.
% Inspiré des méthodes réalisé dans le projet d'origine : https://github.com/SIGSWAG/PrologPuissance4
% 1 si occupé par le joueur en paramètre
% 0 si vide c
% -1 sinon
evaluerLigne(X,Y,Joueur,Gauche1,Gauche2,Gauche3,Droite1, Droite2, Droite3) :-
    verifierGaucheLigne(X,Y,Joueur,Gauche1,Gauche2,Gauche3),
    verifierDroiteLigne(X,Y,Joueur,Droite1,Droite2,Droite3).

%cases à droite
verifierDroiteLigne(X,Y,Joueur,Droite1,Droite2,Droite3):-
    incr(X,X1),
    incr(X1,X2),
    incr(X2,X3), 
    (X1 =< 7, caseTest(X1,Y,Joueur), Droite1 is 1,!;
     X1 =< 7, not(caseTest(X1,Y,_)), Droite1 is 0,!;
     Droite1 is -1),
    (X2 =< 7, caseTest(X2,Y,Joueur), Droite2 is 1,!;
     X2 =< 7, not(caseTest(X2,Y,_)), Droite2 is 0,!;
     Droite2 is -1),
    (X3 =< 7, caseTest(X3,Y,Joueur), Droite3 is 1,!;
     X3 =< 7, not(caseTest(X3,Y,_)), Droite3 is 0,!;
     Droite3 is -1).

%cases à gauche
verifierGaucheLigne(X,Y,Joueur,Gauche1,Gauche2,Gauche3):-
    decr(X,X1),
    decr(X1,X2),
    decr(X2,X3), 
    (X1 >=1 , caseTest(X1,Y,Joueur), Gauche1 is 1,!;
     X1 >=1 , not(caseTest(X1,Y,_)), Gauche1 is 0,!;
     Gauche1 is -1),
    (X2 >=1 , caseTest(X2,Y,Joueur), Gauche2 is 1,!;
     X2 >=1 , not(caseTest(X2,Y,_)), Gauche2 is 0,!;
     Gauche2 is -1),
    (X3 >=1 , caseTest(X3,Y,Joueur), Gauche3 is 1,!;
     X3 >=1 , not(caseTest(X3,Y,_)), Gauche3 is 0,!;
     Gauche3 is -1).

% evaluerColonne(+X,+Y,+Joueur,-Gauche1,-Gauche2,-Gauche3,-Droite1,-Droite2,-Droite3)
% Permet de déterminer l'état des 3 cases en haut et en bas d'un pion en colonne.
% Inspiré des méthodes réalisé dans le projet d'origine : https://github.com/SIGSWAG/PrologPuissance4
% 1 si occupé par le joueur en paramètre
% 0 si vide c
% -1 sinon
evaluerColonne(X,Y,Joueur,Gauche1,Gauche2,Gauche3,Droite1, Droite2, Droite3) :-
    verifierGaucheColonne(X,Y,Joueur,Gauche1,Gauche2,Gauche3),
    verifierDroiteColonne(X,Y,Joueur,Droite1,Droite2,Droite3).

%cases en haut
verifierDroiteColonne(X,Y,Joueur,Droite1,Droite2,Droite3):-
    incr(Y,Y1),
    incr(Y1,Y2),
    incr(Y2,Y3), 
    (Y1 =< 6, caseTest(X,Y1,Joueur), Droite1 is 1,!;
     Y1 =< 6, not(caseTest(X,Y1,_)), Droite1 is 0,!;
     Droite1 is -1),
    (Y2 =< 6, caseTest(X,Y2,Joueur), Droite2 is 1,!;
     Y2 =< 6, not(caseTest(X,Y2,_)), Droite2 is 0,!;
     Droite2 is -1),
    (Y3 =< 6, caseTest(X,Y3,Joueur), Droite3 is 1,!;
     Y3 =< 6, not(caseTest(X,Y3,_)), Droite3 is 0,!;
     Droite3 is -1).

%cases en bas
verifierGaucheColonne(X,Y,Joueur,Gauche1,Gauche2,Gauche3):-
    decr(Y,Y1),
    decr(Y1,Y2),
    decr(Y2,Y3), 
    (Y1 >=1 , caseTest(X,Y1,Joueur), Gauche1 is 1,!;
     Y1 >=1 , not(caseTest(X,Y1,_)), Gauche1 is 0,!;
     Gauche1 is -1),
    (Y2 >=1 , caseTest(X,Y2,Joueur), Gauche2 is 1,!;
     Y2 >=1 , not(caseTest(X,Y2,_)), Gauche2 is 0,!;
     Gauche2 is -1),
    (Y3 >=1 , caseTest(X,Y3,Joueur), Gauche3 is 1,!;
     Y3 >=1 , not(caseTest(X,Y3,_)), Gauche3 is 0,!;
     Gauche3 is -1).

% evaluerColonne(+X,+Y,+Joueur,-Gauche1,-Gauche2,-Gauche3,-Droite1,-Droite2,-Droite3)
% Permet de déterminer l'état des 3 cases en haut à droite et en bas à gauche d'un pion (la diagonale 1 du plateau).
% Inspiré des méthodes réalisé dans le projet d'origine : https://github.com/SIGSWAG/PrologPuissance4
% 1 si occupé par le joueur en paramètre
% 0 si vide c
% -1 sinon
evaluerDiag1(X,Y,Joueur,Gauche1,Gauche2,Gauche3,Droite1, Droite2, Droite3) :-
    verifierGaucheColonne(X,Y,Joueur,Gauche1,Gauche2,Gauche3),
    verifierDroiteColonne(X,Y,Joueur,Droite1,Droite2,Droite3).

%cases en haut à droite
verifierDroiteDiag1(X,Y,Joueur,Droite1,Droite2,Droite3):-
    incr(Y,Y1),
    incr(Y1,Y2),
    incr(Y2,Y3), 
    incr(X,X1),
    incr(X1,X2),
    incr(X2,X3), 
    (Y1 =< 6, X1 =< 7, caseTest(X1,Y1,Joueur), Droite1 is 1,!;
     Y1 =< 6, X1 =< 7, not(caseTest(X1,Y1,_)), Droite1 is 0,!;
     Droite1 is -1),
    (Y2 =< 6, X2 =< 7, caseTest(X2,Y2,Joueur), Droite2 is 1,!;
     Y2 =< 6, X2 =< 7, not(caseTest(X2,Y2,_)), Droite2 is 0,!;
     Droite2 is -1),
    (Y3 =< 6, X3 =< 7, caseTest(X3,Y3,Joueur), Droite3 is 1,!;
     Y3 =< 6, X3 =< 7, not(caseTest(X3,Y3,_)), Droite3 is 0,!;
     Droite3 is -1).


%cases en bas à gauche
verifierGaucheDiag1(X,Y,Joueur,Gauche1,Gauche2,Gauche3):-
    decr(Y,Y1),
    decr(Y1,Y2),
    decr(Y2,Y3), 
    decr(X,X1),
    decr(X1,X2),
    decr(X2,X3), 
    (Y1 >=1 ,X1 >= 1, caseTest(X1,Y1,Joueur), Gauche1 is 1,!;
     Y1 >=1 ,X1 >= 1, not(caseTest(X1,Y1,_)), Gauche1 is 0,!;
     Gauche1 is -1),
    (Y2 >=1 ,X2 >= 1, caseTest(X2,Y2,Joueur), Gauche2 is 1,!;
     Y2 >=1 ,X2 >= 1, not(caseTest(X2,Y2,_)), Gauche2 is 0,!;
     Gauche2 is -1),
    (Y3 >=1 ,X3 >= 1, caseTest(X3,Y3,Joueur), Gauche3 is 1,!;
     Y3 >=1 ,X3 >= 1, not(caseTest(X3,Y3,_)), Gauche3 is 0,!;
     Gauche3 is -1).

% evaluerColonne(+X,+Y,+Joueur,-Gauche1,-Gauche2,-Gauche3,-Droite1,-Droite2,-Droite3)
% Permet de déterminer l'état des 3 cases en haut à gauche et en bas à droite d'un pion (la diagonale 2 du plateau).
% Inspiré des méthodes réalisé dans le projet d'origine : https://github.com/SIGSWAG/PrologPuissance4
% 1 si occupé par le joueur en paramètre
% 0 si vide c
% -1 sinon
evaluerDiag2(X,Y,Joueur,Gauche1,Gauche2,Gauche3,Droite1, Droite2, Droite3) :-
    verifierGaucheColonne(X,Y,Joueur,Gauche1,Gauche2,Gauche3),
    verifierDroiteColonne(X,Y,Joueur,Droite1,Droite2,Droite3).

%cases en bas à droite
verifierDroiteDiag2(X,Y,Joueur,Droite1,Droite2,Droite3):-
    decr(Y,Y1),
    decr(Y1,Y2),
    decr(Y2,Y3), 
    incr(X,X1),
    incr(X1,X2),
    incr(X2,X3), 
    (Y1 >=1 6, X1 =< 7, caseTest(X1,Y1,Joueur), Droite1 is 1,!;
     Y1 >=1 6, X1 =< 7, not(caseTest(X1,Y1,_)), Droite1 is 0,!;
     Droite1 is -1),
    (Y2 >=1 6, X2 =< 7, caseTest(X2,Y2,Joueur), Droite2 is 1,!;
     Y2 >=1 6, X2 =< 7, not(caseTest(X2,Y2,_)), Droite2 is 0,!;
     Droite2 is -1),
    (Y3 >=1 6, X3 =< 7, caseTest(X3,Y3,Joueur), Droite3 is 1,!;
     Y3 >=1 6, X3 =< 7, not(caseTest(X3,Y3,_)), Droite3 is 0,!;
     Droite3 is -1).

%cases en haut à gauche
verifierGaucheDiag2(X,Y,Joueur,Gauche1,Gauche2,Gauche3):-
    incr(Y,Y1),
    incr(Y1,Y2),
    incr(Y2,Y3), 
    decr(X,X1),
    decr(X1,X2),
    decr(X2,X3), 
    (Y1 =< 6, X1 >= 1, caseTest(X1,Y1,Joueur), Droite1 is 1,!;
     Y1 =< 6, X1 >= 1, not(caseTest(X1,Y1,_)), Droite1 is 0,!;
     Droite1 is -1),
    (Y2 =< 6, X2 >= 1, caseTest(X2,Y2,Joueur), Droite2 is 1,!;
     Y2 =< 6, X2 >= 1, not(caseTest(X2,Y2,_)), Droite2 is 0,!;
     Droite2 is -1),
    (Y3 =< 6, X3 >= 1, caseTest(X3,Y3,Joueur), Droite3 is 1,!;
     Y3 =< 6, X3 >= 1, not(caseTest(X3,Y3,_)), Droite3 is 0,!;
     Droite3 is -1).



/****************************************************************
                    HEURISTIQUES DE POSITIONNEMENT 
                    DES PIONS DANS LE PLATEAU

Evaluent pour chaque pion sa position dans le plateau de jeu.
Les positions centrales sont plus avantageuses et apportent donc plus de
points. Les scores des pions adverses sont déduits du score des pions courant. 
Heurirstique basé sur les sites : https://www.christian-schmidt.fr/puissance4
et https://www.researchgate.net/publication/331552609_Research_on_Different_Heuristics_for_Minimax_Algorithm_Insight_from_Connect-4_Game

*****************************************************************/

% positionIA/3(+CouleurJoueur,-ScorePosition,+PoidsCaseTableau)
% Donne une note d'autant plus forte que le positionnement des pions 
% du joueur est central.
positionIA(CouleurJoueur, ScorePosition, PoidsCaseTableau):- 
    PoidsCaseTableau > 0,
    findall(S, calculerScorePlateau(CouleurJoueur, S),Scores),
    sum(Scores, ScoreDefensifTotJoueur),
    couleurAdverse(CouleurJoueur, JoueurAdverse),
    findall(S, calculerScorePlateau(JoueurAdverse,S), ScoresAdverse), 
    sum(ScoresAdverse, ScoreDefensifTotAdverse),
    ScorePosition is ScoreDefensifTotJoueur - ScoreDefensifTotAdverse.
positionIA(_, 0, _).

% calculerScorePlateau(+CouleurJoueur, -Score)
% Donne une note en fonction de la position d'un pion.
calculerScorePlateau(CouleurJoueur, Score):-
    caseTest(X,Y, CouleurJoueur),
    valeurCasePlateau(X,Y,S),
    Score is S.

/* 
Les scores donnés par défaut pour les cases sont :
        3	4	5	7	5	4	3
        4	6	8	10	8	6	4
        5	8	11	13	11	8	5
        5	8	11	13	11	8	5
        4	6	8	10	8	6	4
        3	4	5	7	5	4	3
                                                        */
valeurCasePlateau(1,1,3).
valeurCasePlateau(2,1,4). 
valeurCasePlateau(3,1,5). 
valeurCasePlateau(4,1,7). 
valeurCasePlateau(5,1,5). 
valeurCasePlateau(6,1,4). 
valeurCasePlateau(7,1,3). 

valeurCasePlateau(1,2,4).
valeurCasePlateau(2,2,6). 
valeurCasePlateau(3,2,8). 
valeurCasePlateau(4,2,10). 
valeurCasePlateau(5,2,8). 
valeurCasePlateau(6,2,6). 
valeurCasePlateau(7,2,4). 

valeurCasePlateau(1,3,5).
valeurCasePlateau(2,3,8). 
valeurCasePlateau(3,3,11). 
valeurCasePlateau(4,3,13). 
valeurCasePlateau(5,3,11). 
valeurCasePlateau(6,3,8). 
valeurCasePlateau(7,3,5). 

valeurCasePlateau(1,4,5).
valeurCasePlateau(2,4,8). 
valeurCasePlateau(3,4,11). 
valeurCasePlateau(4,4,13). 
valeurCasePlateau(5,4,11). 
valeurCasePlateau(6,4,8). 
valeurCasePlateau(7,4,5). 

valeurCasePlateau(1,5,4).
valeurCasePlateau(2,5,6). 
valeurCasePlateau(3,5,8). 
valeurCasePlateau(4,5,10). 
valeurCasePlateau(5,5,8). 
valeurCasePlateau(6,5,6). 
valeurCasePlateau(7,5,4). 

valeurCasePlateau(1,6,3).
valeurCasePlateau(2,6,4). 
valeurCasePlateau(3,6,5). 
valeurCasePlateau(4,6,7). 
valeurCasePlateau(5,6,5). 
valeurCasePlateau(6,6,4). 
valeurCasePlateau(7,6,3). 


/****************************************************************
                    HEURISTIQUES BASEE SUR LE PIEGE 
                    DE FAIRE DES 7

Evaluent pour chaque pion s'il est à l'intersection de 2 pions en ligne ou 
en colonne avec 2 pions en diagonale pour le joueur courant. 
Cela permet de former un 7 qui permet de tendre un piège à l'adversaire en 
donnant la possibilité de gagner en plusieurs points
On estime qu'un 7 selon son orientation est plus ou moins avantageux

*****************************************************************/

% piege7IA/3(+CouleurJoueur,-ScorePiege,+PoidsPiege)
% Donne une note d'autant plus forte que le positionnement des pions 
% forme des 7
piege7IA(CouleurJoueur, ScorePiege, PoidsPiege):-
    PoidsPiege > 0,
    findall(S, evalPiege(CouleurJoueur, S),Scores),
    sum(Scores, ScorePiegeTot),
    ScorePiege is ScorePiegeTot.
piege7IA(_, 0, _).    

% evalPiege/2(+CouleurJoueur,-Score)
% évalue si un plan est au centre d'un 7
evalPiege(CouleurJoueur, Score) :-
    caseTest(X,Y,CouleurJoueur),
    calculerScorePiege(X, Y, CouleurJoueur, S),
    Score is S * -1.

calculerScorePiege(X, Y, CouleurJoueur, Score) :- 
    evaluerLigne(X,Y,CouleurJoueur,LigneGauche1, LigneGauche2, _, LigneDroite1, LigneDroite2, _),
    evaluerColonne(X,Y,CouleurJoueur,ColonneGauche1, ColonneGauche2, _, ColonneDroite1, ColonneDroite2, _),
    evaluerDiag1(X,Y,CouleurJoueur,Diag1Gauche1, Diag1Gauche2, _, Diag1Droite1, Diag1Droite2, _),
    evaluerDiag2(X,Y,CouleurJoueur,Diag2Gauche1, Diag2Gauche2, _, Diag2Droite1, Diag2Droite2, _),
    SeptBon1 = 1 + LigneGauche1 + LigneGauche2 + Diag1Gauche1 + Diag1Gauche2,
    piege7BonPositionnement(SeptBon1, ScoreBon1),
    SeptBon2 = 1 + LigneDroite1 + LigneDroite2 + Diag2Droite1 + Diag2Droite2,
    piege7BonPositionnement(SeptBon2, ScoreBon2),
    SeptBas1 = 1 + LigneDroite1 + LigneDroite2 + Diag1Droite1 + Diag1Droite2,
    piege7BasPositionnement(SeptBas1, ScoreBas1),
    SeptBas2 = 1 + LigneGauche1 + LigneGauche2 + Diag2Gauche1 + Diag2Gauche2,
    piege7BasPositionnement(SeptBas2, ScoreBas2),
    SeptMoyen1 = 1 + ColonneDroite1 + ColonneDroite2 + Diag2Gauche1 + Diag1Gauche2,
    piege7MoyenPositionnement(SeptMoyen1, ScoreMoyen1),
    SeptMoyen2 = 1 + ColonneDroite1 + ColonneDroite2 + Diag1Droite1 + Diag1Droite2,
    piege7MoyenPositionnement(SeptMoyen2, ScoreMoyen2),
    SeptMoyen3 = 1 + ColonneGauche1 + ColonneGauche2 + Diag2Droite1 + Diag2Droite2,
    piege7MoyenPositionnement(SeptMoyen3, ScoreMoyen3),
    SeptMoyen4 = 1 + ColonneGauche1 + ColonneGauche2 + Diag1Gauche1 + Diag1Gauche2,
    piege7MoyenPositionnement(SeptMoyen4, ScoreMoyen4),
    Score is ScoreBon1 + ScoreBon2 + ScoreBas1 
            + ScoreBas2 + ScoreMoyen1 + ScoreMoyen2
            + ScoreMoyen3 + ScoreMoyen4.


%% Scores associés à chaque 7 :
% Les bons 7 :
/*
    ***  ***
     *    *
    *      *
*/
piege7BonPositionnement(5, 250).
piege7BonPositionnement(4,125).   
piege7BonPositionnement(_, 0). 
% Les moyens 7 :
/*
    *  *  *        *   *  *
    * *   * *    * *    * * 
    *     *  *  *  *      *
*/
piege7MoyenPositionnement(5,150).
piege7MoyenPositionnement(4,75).
piege7MoyenPositionnement(_,0).
% Les mauvais 7 :
/*
    *      *
     *    *
    ***  ***
*/
piege7BasPositionnement(5,100).
piege7BasPositionnement(4,50).
piege7BasPositionnement(_,0).


/****************************************************************
                    HEURISTIQUES D'EVALUATION 
                    DE L'OPENING

Evalue la qualité du premier cout de chaque joueur.
Le puissance 4 étant un jeu résolu, il a été montré que pour les joueurs
n'effectuants aucune erreur le premier pion permettait de déterminer directement 
l'issue de la partie.
Sources : https://connect4.gamesolver.org/fr/ et https://www.youtube.com/watch?v=yDWPi1pZ0Po

*****************************************************************/


% opening(+CouleurJoueur, -ScoreOpening,+ PoidsOpening)
% Donne une note d'autant plus forte que l'opening du joueur courant est bon 
% par-rapport à celui de son adversaire
opening(CouleurJoueur, ScoreOpening, PoidsOpening) :- 
    PoidsOpening > 0,
    couleurAdverse(CouleurJoueur, JoueurAdverse),
    caseTest(X,1,CouleurJoueur), 
    caseTest(X2,1,JoueurAdverse),
    evaluerOpening(X, CouleurJoueur, ScoreJoueur),
    evaluerOpening(X2, JoueurAdverse, ScoreJoueurAdverse),
    ScoreOpening is ScoreJoueur - ScoreJoueurAdverse,! .
opening(_, 0, _).    

% Le rouge est le premier à joueur.
% S'il joue parfaitement et son adversaire aussi,
% son premier coup dans la colonne 4 lui assure la victoire
% la colonne 3 et 5 le nul et le reste la défaite
evaluerOpening(X, rouge, Score):- 
    (X == 1, Score is -1000, !;
     X == 2, Score is -1000, !;
     X == 3, Score is 0, !;
     X == 4, Score is 1000, !;
     X == 5, Score is 0, !;
     X == 6, Score is -1000, !;
     X == 7, Score is -1000, !;   
        Score is 0).

% Le jaune est le second à joueur.
% S'il joue parfaitement et son adversaire aussi,
% les colonnes 3,4 et 5 permettent d'amortir les dégats
% le reste lui assure la défaite
evaluerOpening(X, jaune, Score):- 
    (X == 1, Score is -1000, !;
     X == 2, Score is -1000, !;
     X == 3, Score is 500, !;
     X == 4, Score is 1000, !;
     X == 5, Score is 500, !;
     X == 6, Score is -1000, !;
     X == 7, Score is -1000, !;   
        Score is 0).

/****************************************************************
                    HEURISTIQUES D'EVALUATION 
                    DE L'ADJACENCE

Evalue le nombre de possibilité disponible sur une ligne pour un joueur 
Cela peremt comme le piège du 7 de mettre en place un situation ou le joueur 
puet gagner à 2 endroits différents
Le score donnée à l'adjacence est d'autant plus important que les colonnes libres disponibles
sont de part et d'autre d'un alignement de pions important.
Source utilisée : https://www.researchgate.net/publication/331552609_Research_on_Different_Heuristics_for_Minimax_Algorithm_Insight_from_Connect-4_Game

*****************************************************************/

% piegeAdjacence(+CouleurJoueur, -ScoreAdjacence, +PoidsAdjacence)
% Donne une note d'autant plus forte les 2 colonnes autour d'un alignement de pions 
% du joueur son libre alors que celles de son adversaire sont occupée.
piegeAdjacence(CouleurJoueur, ScoreAdjacence, PoidsAdjacence) :- 
    PoidsAdjacence > 0,
    couleurAdverse(CouleurJoueur, JoueurAdverse),
    findall(S, evaluerAdjacence(CouleurJoueur, S),ScoreJoueur),
    sum(ScoreJoueur, ScoresJoueur),
    findall(S, evaluerAdjacence(JoueurAdverse, S),ScoreAdverse),
    sum(ScoreAdverse, ScoresAdverse),
    ScoreAdjacence is (ScoresJoueur - (2 * ScoresAdverse)).
piegeAdjacence(_, 0, _).  

% evaluerAdjacence/2(+CouleurJoueur,-ScoreJoueur)
% Donne le score d'évaluation d'adjacence d'un pion 
evaluerAdjacence(CouleurJoueur, ScoreJoueur):-
    caseTest(X,Y,CouleurJoueur),
    calculerScoreAlignement(X, Y, CouleurJoueur, S),
    ScoreJoueur is S.  
  
% calculerScoreAjacence/2(+X,+Y,+CouleurJoueur,-Score)
% Calcul le score d'alignement d'un pion sur une ligne
calculerScoreAjacence(X,Y, CouleurJoueur, Score ):-
    evaluerLigne(X,Y,CouleurJoueur,LigneGauche1, LigneGauche2, LigneGauche3, LigneDroite1, LigneDroite2, LigneDroite3),
    adjacence3(LigneGauche1, LigneGauche2, LigneGauche3, LigneDroite1, LigneDroite2, LigneDroite3, Score1),
    adjacence2(LigneGauche1, LigneGauche2, LigneDroite1, LigneDroite2, Score2),
    adjacence1(LigneGauche1, LigneDroite1, Score3),
    Score is Score1 + Score2 + Score3.


/******************************************/
%         Liste des configurations         %
%         qui augmentent le score :        %
%le pion étudié peut être n'importe quel x %
/******************************************/
%Configuration avec adjacence et alignement de 3 pions
%_,x,x,x,_%
adjacence3(Gauche1, Gauche2, Gauche3, Droite1, Droite2, Droite3, Score):-
    (  
        Droite1==1, Droite2==1, Droite3==0, Gauche1==0, Score is 1000;
        Droite1==1, Droite2==0, Gauche1==1, Gauche2==0, Score is 1000;
        Gauche1==1, Gauche2==1, Gauche3==0, Droite1==0, Score is 1000);
    (Score is 0). 

%Configuration avec adjacence et alignement de 2 pions
%_,x,x,_%
adjacence2(Gauche1, Gauche2 , Droite1, Droite2 , Score):-
    (  
        Droite1==1, Droite2==0, Gauche1==0, Score is 120;
        Gauche1==0, Droite1==0, Gauche2==0, Score is 120
        );
    (Score is 0).     

%Configuration avec adjacence et alignement d'un pion
%_,x,_%
adjacence1(Gauche1, Droite1, Score):-
    (  
        Droite1==0, Gauche1==0, Score is 60
        );
    (Score is 0).  