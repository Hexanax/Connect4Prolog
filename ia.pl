%%%%%%%%%%%% ia.pl %%%%%%%%%%%%
:- module(ia, [iaAleatoire/1]).

%%%%%%%%%%%%%%%%%%%%%%%
%% Pr�dicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

iaAleatoire(Coup) :- nbColonnes(NBCOLONNES), Coup is random(NBCOLONNES)+1.
