%%%%%%%%%%%% ia.pl %%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%
%% Pr�dicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

iaAleatoire(Coup) :- nbColonnes(NBCOLONNES), random_between(1,NBCOLONNES,Coup).