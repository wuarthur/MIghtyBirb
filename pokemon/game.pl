:- [utils].
:- [loader2].
:- [selectpokemon].
:- [battle].



%%%%%%%%% Easier to update KB if we use same 'active_pokemons' for both.
%%%%%%% We probably gonna use Idx for active pokemons, and update their health via DB as in battle.pl
%%%%%%% less "INNTER LEFT JOIN" that way.


active_pokemons('rival', []).
active_pokemons('mine', []).



%%%%%%%%% TODO next -- Pokemon team picking
