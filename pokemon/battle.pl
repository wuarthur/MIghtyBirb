
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%% Takes into account pokemons with only 1 type
/*
  NOTE:
  damage_multiplier would have to be changed so that the att_idx does not reflect a pokemon, but
  a specific type.
  so att_ind should be a string of one of the types

  i think this function is a bit overly complicated
  it should simply be:
  1) look up the types of the defending pokemon
  2) look in the kb what the multiplier is versus the attacking type
  3) multiply the values together
*/

damage_multiplier(Att_Idx, Def_Idx, Multiplier):-
  findall([S], pokemon(Att_Idx, 'type', S), A),
  findall([S], pokemon(Def_Idx, 'type', S), D),
  flatten(A, Att_types),
  flatten(D, Def_types),
  maplist(map_multipliers(Def_types), Att_types, M),
  flatten(M, Multipliers),
  product_list(Multipliers, Multiplier).

map_multipliers(Def_types, Att_type, Multipliers):-
  maplist(map_multiplier(Att_type), Def_types, Multipliers).

map_multiplier(Att_type, Def_type, Multiplier):-
  attack(Att_type, Def_type, Multiplier).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%% TODO: -- I have no idea if this is right, never played pokemon
%%%%%%%% Please tell me im not out of touch with reality and this is right
/*
  the damage depends on the move used
  so it must use calculate_move


  calculate_att(AttackingPokemon, DefendingPokemon, Move, Damage):-

  1) get the category of the move
    if it is special:
      use the defendingPokemon's special defense value
      use the attackingPokemon's special attack value
    if it is physical:
      use the defendingPokemon's defense value
      use the attackingPokemon's attack value
    (we are going to ignore status moves)
  2) get the type of the move
  3) if the type of the move is the same as one of the types of the attacking pokemon, STAB is true
      -- STAB means to multiply the damage by 1.25
      -- i dont know if it should live in a separate function or in here. this is the only place
         STAB is used
  4) get the damage_multiplier value using the move type and defending pokemon
  5) the amount of damage the defending pokemon receives is: (simplified version of real formula)
      movePower taken from kb *
      attacking pokemon specialphysical attack value / defending pokemon specialphysical defense value

      ^all of that /50
      then +2
      then * damage_multiplier * STAB
  6) take the floor of damage. if it's less than 1, return 1
*/

calculate_att(Att_Idx, Def_Idx, Att_type, Damage):-
  defense_type(Att_type, Def_Type),
  pokemon(Att_Idx, Att_type, Att_value),
  pokemon(Def_Idx, Def_Type, Def_value),
  Raw_damage is Att_value - Def_value,
  damage_multiplier(Att_Idx, Def_Idx, Multiplier),
  Damage is Raw_damage * Multiplier.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%% TODO: --  Haha, not even gonna attempt this.
/*
  this is not needed
*/

calculate_move(Att_Idx, Def_Idx, Move, Damage).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%% Update KB to reflect change in pokemon status.
/*
  this is only the change in hp right?
*/

update_stat(Idx, Stat, Diff):-
  pokemon(Idx, Stat, Old_value),
  New_value is Old_value + Diff,
  retract(pokemon(Idx, Stat, _)),
  assertz(pokemon(Idx, Stat, New_value)).
