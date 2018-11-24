
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%% Takes into account pokemons with only 1 type

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

calculate_move(Att_Idx, Def_Idx, Move, Damage).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%% Update KB to reflect change in pokemon status.

update_stat(Idx, Stat, Diff):-
  pokemon(Idx, Stat, Old_value),
  New_value is Old_value + Diff,
  retract(pokemon(Idx, Stat, _)),
  assertz(pokemon(Idx, Stat, New_value)).