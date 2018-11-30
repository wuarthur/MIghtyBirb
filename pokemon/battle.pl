%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calculate type multiplier against a pokemon
% each pokemon has 1 or 2 types, which have a different multiplier value against a type
% total value is the product of the multiplier values

calculate_multiplier(Att_move_idx, Def_pokemon_Idx, Multiplier):-
  move(Att_move_idx, 'type', Att_type),
  findall(S, pokemon(Def_pokemon_Idx, 'type', S), Def_types),
  maplist(map_multiplier(Att_type), Def_types, M),
  flatten(M, Multipliers),
  product_list(Multipliers, Multiplier).

map_multiplier(Att_type, Def_type, Multiplier):-
  attack(Att_type, Def_type, Multiplier).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% STAB -- 1.25 if there is a type match, 1 otherwise
calculate_stab(Att_move_idx, Att_pokemon_idx,1.25):-
  type_match(Att_move_idx, Att_pokemon_idx).
calculate_stab(Att_move_idx, Att_pokemon_idx,1):-
  \+ type_match(Att_move_idx, Att_pokemon_idx).

% check if the attack move type matches a pokemon move type
type_match(Att_move_idx, Att_pokemon_idx):-
  move(Att_move_idx, 'type', Att_type),
  pokemon(Att_pokemon_idx, 'type', Att_type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attack_category('Physical', 'att').
attack_category('Special', 'sp_att').
defense_category('Physical', 'def').
defense_category('Special', 'sp_def').

% get attack value
calculate_attack(Att_move_idx, Att_pokemon_idx, Attack):-
  move(Att_move_idx ,'category', Category),
  attack_category(Category, C),
  pokemon(Att_pokemon_idx, C, Attack).

% get defense value
calculate_defense(Att_move_idx, Def_pokemon_idx, Defense):-
  move(Att_move_idx ,'category', Category),
  defense_category(Category, C),
  pokemon(Def_pokemon_idx, C, Defense).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get actual damage value (exact amount of hp the defending pokemon will lose)
calculate_damage(Attack,Defense,Stab,Multiplier,Value) :-
  B is Attack/Defense,
  C is B+2,
  D is C * Stab * Multiplier,
  Value is ceiling(D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate_att(Att_pokemon_idx, Def_pokemon_idx, Att_move_idx, Damage):-
  calculate_attack(Att_move_idx, Att_pokemon_idx, Attack),
  calculate_defense(Att_move_idx, Def_pokemon_idx, Defense),
  calculate_stab(Att_move_idx, Att_pokemon_idx, Stab),
  calculate_multiplier(Att_move_idx, Def_pokemon_idx, Multiplier),
  calculate_damage(Attack,Defense,Stab,Multiplier,Damage).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Pick a best move for active pokemon

best_move(Att_idx, Def_idx, Move_indices, Best_move):-
  maplist(calculate_att(Att_idx, Def_idx), Move_indices, Damage_pts),
  zip(Damage_pts, Move_indices, Zipped),
  sort_zipped(Zipped, Sorted),
  reverse(Sorted, Reversed),
  Reversed = [Fst|_],
  Fst = [_, Best_move].







pvp(Npc, AttackerId, DefenderId):-
  active_pokemon(AttackerId, 'pokemon_idx', ANo),
  active_pokemon(DefenderId, 'pokemon_idx', DNo),
  getName(ANo, AName),
  active_pokemon(DefenderId, 'hp', DHp),
  getMove(AttackerId, AMove),
  get_move_name(AMove, AMoveName),
  nl(),append_opponents(Npc), write(AName), write(' used '), write(AMoveName),
  calculate_att(ANo, DNo, AMove, Damage),
  Value is DHp - Damage,
  check_hp(Value, Npc, DefenderId, AttackerId, Damage).


check_hp(Value, Npc, DefenderId, AttackerId, _):-
  Value < 0,
  %next turn
  not_npc(Npc,NotNpc),
  fainted(NotNpc, DefenderId, AttackerId).

check_hp(Value, Npc, DefenderId, AttackerId, Damage):-
  Value >= 0,
  active_pokemon(DefenderId, 'pokemon_idx', DNo),
  active_pokemon(DefenderId, 'hp', DHp),
  getName(DNo, DName),
  %next turn
  not_npc(Npc,NotNpc),
  nl(), append_opponents(NotNpc), write(DName), write(' has '), write(DHp), write(' hp left'),
  update_stat(DefenderId, hp, Damage),
  pvp(NotNpc, DefenderId, AttackerId).


% Npc is true if fainted pokemon belongs to npc, false if it belongs to you
append_opponents(true):-
  write('\t\t\tThe opponent\'s ').
append_opponents(false).

not_npc(false,true).
not_npc(true,false).

fainted(Npc, FaintedPokemonId, AlivePokemonId):-
  nl(),
  append_opponents(Npc),
  active_pokemon(FaintedPokemonId, 'pokemon_idx', FaintedPokemonPokeNo),
  getName(FaintedPokemonPokeNo, FaintedPokemonName),
  write(FaintedPokemonName),
  write(' has fainted.'),
  retractall(active_pokemon(FaintedPokemonId,_,_)),
  findall(I,active_pokemon(I,npc,Npc),Ids),
  print(Ids),
  length(Ids, L),
  check_for_remaining_pokemon(L, AlivePokemonId, Npc).

check_for_remaining_pokemon(0, _, Npc):-
  i_won(Npc).
check_for_remaining_pokemon(N, AlivePokemonId, Npc):-
  N > 0,
  active_pokemon(ReplacementPokemonId, npc, Npc),
  forall(active_pokemon(Value2, npc, Npc),(ReplacementPokemonId>Value2->fail;true)),
  trigger_next_match(Npc, ReplacementPokemonId, AlivePokemonId).

trigger_next_match(false, ReplacementPokemonId, AlivePokemonId):-
  active_pokemon(ReplacementPokemonId, 'pokemon_idx', PokeNo),
  getName(PokeNo, PokeName),
  nl(),nl(),write('Go! '), write(PokeName),
  pvp(true, AlivePokemonId, ReplacementPokemonId).
trigger_next_match(true, ReplacementPokemonId, AlivePokemonId):-
  active_pokemon(ReplacementPokemonId, 'pokemon_idx', PokeNo),
  getName(PokeNo, PokeName),
  nl(),nl(),write('\t\t\tThe opponent sent out '), write(PokeName),
  pvp(false, AlivePokemonId, ReplacementPokemonId).

i_won(false):-
  nl(), write('\t\t\tOpponent has won').
i_won(true):-
  nl(), write('I won').

get_move_name(Id, Name):-
  move(Id, move, Name).

getMove(Id, Move):-
  random_move(Id, Move).

battleTilDeath:-
  active_pokemon(MyPokeID,npc,false),
  active_pokemon(EnemyPokeID,npc,true),
  active_pokemon(MyPokeID, 'pokemon_idx', MyPokeNo),
  active_pokemon(EnemyPokeID, 'pokemon_idx', FoePokeNo),
  getName(MyPokeNo, MyPokeName),
  getName(FoePokeNo, FoeName),
  nl(),nl(),write('\t\t\tThe opponent sent out '), write(FoeName),
  nl(),write('Go! '), write(MyPokeName),
  nl(),
  pvp(false, MyPokeID, EnemyPokeID).
