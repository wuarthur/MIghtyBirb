
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

calculate_multiplier(Att_move_idx, Def_pokemon_Idx, Multiplier):-
  move(Att_move_idx, 'type', Att_type),
  findall(S, pokemon(Def_pokemon_Idx, 'type', S), Def_types),
  maplist(map_multiplier(Att_type), Def_types, M),
  flatten(M, Multipliers),
  product_list(Multipliers, Multiplier).

map_multiplier(Att_type, Def_type, Multiplier):-
  attack(Att_type, Def_type, Multiplier).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calculate STAB value to either 1 (no STAB) or 1.25 (STABBY)

calculate_stab(Att_move_idx, Att_pokemon_idx, Multiplier):-
  move(Att_move_idx, 'type', Att_type),
  findall(S, pokemon(Att_pokemon_idx, 'type', S), Att_types),
  count(Att_type, Att_types, C),
  apply_stab(C, Multiplier).

apply_stab(0, 1).
apply_stab(C, 1.25):-
  C > 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

attack_category('Physical', 'att').
attack_category('Special', 'sp_att').
defense_category('Physical', 'def').
defense_category('Special', 'sp_def').

calculate_attack(Att_move_idx, Att_pokemon_idx, Attack):-
  move(Att_move_idx ,'category', Category),
  attack_category(Category, C),
  pokemon(Att_pokemon_idx, C, Attack).

calculate_defense(Att_move_idx, Def_pokemon_idx, Defense):-
  move(Att_move_idx ,'category', Category),
  defense_category(Category, C),
  pokemon(Def_pokemon_idx, C, Defense).


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

% don't let Value be less than 1.
ret_att(0, 1).
ret_att(Value, Value) :-
  Value > 0.

calculate_damage(Attack,Defense,Stab,Multiplier,Value) :-
  A is Attack/Defense,
  B is A/50,
  C is B+2,
  D is C * Stab * Multiplier,
  E is floor(D),
  ret_att(E, Value).


calculate_att(Att_pokemon_idx, Def_pokemon_idx, Att_move_idx, Damage):-
  calculate_attack(Att_move_idx, Att_pokemon_idx, Attack),
  calculate_defense(Att_move_idx, Def_pokemon_idx, Defense),
  calculate_stab(Att_move_idx, Att_pokemon_idx, Stab),
  calculate_multiplier(Att_move_idx, Def_pokemon_idx, Multiplier),
  calculate_damage(Attack,Defense,Stab,Multiplier,Damage).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Pick a best move for active pokemon

best_move(Att_idx, Def_idx, Move_indices, Best_move):-
  maplist(calculate_att(Att_idx, Def_idx), Move_indices, Damage_pts),
  zip(Damage_pts, Move_indices, Zipped),
  sort_zipped(Zipped, Sorted),
  reverse(Sorted, Reversed),
  Reversed = [Fst|_],
  Fst = [_, Best_move].





%%%%%Weeeeeee

pokemonState(Id, Hp).



fight('f', _ ,Id2, Hp2, Id3, Hp3):-
  print('2 won'),
  Id3 is Id2,
  Hp3 is Hp2.

fight(Id1, Hp1 ,Id2, 0, Id3, Hp3):-
  print('1 won'),
  Id3 is Id1,
  Hp3 is Hp1.
%Id1: pokemon1 id 
%Hp1: pokemon1's hp when fight begun
%Id2: other pokemon2 id 
%Hp2: pokemon2's hp when fight begun
%Id3: winning pokemon id 
%Hp3: remainging hp after the fight
fight(Id1, Hp1 ,Id2, Hp2, Id3, Hp3):-
  Hp2>0->
    %todo reduce health base on move used instead of just 20
    New1 is Hp1 - 20,
    New1 > 0 -> 
      %todo reduce health base on move used instead of just 20
      New2 is Hp2 - 20,
      fight(Id1, New1 ,Id2, New2, Id3, Hp3);
  fight('f', _ ,Id2, Hp2, Id3, Hp3);
  fight(Id1, Hp1 ,_, 0, Id3, Hp3).

