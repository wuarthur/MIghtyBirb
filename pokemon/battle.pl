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
  A is Attack/Defense,
  B is A/50,
  C is B+2,
  D is C * Stab * Multiplier,
  E is floor(D),
  ret_att(E, Value).

% don't let Value be less than 1.
ret_att(0, 1).
ret_att(Value, Value) :-
  Value > 0.

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
% NOTE: @yang yang i think this is used during the battle

best_move(Att_idx, Def_idx, Move_indices, Best_move):-
  maplist(calculate_att(Att_idx, Def_idx), Move_indices, Damage_pts),
  zip(Damage_pts, Move_indices, Zipped),
  sort_zipped(Zipped, Sorted),
  reverse(Sorted, Reversed),
  Reversed = [Fst|_],
  Fst = [_, Best_move].





%%%%%Weeeeeee
% @yang yang, the pokemon with the higher speed goes first
% random if the speed is the same



%Id1: pokemon1 id
%Hp1: pokemon1's hp when fight begun
%Id2: other pokemon2 id
%Hp2: pokemon2's hp when fight begun
%Id3: winning pokemon id
%Hp3: remainging hp after the fight
% fight(Id1, Hp1 ,Id2, Hp2):-
%     active_pokemon(Id1,'pokemon_idx',Index1), getName(Index1, Name1),
%     active_pokemon(Id2,'pokemon_idx',Index2), getName(Index2, Name2),
%     Hp2 < 0 ->   print(Name2),print('1 won with: '), print(Hp1), print(' HP'), nl(),Id3 is Id1,Hp3 is Hp1,V is 1, !;
%     Hp2 == 0 ->  print(Name2), print('1 won with: '), print(Hp1), print(' HP'),nl(),Id3 is Id1,Hp3 is Hp1,V is 1, !;
%     getMove(Id2, Move2),
%     getMove(Id1, Move1),
%     active_pokemon(Id1,'pokemon_idx',Index),
%     active_pokemon(Id2,'pokemon_idx',Index2),
%     calculate_att(Index, Index2, Move2, Dmg1),
%     calculate_att(Index2, Index, Move1, Dmg2),
%     New1 is Hp1 - 10,
%     New2 is Hp2 - 20,
%     % print(Hp1),
%     % print(":"),
%     % print(New1),
%     % nl(),
%     New1 > 0 -> fight(Id1, New1 ,Id2, New2, Id3, Hp3 ,V );
%     print(Name),
%     print(' won with: '),print(Hp2), print(' HP'),nl(),Id3 is Id2,Hp3 is Hp2,V is 2.

%print("recur1"), nl(), fight('f', 0 ,Id2, Hp2, Id3, Hp3, V);
%pokemon 1v1
%Requires:
%Active_pokemon ids
%Pokemon indexes
%returns:
%Active_pokemon id of winner

pvp(MyId, FoeId):-
  active_pokemon(MyId, 'pokemon_idx', MyPokeNo),
  active_pokemon(FoeId, 'pokemon_idx', FoePokeNo),
  pokemon(MyPokeNo, speed, MySpeed),
  pokemon(FoePokeNo, speed, FoeSpeed),
  nl(),print('In pvp'),nl(),
  MySpeed >= FoeSpeed ->
    print('I am faster'),
    do_attack(false, MyPokeNo, FoePokeNo);
  MySpeed < FoeSpeed ->
    print('They are faster'),
    do_attack(true, FoePokeNo, MyPokeNo)
  .

do_attack(Npc, AttackerId, DefenderId):-
  active_pokemon(AttackerId, 'pokemon_idx', ANo),
  active_pokemon(DefenderId, 'pokemon_idx', DNo),
  getName(ANo, AName),
  getName(DNo, DName),
  active_pokemon(DefenderId, 'hp', DHp),
  getMove(AttackerId, AMove),
  get_move_name(AMove, AMoveName),
  print(AMoveName),
  nl(), write(AName), write(' used '), write(AMoveName),

  calculate_att(ANo, DNo, AMove, Damage),
  Damage >= DHp ->
    fainted(Npc, DefenderId, AttackerId);
  Damage < DHp ->
    nl(),
    Npc == true -> Next = false, write('The opponet\'s ');
    Npc == false -> Next = true;
    write(DName), write(' took '), write(Damage), write(' damage'),
    update_stat(DefenderId, hp, Damage),
    do_attack(Next, DefenderId, AttackerId)
  .

% Npc is true if fainted pokemon belongs to npc, false if it belongs to you
fainted(Npc, FaintedPokemonId, AlivePokemonId):-
  nl(),
    Npc == true ->
      write('The opponent\'s '),
      active_pokemon(FaintedPokemonId, 'pokemon_idx', FaintedPokemonPokeNo),
      getName(FaintedPokemonPokeNo, FaintedPokemonName),
      write(FaintedPokemonName);
    Npc == false ->
      active_pokemon(AlivePokemonId, 'pokemon_idx', AlivePokeNo),
      getName(AlivePokeNo, AliveName),
      write(AliveName);
  write(' has fainted.'),

  retractall(active_pokemon(FaintedPokemonId,_,_)),

  findall(I,active_pokemon(I,npc,Npc),Ids),
  Ids == [] -> opponent_won(Npc);
  active_pokemon(ReplacementPokemonId, npc, Npc),
  active_pokemon(ReplacementPokemonId, 'pokemon_idx', ReplacementPokeNo), getName(ReplacementPokeNo, ReplacementName),
  nl(),
  Npc == true -> write('The opponent sent out ');
  Npc == false -> write('Go! ');
  write(ReplacementName),

  Npc == true -> pvp(AlivePokemonId, ReplacementPokemonId);
  Npc == false -> pvp(ReplacementPokemonId, AlivePokemonId)
  .


opponent_won(Npc):-
  nl(),
  Npc == true -> write('Opponent ');
  Npc == false -> write('I ');
  write(won).

get_move_name(Id, Name):-
  move(Id, move, Name).

getMove(Id, Move):-
  random_move(Id, Move).

battleTilDeath:-
  active_pokemon(MyPokeID,npc,false),
  active_pokemon(MyPokeID,pokemon_idx,MyPokeNo),
  active_pokemon(EnemyPokeID,npc,true),
  active_pokemon(EnemyPokeID,pokemon_idx,EnemyPokeNo),
  getName(MyPokeNo, MyName),
  getName(EnemyPokeNo, EnemyName),
  write('Foe sent out '), write(EnemyName),
  nl(),
  write('Go! '), write(MyName),
  pvp(MyPokeID, EnemyPokeID).
  %battleTilDeath(Z).
