%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- [game].

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
fight(Id1, Hp1 ,Id2, Hp2, Id3, Hp3, V):-
    active_pokemon(Id1,'pokemon_idx',Index2), getName(Index2, Name2), 
    active_pokemon(Id2,'pokemon_idx',Index), getName(Index, Name), 
    Hp2 < 0 ->   print(Name2),print('1 won with: '), print(Hp1), print(' HP'), nl(),Id3 is Id1,Hp3 is Hp1,V is 1, !;
    Hp2 == 0 ->  print(Name2), print('1 won with: '), print(Hp1), print(' HP'),nl(),Id3 is Id1,Hp3 is Hp1,V is 1, !;
    getMove(Id2, Move2),
    move(Move2, 'move', Move2_name),
    getMove(Id1, Move1),
    move(Move1, 'move', Move1_name),
    active_pokemon(Id1,'pokemon_idx',Index),
    active_pokemon(Id2,'pokemon_idx',Index2),
    calculate_att(Index, Index2, Move2, Dmg1),
    calculate_att(Index2, Index, Move1, Dmg2),
    New1 is Hp1 - Dmg1,
    print(Name), print(' used '), print(Move1_name), print(' it did '), print(Dmg1), print(' Damage'),nl(),
    New2 is Hp2 - Dmg2,
    print(Name2), print(' used '), print(Move2_name), print(' it did '), print(Dmg2), print(' Damage'), nl(),
    % print(Hp1),
    % print(":"),
    % print(New1),
    % nl(),
    New1 > 0 -> fight(Id1, New1 ,Id2, New2, Id3, Hp3 ,V );
    print(Name),
    print(' won with: '),print(Hp2), print(' HP'),nl(),Id3 is Id2,Hp3 is Hp2,V is 2.
    
%print("recur1"), nl(), fight('f', 0 ,Id2, Hp2, Id3, Hp3, V);
%pokemon 1v1
%Requires:
%Active_pokemon ids
%Pokemon indexes
%returns:
%Active_pokemon id of winner

pvp(Id1, Id2, Winner):-
  active_pokemon(Id1, 'hp', HP1),
  active_pokemon(Id2, 'hp', HP2),
  fight(Id1, HP1, Id2, HP2, Wonner, HP, Won),
  pokemon(Wonner, 'name', Name),
  Won == 2 -> Diff is HP2 - HP, retract(active_pokemon(Id1,_,_)), update_stat(Id2, 'hp',Diff),Winner = Id2;
  retract(  (Id2,_,_)),
  Diff is HP1 - HP,
  update_stat(Id1, 'hp',Diff),
  Winner = Id1.

getMove(Id, Move):-
  %pokemon(Pid, 'move', Move).
  random_move(Id, Move).

battleTilDeath(Z):-
  findall(I,active_pokemon(I,npc,false),MyIDs),
  findall(I,active_pokemon(I,npc,true),EnemyIDs),
  % print(MyIDs),
  % print(":"),
  % print(EnemyIDs),
  MyIDs == [] -> Z = 'Enemy';
  EnemyIDs == [] -> Z = 'Me';
  nth0(0, MyIDs, MyPokeID),
  nth0(0, EnemyIDs, EnemyPokeID),
  pvp(MyPokeID, EnemyPokeID, Winner),
  % print("yes:"),
  % print(Winner),
  % nl(),

  findall(I,active_pokemon(I,npc,false),MyIDs),
  findall(I,active_pokemon(I,npc,true),EnemyIDs),
  % print(MyIDs),
  % print(":"),
  % print(EnemyIDs),
  MyIDs == [] -> Z = 'Enemy';
  EnemyIDs == [] -> Z = 'Me';
  battleTilDeath(Z).
