%%%%%%%%%%% Picks default moves set for pokemon, 4 is max I think.
% TODO: can this handle pokemon with movesets smaller than 4?
% Ex: 132 has a moveset of 1

default_moves(Idx, Move_indices):-
  findall(M, pokemon(Idx, 'move', M), Moves),
  random_permutation(Moves, M),
  take(4, M, Move_indices).


% Pick a random move for active pokemon

random_move(Id, Move):-
  active_pokemon(Id, 'moves', Move_indices),
  random_permutation(Move_indices, Moves),
  Moves = [Move|_ ].
