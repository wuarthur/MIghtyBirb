:- use_module(library(apply)).
:- use_module(library(csv)).
:- dynamic pokeDex/3. pokeDex(num,col, value).

import:-load().
load():-
  get_pokedex().

% general csv parser
get_rows_data(File, Rows):-
  csv_read_file(File, Rows),
  maplist(assert, Rows).

get_pokedex() :-
  get_rows_data("./csvs/pokedex.csv", [_,H|T]),
  add_pokeDex_to_db(H,T).

add_pokeDex_to_db(row(Num,Name,Type1,Type2,Total,HP,Attack,Defense,SpAtk,SpDef,Speed,Gen,Legendary),[H|T]):-
  assert(pokeDex(Num,'Name', Name)),
  assert(pokeDex(Num,'Type1', Type1)),
  assert(pokeDex(Num,'Type2', Type2)),
  assert(pokeDex(Num,'Total', Total)),
  assert(pokeDex(Num,'HP', HP)),
  assert(pokeDex(Num,'Attack', Attack)),
  assert(pokeDex(Num,'Defense', Defense)),
  assert(pokeDex(Num,'SpAtk', SpAtk)),
  assert(pokeDex(Num,'SpDef', SpDef)),
  assert(pokeDex(Num,'Speed', Speed)),
  assert(pokeDex(Num,'Generation', Gen)),
  assert(pokeDex(Num,'Legendary', Legendary)),
  add_pokeDex_to_db(H,T).
