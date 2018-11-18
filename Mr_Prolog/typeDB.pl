:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module(pokemon).

import:-
    csv_read_file('pokemon.csv', Rows, [functor(ptypes), arity(12)]), 
    maplist(assert, Rows).



isType(Type, Name):-
    pokemon:pokeTypes(Num,Name,Type1,Type,HP,Attack,Defense,SpAtk,SpDef,Speed,Generation,Legendary).
isType(Type, Name):-
    pokemon:pokeTypes(Num,Name,Type,Type2,HP,Attack,Defense,SpAtk,SpDef,Speed,Generation,Legendary).




%pokemon:pokeTypes(_,X,"Fire",_,_,_,_,_,_,_,_,_).

