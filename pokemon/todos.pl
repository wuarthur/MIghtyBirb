% TODO: generate KB so that we only get the csv files once
      % pokedex takes 3 params (pokemon name, stat name, stat value)
      % types takes 3 params (attack type, def type, value)
      % moves takes 3 params (name, stat name, stat value)
      % movesets takes 2 params (pokemon name, move name)
% TODO: user IO interface (Probably from command line. see Poole's geography.pl)
      % just make the basic frame for now, we will come up with the questions and possible interactions later
% TODO: return a pokemon that takes X damage from an type or less
      % ex. given a type, fire, and damage, 1, return a pokemon that takes less than 1 damage from fire
      damageMultiplier(DefendingPokemon, AttackingType, MultiplierBetween0and4)
% TODO: get pokemon with highest total base stat
      % literally add all of the stats together and return highest
      highestTotalStat(Pokemon, Value)
% TODO: get pokemon with highest stat1 > stat2 > stat3... (where stats = hp/atk/def/spatk/spdef/spd)
      % make a general formula that takes an order in an array ex. ['hp','speed',A,B,C,D]
      % then returns the pokemon from the list that has the highest hp, then if there is a tie, highest speed etc
      highestStat(Pokemon, StatName, Value)
% TODO: specify the stat strategy (ex. 2 atk, 2 sp atk, 1 hp, 1 spd)
      % given a list of strats (so the above would be ['atk','atk','spatk','spatk','hp','spd'])
      % return a pokemon team of 6
      statStrategy(Stat1, Stat2, Stat3, Stat4, Stat5, Stat6)
% TODO: specify generation, takes in a number (for the generation)
      % return a pokemon with a matching gen number
      % we will specify gen 0 as all generations (no filtering)
      generation(Pokemon, GenNumber, Boolean)
% TODO: no legendaries + mythical pokemon
      % check if pokemon is not legendary
      legendary(Pokemon, Boolean)
% TODO: rival teams (random, following strategy, hard coded -- ex. champion teams)
      % start with hard coded ones first
      % we will define the teams (probably in a csv or something)
      % then do random
      % randomly generate 6 pokemon. (allow for repeats???)
      % then do strategy
      % get the best 6 pokemon. start with 1, then check for type coverage (which weakenesses does it have), then do the 2nd one to cover those weakenesses
      % (above formula is subject to change, idk the best way to generate pokemon teams)
      rivalTeam(Pokemon1, Pokemon2, Pokemon3, Pokemon4, Pokemon5, Pokemon6)
% TODO: find best match up for pokemon
      % based on types. so we will find a counter pokemon for each rival pokemon
      myTeam(Pokemon1, Pokemon2, Pokemon3, Pokemon4, Pokemon5, Pokemon6)
% TODO: doubles
      % 2 pokemon on a field at a time. idk if this is too similar to solo battles
      % the only thing that's really affected are the moves it uses. might be put off
% TODO: moves - find csv, generate strategies (ex. 1 heal, 1 spatk, 1 atk, 1 buff)
      % DO THIS LAST
