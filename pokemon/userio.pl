%% I think we should ignore speed && Status buffs debuffs to be honest
%% I dont have that much time next week, since im pretty scewed for STAT 404
%% Stretch goal tho
%% ALSO DID WE BOOK A DEMO DATE?


%  TODO - Console IO

/*
  Part 1: assemble team

  1) ask if they want to build a team themselves or have assistance from the program
  2) if themselves:
    - ask them for the first pokemon of their team (must give national dex number. reply 'any' if
    it doesn't matter, reply 'stop' to stop and have a team of less than 6 pokemon)
  3) if assistance:
    - ask them if they want a random team or a strategy
    a) if random:
      - literally randomly generate a team of 6. can have repeats
    b) if strategy:
      - # options. after each option tree is done return here.
      - by default:
          - complementing types
          - any stat
          - no legendary
          - any generation
          - allow repeats
      i) complementing type or same type
        1) if same type:
          - get them to specify a type (display available)
        2) if complementing:
          - add this fact to the kb
      ii) focus on a specific stat
        1) declare stat distribution
      iii) allow legendary y/n
      iv) set generation
        1) allow previous generations? y/n
      v) allow repeats y/n

  4) generate team
  5) decide rival difficulty
      - difficulty is decided by the base stat. we will section them off so that
        each teir of base stat represents the next difficulty level
  6) generate rival team

  Part 2: battle

  6) automatic battle or manual? jump to end?
  7) ^ gets repeated everytime someone faints

  8) when game over (either team has no pokemon),
     player can either play again versus a new rival or change teams

*/



%% We try to use only number keys as user input, instead of making them type
%% pokemon names and such, since exact matches are hard.

%% We list all possible user inputs and let them choose
%% There will be no REDO step, we probably wont have time for it.


% Run clear screen before next input function
clear_screen:-
  rep_list('\n', 100, Blank_lines),
  maplist(write, Blank_lines).


%%% Question Loop
%%% Q is Question you want to ask
%%% Opts is a list of options presented to user, must be pre formatted with \t or \n
%%% Ans is a list of args passed into Resolv fn, 1-1 relationship between Opt <-> Ans
ask_user(Q, Opts, Ans, Resolv):-
  clear_screen,
  writef('%w\n', [Q]),
  maplist(write, Opts),
  write('\n'),
  read(Input),
  Idx is Input - 1,
  nth0(Idx, Ans, A),
  call(Resolv, A).

%% Example
ask_q1:-
  ask_user(
    'Hi there, whats your fav colour',
    ['blue\n', 'red\n', 'purple\n'],
    ['B\n', 'R\n', 'P\n'],
    write
  ).

%% Write Q for picking a pokemon

%% Write Q for using move in battle

%% Write Q for