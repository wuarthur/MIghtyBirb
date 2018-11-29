%% I think we should ignore speed && Status buffs debuffs to be honest
%% I dont have that much time next week, since im pretty scewed for STAT 404
%% Stretch goal tho
%% ALSO DID WE BOOK A DEMO DATE?

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
%%% We cant just write a new line because we have too many pokemon, need to display like 5 per line.
%%% Ans is a list of args passed into Resolv fn, 1-1 relationship between Opt <-> Ans
ask_user(Q, Opts, Ans, Resolv):-
  save_q(Q, Opts, Ans, Resolv),
  clear_screen,
  writef('%w\n', [Q]),
  add_number_prefix(Opts, O),
  maplist(write, O),
  write('\n'),
  parse_input(Ans, Resolv).

parse_input(Ans, Resolv):-
  read(Input),
  try_input(Input, Ans, Resolv).

try_input(Input, Ans, Resolv):-
  valid_input(Input, Ans),
  Idx is Input - 1,
  nth0(Idx, Ans, A),
  call(Resolv, A).

try_input(Input, Ans, Resolv):-
  not(valid_input(Input, Ans)),
  write('Please try again with a valid selection\n'),
  parse_input(Ans, Resolv).

valid_input(Input, Ans):-
  number(Input),
  length(Ans, L),
  Input >= 0,
  Input =< L.


add_number_prefix(List, Prefixed):-
  length(List, L),
  findall(N, between(1, L, N), Nums),
  maplist(concat_prefix, Nums, List, Prefixed).

concat_prefix(N, S, R):-
  swritef(R, '%w. %w', [N, S]).


%%%%%%%%%%%%%% Some errors dont seem to be catchable using the prologs catch/3.
%%%%%%%%%%%%%% For example, if user input in only "." -> Syntax error: Unexpected end of clause
%%%%%%%%%%%%%% Here is how you recover, call "recover."

:- dynamic last_q/4.

save_q(Q, Opts, Ans, Resolv):-
  assertz(last_q(Q, Opts, Ans, Resolv)).
save_q(Q, Opts, Ans, Resolv):-
  retractall(last_q(_, _, _, _)),
  assertz(last_q(Q, Opts, Ans, Resolv)).

recover:-
  last_q(Q, Opts, Ans, Resolv),
  ask_user(Q, Opts, Ans, Resolv).
