% The dynamic declaration prevents undefined predicate errors.
:- dynamic on_t1/0, on_t2/0, on_t3/0, off_t1/0, off_t2/0, off_t3/0, plugged_bath/0,
	   unplugged_bath/0, plugged_sink/0, unplugged_sink/0, hot_water/0.

% See assignment description for the intended interpretation of the symbols
pressurized_p1.
% Bath related statements
pressurized_p2 :- on_t1, pressurized_p1.
flow_shower :- on_t2, pressurized_p2.
wet_bath :- flow_shower.
flow_d2 :- wet_bath, unplugged_bath.


% Sink related statements
% question (a)
pressurized_p3 :- on_t1, pressurized_p1.  % check if pipe to sink is pressurized
flow_sink :- on_t3, pressurized_p3.       % sink can flow if tap is on and its pressurized
wet_sink :- flow_sink.                    % sink is wet when water flows through it
flow_d3 :- wet_sink, unplugged_sink.      % can drain out water if unplugged and there is water

flow_d1 :- flow_d2.                       % from original file, for bath
flow_d1 :- flow_d3.                       % if there is water in the sink drain, theres water in the main drain

% overflow related statements
% question (b)
overflow_sink :- wet_sink, plugged_sink.  % sink has overflowed
overflow_bath :- wet_bath, plugged_bath.  % bath has overflowed
wet_floor :- overflow_sink.               % floor is wet if sink has overflowed or bath has overflowed
wet_floor :- overflow_bath.


% hot water related statements
% question (c)
on_ht1 :- on_t1, hot_water.               % hot water tap next to on_t1. Must have water flowing through it
on_ht2 :- on_t2, on_ht1.                  % hot water tap for bath
hot_bath :- on_ht2, flow_shower.          % bath water is hot
on_ht3 :- on_t3, on_ht1.                  % hot water tap for sink
hot_sink :- on_t3, on_ht1.                % sink water is hot
hot_floor :- hot_bath, wet_floor.         % water on the floor is hot
hot_floor :- hot_sink, wet_floor.


% You can change the status of taps and plugs by adding or removing (commenting out) statements:
hot_water.
on_t1.
on_t2.
on_t3.
unplugged_bath.
unplugged_sink.

/**
[trace]  ?- hot_floor.
   Call: (8) hot_floor ?
   Call: (9) hot_bath ?
   Call: (10) on_ht2 ?
   Call: (11) on_t2 ?
   Exit: (11) on_t2 ?
   Call: (11) on_ht1 ?
   Call: (12) on_t1 ?
   Exit: (12) on_t1 ?
   Call: (12) hot_water ?
   Exit: (12) hot_water ?
   Exit: (11) on_ht1 ?
   Exit: (10) on_ht2 ?
   Call: (10) flow_shower ?
   Call: (11) on_t2 ?
   Exit: (11) on_t2 ?
   Call: (11) pressurized_p2 ?
   Call: (12) on_t1 ?
   Exit: (12) on_t1 ?
   Call: (12) pressurized_p1 ?
   Exit: (12) pressurized_p1 ?
   Exit: (11) pressurized_p2 ?
   Exit: (10) flow_shower ?
   Exit: (9) hot_bath ?
   Call: (9) wet_floor ?
   Call: (10) overflow_sink ?
   Call: (11) wet_sink ?
   Call: (12) flow_sink ?
   Call: (13) on_t3 ?
   Exit: (13) on_t3 ?
   Call: (13) pressurized_p3 ?
   Call: (14) on_t1 ?
   Exit: (14) on_t1 ?
   Call: (14) pressurized_p1 ?
   Exit: (14) pressurized_p1 ?
   Exit: (13) pressurized_p3 ?
   Exit: (12) flow_sink ?
   Exit: (11) wet_sink ?
   Call: (11) plugged_sink ?
   Fail: (11) plugged_sink ?
   Fail: (10) overflow_sink ?
   Redo: (9) wet_floor ?
   Call: (10) overflow_bath ?
   Call: (11) wet_bath ?
   Call: (12) flow_shower ?
   Call: (13) on_t2 ?
   Exit: (13) on_t2 ?
   Call: (13) pressurized_p2 ?
   Call: (14) on_t1 ?
   Exit: (14) on_t1 ?
   Call: (14) pressurized_p1 ?
   Exit: (14) pressurized_p1 ?
   Exit: (13) pressurized_p2 ?
   Exit: (12) flow_shower ?
   Exit: (11) wet_bath ?
   Call: (11) plugged_bath ?
   Fail: (11) plugged_bath ?
   Fail: (10) overflow_bath ?
   Fail: (9) wet_floor ?
   Redo: (8) hot_floor ?
   Call: (9) hot_sink ?
   Call: (10) on_t3 ?
   Exit: (10) on_t3 ?
   Call: (10) on_ht1 ?
   Call: (11) on_t1 ?
   Exit: (11) on_t1 ?
   Call: (11) hot_water ?
   Exit: (11) hot_water ?
   Exit: (10) on_ht1 ?
   Exit: (9) hot_sink ?
   Call: (9) wet_floor ?
   Call: (10) overflow_sink ?
   Call: (11) wet_sink ?
   Call: (12) flow_sink ?
   Call: (13) on_t3 ?
   Exit: (13) on_t3 ?
   Call: (13) pressurized_p3 ?
   Call: (14) on_t1 ?
   Exit: (14) on_t1 ?
   Call: (14) pressurized_p1 ?
   Exit: (14) pressurized_p1 ?
   Exit: (13) pressurized_p3 ?
   Exit: (12) flow_sink ?
   Exit: (11) wet_sink ?
   Call: (11) plugged_sink ?
   Fail: (11) plugged_sink ?
   Fail: (10) overflow_sink ?
   Redo: (9) wet_floor ?
   Call: (10) overflow_bath ?
   Call: (11) wet_bath ?
   Call: (12) flow_shower ?
   Call: (13) on_t2 ?
   Exit: (13) on_t2 ?
   Call: (13) pressurized_p2 ?
   Call: (14) on_t1 ?
   Exit: (14) on_t1 ?
   Call: (14) pressurized_p1 ?
   Exit: (14) pressurized_p1 ?
   Exit: (13) pressurized_p2 ?
   Exit: (12) flow_shower ?
   Exit: (11) wet_bath ?
   Call: (11) plugged_bath ?
   Fail: (11) plugged_bath ?
   Fail: (10) overflow_bath ?
   Fail: (9) wet_floor ?
   Fail: (8) hot_floor ?
false.
**/
