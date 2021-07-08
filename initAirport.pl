%% This is file initAirport.pl %%
	/*---------------- The initial state.------------------ */

/* Objects in the airport domain
      %% the airplanes: out-bound boeing347  and  in-bound md25. 
      %% the airplane types: medium and light.
      %% the segments
                       v0
                       |
                       v8
                       |
               v5--v7--v2--v9--v1 	
                |      |
                |      v6
                |      |
               v4------v3
*/
	adjacent(v0,v8).	adjacent(v1,v9).
	adjacent(v8,X) :- member(X,[v0,v2]).
	adjacent(v9,X) :- member(X,[v1,v2]).
	adjacent(v7,X) :- member(X,[v5,v2]).
	adjacent(v6,X) :- member(X,[v3,v2]).
	adjacent(v2,X) :- member(X,[v8,v9,v6,v7]).
	adjacent(v3,X) :- member(X,[v6,v4]).
	adjacent(v4,X) :- member(X,[v3,v5]).
	adjacent(v5,X) :- member(X,[v7,v4]).

isStartRunway(seg(v3,v4), v4).
isStartRunway(seg(v5,v4), v4).

      atSegment(boeing347, seg(v8,v0),[]).
      blocked(seg(v8,v0),boeing347,[]).
      blocked(seg(v0,v8),boeing347,[]).
      hasType(boeing347,medium).
      facing(boeing347,v0,[]).
	isParked(boeing347,seg(v8,v0),[]).

      atSegment(md25, seg(v4,v3),[]).
      blocked(seg(v4,v3),md25,[]).
      blocked(seg(v3,v4),md25,[]).
      hasType(md25,light).
      facing(md25,v3,[]).
	isMoving(md25,[]).


	/* ------------------ Goal state ---------------------*/

	%% First planning problem to solve: use this goal state
goal_state(S) :- airborne(boeing347,Seg,S).

	%% Second planning problem to solve: use this goal state
%  goal_state(S) :-  isParked(md25,seg(v9,v1),S).

%%This 3rd planning problem needs 12 steps and takes about 10min without heuristics.
% goal_state(S) :- 
%	isParked(md25,seg(v9,v1),S),
%	airborne(boeing347,Seg,S).

