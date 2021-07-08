/*

Brandon Holmes, Section 2, 500751878
Nya Samahan, Section 2, 500634913
Rafael Natividad, Section 3, 500705909

*/

:- discontiguous(isParked/3).
:- discontiguous(atSegment/3).
:- discontiguous(blocked/3).
:- discontiguous(facing/3).
:- discontiguous(isMoving/2).
:- discontiguous(hasType/2).

%% load initAirport.pl with the initial and goal states
:- [ initAirport ].

solve_problem(N,L) :- C0 is cputime,
                      max_length(L,N),
                      reachable(S,L), goal_state(S),
                      Cf is cputime, D is Cf - C0, nl,
                      write('Elapsed time (sec): '), write(D), nl.

max_length([],N) :- N >= 0.
max_length([_|L],N1) :- N1 > 0, N is N1 -1, max_length(L,N).

reachable(S,[]) :- initial_state(S).
reachable(S2, [M|List]) :- reachable(S1,List), 
                    legal_move(S2,M,S1).   

legal_move([A|S], A, S) :- poss(A,S).
initial_state([]).


    /* --------------- Precondition Axioms ----------------- */

poss(move(Airplane,Type,Dir1,Seg1,Seg2,Dir2),S) :- 
	isMoving(Airplane,S),
	atSegment(Airplane,Seg1,S),
	facing(Airplane,Dir1,S),
	hasType(Airplane,Type),
	canMove(Seg1,Seg2,Dir1),
	moveDir(Seg1,Seg2,Dir2),
	Seg1 = seg(V1,V2),
	Seg2 = seg(V2,V3),
	not(V1=V3),
	not( (blocked(Seg2,Airplane2,S),
		not(Airplane2=Airplane)) ),
	not( (atSegment(Airplane3,Seg3,S),
		not(Airplane3=Airplane),
		not(Seg3=Seg2),
		segIsBlocked(Seg3,Type,Seg2,Dir2)) ).

poss(pushback(Airplane,Type,Dir1,Seg1,Seg2,Dir2),S) :- 
	(isParked(Airplane,Seg1,S) ; isPushing(Airplane,S)),
	atSegment(Airplane,Seg1,S),	
	facing(Airplane,Dir1,S),
	hasType(Airplane,Type),
	canPushback(Seg1,Seg2,Dir1),
	moveBackDir(Seg1,Seg2,Dir2),
	not( (blocked(Seg2,Airplane2,S),
		not(Airplane2=Airplane)) ).

poss(startup(Airplane),S) :- 
	isPushing(Airplane,S),
	atSegment(Airplane,Seg,S),
	facing(Airplane,Dir,S),
	hasType(Airplane,Type),
	not( (atSegment(Airplane2,Seg2,S),
		not(Airplane2=Airplane),
		not(Seg2=Seg),
		segIsBlocked(Seg2,Type,Seg,Dir)) ).

poss(park(Airplane,Type,Seg,Dir),S) :- 
	isMoving(Airplane,S),
	atSegment(Airplane,Seg,S),	
	facing(Airplane,Dir,S),
	hasType(Airplane,Type),
	not( isStartRunway(Seg,Dir) ).

poss(takeoff(Airplane,Seg,Dir),S) :- 
	isMoving(Airplane,S),
	atSegment(Airplane,Seg,S),	
	facing(Airplane,Dir,S),
	isStartRunway(Seg,Dir).

/*--------------- Sucessor State Axioms --------------------*/

/* Given: fluent blocked(Seg,Airplane,S) */


blocked(Seg2,Airplane, [move(Airplane,_Type,_Dir1,_Seg1,Seg2,_Dir2) | _S]).
 
blocked(Seg,Airplane, [move(Airplane,_Type,_Dir1,_Seg1,Seg2,_Dir2) | _S]) :-
	opposite(Seg,Seg2).

blocked(Segment,Airplane,[move(Airplane,Type,_Dir1,_Seg1,Seg2,Dir2) | _S]) :-
	segIsBlocked(Segment,Type,Seg2,Dir2).

blocked(Seg2,Airplane, [pushback(Airplane,_Type,_Dir1,_Seg1,Seg2,_Dir2) | _S]).

blocked(Seg1,Airplane, [pushback(Airplane,_Type,Dir1,Seg1,Seg2,Dir2) | _S]) :-
	opposite(Seg1,Seg2), different(Dir1,Dir2). 

/* When Airplane is being pushed away from a gateway, its engines are off,
 and for this reason it does not block any segments it is not at.
 But when an Airplane starts up its engines, it starts blocking a few segments. */

blocked(Segment,Airplane, [startup(Airplane)|S]) :-
	hasType(Airplane, Type),
	facing(Airplane,Dir1,S),
	atSegment(Airplane,Seg1,S),
	segIsBlocked(Segment,Type,Seg1,Dir1).
       
/* If Seg1 is blocked in situation S, it remains blocked in the next 
   situation [A|S] if it is not true that the most recent action A was 
   moving from Seg1 to Seg2 and Seg1 is not blocked from Seg2,        
   and
   it is not true that the most recent action was moving from some Seg,Dir
   to Seg2,Dir2 and Seg1 was blocked from Seg, but is not blocked from Seg2, 
   and
   the last action is not pushback from Seg1 to a distinct segment Seg2,         
   and
   the last action is not takeoff from Seg1 or from another segment,  
   and
   Airplane did not park at a segment such that Seg1 is blocked from it:
   after parking Seg1 ceases to become blocked because engines turn off.
   Otherwise, if one of the above conditions fails, then Seg1 is no longer 
   blocked in the next state. 
*/

blocked(Seg1,Airplane, [A | S]) :-
	not( (A=move(Airplane,Type,Dir1,Seg1,Seg2,Dir2),
		not(segIsBlocked(Seg1,Type,Seg2,Dir2))) ), 

	not( (A=move(Airplane,Type,SomeDir,SomeSeg,Seg2,Dir2),
		segIsBlocked(Seg1,Type,SomeSeg,SomeDir), 
		not(Seg1=Seg2),
		not(segIsBlocked(Seg1,Type,Seg2, Dir2))) ),

	not( (A=pushback(Airplane,Type,Dir1,Seg1,Seg2,Dir2),
		not(opposite(Seg1,Seg2))) ),

/* Seg1 that is blocked by Airplane in the current situation S remains 
blocked by this Airplane in the next situation [A|S] unless the last action A
is such that the airplane took off from this or another segment.	*/

	not(A=takeoff(Airplane,_Segment,Dir)),
	not( (A=park(Airplane,Type,SomeSeg,Dir),
	      segIsBlocked(Seg1,Type,SomeSeg,Dir),
		not(SomeSeg = Seg1)) ), blocked(Seg1,Airplane,S).


isParked(Airplane,Seg,[park(Airplane,_Type,Seg,_Dir) | _S]).
isParked(Airplane,Seg,[A | S]) :-
	not( A=pushback(Airplane,_Type,_Dir) ),
	isParked(Airplane,Seg,S).

airborne(Airplane,Seg,[takeoff(Airplane,Seg,_Dir) | _S]).
airbone(Airplane,Seg,[_A | S]) :-
	airbone(Airplane,Seg,S).

isMoving(Airplane,[move(Airplane,_Type,_Dir1,_Seg1,_Seg2,_Dir2) | _S]).

isMoving(Airplane,[startup(Airplane) | _S]).

isMoving(Airplane,[A | S]) :-
	not( A=park(Airplane,_Type,_Seg1,_Dir1) ),
	not( A=takeoff(Airplane,_Seg2,_Dir2) ),
	isMoving(Airplane,S).

isPushing(Airplane,[pushback(Airplane,_Type,_Dir1,_Seg1,_Seg2,_Dir2) | _S]).
isPushing(Airplane,[A | S]) :-
	not( A=startup(Airplane) ),
	isPushing(Airplane,S).

atSegment(Airplane,Segment,[move(Airplane,_Type,_Dir1,_Seg1,Segment,_Dir2) | _S]).

atSegment(Airplane,Segment,[pushback(Airplane,_Type,_Dir1,_Seg1,Segment,_Dir2) | _S]).

atSegment(Airplane,Segment,[A | S]) :-
	not( (A=move(Airplane,_Type1,_Dir1,_Seg1,Seg2,_Dir2),
		not(Seg2=Segment)) ),
	not( (A=pushback(Airplane,_Type2,_Dir3,_Seg3,Seg4,_Dir4),
		not(Seg4=Segment)) ),
	not( A=takeoff(Airplane,_Seg,_Dir) ),
	atSegment(Airplane,Segment,S).

facing(Airplane,Direction,[move(Airplane,_Type,_Dir1,_Seg1,_Seg2,Direction) | _S]).

facing(Airplane,Direction,[pushback(Airplane,_Type,_Dir1,_Seg1,_Seg2,Direction) | _S]).

facing(Airplane,Direction,[A | S]) :-
	not( (A=move(Airplane,_Type1,_Dir1,_Seg1,_Seg2,Dir2),
		not(Dir2=Direction)) ),
	not( (A=pushback(Airplane,_Type2,_Dir3,_Seg3,_Seg4,Dir4),
		not(Dir4=Direction)) ),
	not( A=takeoff(Airplane,_Seg,_Dir) ),
	facing(Airplane,Direction,S).

/*-------------- Domain Properties ------------*/

segIsBlocked(seg(V1,V2), _Type, seg(V1,V2), V2).
segIsBlocked(seg(V1,V2), _Type, seg(V2,V1), V1).

segIsBlocked(Seg1, Type, Seg2, Dir2) :-
	(Type = medium ; Type = heavy),
	canMove(Seg1,Seg2,_Dir1),
	moveDir(Seg1,Seg2,Dir2).

canMove(seg(V1,V2), seg(V2,V3), V2) :- 
	adjacent(V1,V2), adjacent(V2,V3).	

moveDir(seg(V1,V2), seg(V2,V3), V3) :-
	adjacent(V1,V2), adjacent(V2,V3).	

canPushback(seg(V2,V3),seg(V1,V2),V3) :- 
	adjacent(V1,V2), adjacent(V2,V3).	

moveBackDir(seg(V2,V3),seg(V1,V2),V2) :-
	adjacent(V1,V2), adjacent(V2,V3).	

different(V1,V2) :- not(V1=V2).

/* An airplane can be pushed from gateway by turning it around its axis. 
  In this case, it is re-oriented from a segment between vertices (V1,V2) with 
  heading towards V2 to the same segment, but with the vertex V1 at front.  */
opposite(seg(V1,V2),seg(V2,V1)).


