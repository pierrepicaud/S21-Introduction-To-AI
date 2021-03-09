/*
# Promt
0. No mask, need to reach home, can buy mask on the way home
1. Your environment is a 9 by 9 square
2. Actors, start from bottom left, move like king in chess.
3. Covid, there are 2 of them, generated randomly on the map. Its AOE is like a king, enter it w/o mask or prior doctor visit ends the game. You are safe from covid if you had visited a doctor before or you have a mask.
4. Doctor, generated randomly outside of covid zone, Actor do not know where will the doctor be unless it's in the doctor's AOE, which is like king in chess, doctor can make you immune to covid
5. Home, randomly generated outside covid's AOE, the actor knows the location of the home.
6. Mask, gives covid immunity, generated randomly outside of covid's AOE

Example world (D = doctor, C = covid, M = mask, H = home):

  =====================================================
  |    |     |    |    |	|    |     |    |    |	  |
  |----|-----|----|----|----|----|-----|----|----|----|
  |    |     |    |    |	|    |  D  |    |    |	  |
  |----|-----|----|----|----|----|-----|----|----|----|
  |    |  M  |    |    |	|    |     |    | H  |	  |
  |----|-----|----|----|----|----|-----|----|----|----|
  |    |     |    | C  |	|    |     |    | ^  |	^ |
  |----|-----|----|----|----|----|-----|----|----|----|  
  |    |     |    |    |	|    |     |    | ^  | C  |
  |----|-----|----|----|----|----|-----|----|----|----|
  |    |     |    |    |	|  ^ |  ^  | ^  | ^  |	^ |
  |----|-----|----|----|----|----|-----|----|----|----|
  |1/3 |     |    |    |	|  ^ |  D  | ^  |    |	  |
  |----|-----|----|----|----|----|-----|----|----|----|
  |1/2 | 2/2 |    |    |	|  ^ |  ^  | ^  |    |	  |
  |----|-----|----|----|----|----|-----|----|----|----|
  |1/1 | 2/1 |3/1 |    |	|    |     |    |    |	  |
  =====================================================

*/

% :- style_check(-discontiguous).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

% This would have been easier had I've got practiced in coq
% The actor doesn't know about the covid and the doctor like how the actor doesn't know about the wumpus
% How does vacinated or mask works here?
% How to generate graph randomly? Doing
% What todo about the scream? Turned it into Immuned
% climb? the agent can climb out of the pit where he came in, in our case it would be get in home


% Current Goal
% Still can't figure it out the logic behind how to find the path
% Write doctor constraint
% Write home constraint
% Write mask
% Make a randomly generated map
% Make a GUI
% make a version so that it can track home and go to it.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%AGENT-CODE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% world generation with constraints


% random generation without constraint
c_generate_covid(X/Y-covid):-
    random_between(1, 9, X), random_between(1, 9, Y).
c_generate_home(X/Y-home):-
    random_between(1, 9, X), random_between(1, 9, Y).
c_generate_doctor(X/Y-doctor):-
    random_between(1, 9, X), random_between(1, 9, Y).
c_generate_mask(X/Y-mask):-
    random_between(1, 9, X), random_between(1, 9, Y).


% Problem: No Constraint, solution, exploit some pre-written code.
c_generate_world(W):-
    c_generate_covid(C0), append([], [C0], W0),
	c_generate_covid(C1), append(W0, [C1], W1),
	c_generate_doctor(D0), append(W1, [D0], W2),
    c_generate_doctor(D1), append(W2, [D1], W3),
    c_generate_mask(M), append(W3, [M], W4),
    c_generate_home(H), append(W4, [H], W5),
    append([], W5, W).
	

% world setting
c_world(W) :-
        W = [9/5-covid, 4/6-covid, 2/7-mask, 6/8-doctor, 6/3-doctor, 8/7-home].	% Need tweaking.


% [helper] At Pos there is Ent is true if Pos-Ent exist in world.
c_world_pos_ent(Pos, Ent) :-
        c_world(W),
        member(Pos-Ent, W).


% [helper] There is Perc at Pos if...
c_world_pos_near(Pos, Ent, Perc) :-
        (   c_world_pos_ent(Pos, Ent) -> 
            Perc = 1 % ...if Pos-Ent exist in world or...
        ;   c_pos_neighbour(Pos,Nb), c_world_pos_ent(Nb,Ent) ->
            Perc = 1 % ...if Pos-Ent exist in neighbour of Post-Ent or...
        ;   Perc = 0 % ...or Perc doesn't exist.
        ).

% [helper] Return what? 
c_world_pos_perception(Actions, Pos-_Dir, Perc) :-
        c_world_pos_near(Pos, covid, Ppl),
        c_world_pos_near(Pos, doctor, Alco),
    	% member(X, [One, Two]). Iterate X over the list.
        ((c_world_pos_ent(Pos, mask), \+ member(pa(Pos-_,grab),Actions)) ->
                SeeMask = 1 ; SeeMask = 0 ),
        Perc = perc(Ppl, Alco, SeeMask, _Immuned, _Bump). % Changed scream to immuned



% Check neighborhood
c_pos_neighbour(X/Y, NX/NY) :-
        (   NX #= X - 1, NY = Y
        ;   NX #= X + 1, NY = Y
        ;   NX = X, NY #= Y - 1
        ;   NX = X, NY #= Y + 1
        ).


% Set initial condition
% Q1 What does ...(1/1-_) mean? It means don't care about what's is in the "_"
c_startpos(1/1-_).
c_startdir(right).
c_home(9/9). % Sample home location for testing.

% [helper] This compares the locations between two cells,
% so that it can turn appropriately to that cell.
c_agent_faces(X/Y-right, X1/Y) :- X1 #> X.
c_agent_faces(X/Y-down, X/Y1)  :- Y1 #< Y.
c_agent_faces(X/Y-left, X1/Y)  :- X1 #< X.
c_agent_faces(X/Y-up, X/Y1)    :- Y1 #> Y.

% [helper] For failing the whole rule
c_error(Error) :-
        writeln(Error),
        false.

% [helper] Move validator
c_valid_action(Action, _Pos, _Hist) :-
        memberchk(Action, [right,left,forward]),
        !.

% [helper] Mask coordination validator
c_valid_action(grab, Pos, Hist) :-
        (   c_world_pos_perception(Hist, Pos, perc(_P,_A,1,_Scr,_Bump)) ->
            true
        ;   c_error('There is no mask here.')
        ).

% [helper] Home coordinate validator
c_valid_action(getIn, Pos, _Hist) :-
        (   c_home(Pos) ->
            true
        ;   c_error('Here is not home.')
        ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%AGENT-CODE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


agent_make(S0) :-
        S0 = s(1/1-right,[],[]).


agent_next(S0, Perc, grab, S) :-
        Perc = perc(_Stench,_Breeze,1,_Scream,_Bump),
        !, % Gold? ==> grab
        S0 = s(Pos, Plan, Hist),
        S = s(Pos, Plan, [h(Pos,Plan,Perc,grab)|Hist]).


agent_next(S0, perc(_St,_Br,_Gl,_Scream,1), Action, S) :- !,
        % we bumped - previous position is correct, forget our plan
        S0 = s(_Pos,_Plan,Hist),
        Hist = [h(HPos,_,_,_)|_],
        Action = right, % do anything
        pos_move_newpos(HPos, Action, Newpos),
        S = s(Newpos, [], Hist).


agent_next(s(Pos,[climb],Hist), _Perc, climb, s(Pos,[],Hist)) :- !.


agent_next(s(Pos,[Plan1|Plans],Hist), Perc, Action, S) :- !,
        % normally, we only follow the plan we made previously
        pos_nextpos_goodmove(Pos, Plan1, Action),
        pos_move_newpos(Pos, Action, Pos1-Dir1),
        ( Pos1 = Plan1 ->
                Nextplan = Plans  % reached desired position, head for next
        ;
                Nextplan = [Plan1|Plans]
        ),
        S = s(Pos1-Dir1,Nextplan,[h(Pos,[Plan1|Plans],Perc,Action)|Hist]).


agent_next(s(Pos0,[],Hist0), Perc, Action, S) :-
        % plan is used up - build a new one if possible
        Perc = perc(_St,_Br,0,_Scr,0),
        Hist = [h(Pos0,[],Perc,Action)|Hist0],
        Pos0 = P0-_Dir0,
        ( next_target(Hist, Target) ->
                once(pos_target_hist_plan(P0,Target,Hist,Plan)),
                Plan = [Plan1|_],
                pos_nextpos_goodmove(Pos0, Plan1, Action),
                pos_move_newpos(Pos0, Action, Newpos),
                S = s(Newpos, Plan, Hist)
        ;
                % no new target left - get out of this maze
                once(pos_target_hist_plan(P0,1/1,Hist,Rescueplan0)),
                append(Rescueplan0, [climb], Rescueplan),
                Action = right,  % do anything
                pos_move_newpos(Pos0, Action, Newpos),
                S = s(Newpos,Rescueplan,Hist)
        ).


in_previous_plan(Pos, Hist) :-
        member(h(_Pos,Plan,_Perc,_Action), Hist),
        memberchk(Pos, Plan).

pos_perception(Pos, Hist, Perc) :-
        member(h(Pos-_Dir,_Plan,Perc,_Action), Hist).


no_wumpus(Pos, Hist) :-
        pos_neighbour(Pos, Neighbour),
        pos_visited(Neighbour, Hist),
        pos_perception(Neighbour, Hist, perc(0,_Breeze,_Glitter,_Scream,_Bump)).

no_pit(Pos, Hist) :-
        pos_neighbour(Pos, Neighbour),
        pos_visited(Neighbour, Hist),
        pos_perception(Neighbour, Hist, perc(_Stench,0,_Glitter,_Scream,_Bump)).


next_target(Hist,Target) :-
        pos_visited(Somepos, Hist),
        pos_neighbour(Somepos, Neighbour),
        \+ pos_visited(Neighbour, Hist),
        \+ in_previous_plan(Neighbour, Hist),
        no_wumpus(Neighbour, Hist),
        no_pit(Neighbour, Hist),
        Target = Neighbour.


% make a plan about how to go from Pos to Target, moving along nodes that
% were already visited previously
pos_target_hist_plan(Pos, Target, _Hist, [Target]) :-
        pos_neighbour(Pos, Target).
pos_target_hist_plan(Pos, Target, Hist, [Next|Rest]) :-
        pos_neighbour(Pos, Next),
        pos_visited(Next, Hist),
        pos_target_hist_plan(Next, Target, Hist, Rest).

pos_visited(Pos, Hist) :-
        member(h(Pos-_Dir,_Plan,_Perc,_Action), Hist).

% checking facing direction
pos_faces(right, X/Y, X1/Y) :- X1 #> X.
pos_faces(down, X/Y, X/Y1)  :- Y1 #< Y.
pos_faces(left, X/Y, X1/Y)  :- X1 #< X.
pos_faces(up, X/Y, X/Y1)    :- Y1 #> Y.

pos_nextpos_goodmove(Coord-Dir, Next, Move) :-
        (   
        	pos_faces(Dir, Coord, Next) ->
            Move = forward
        ;   Move = right
        ).


rotation(right).
rotation(left).

% moving
pos_move_newpos(X/Y-right, forward, X1/Y-right) :- X1 #= X + 1.
pos_move_newpos(X/Y-down, forward, X/Y1-down) :-   Y1 #= Y - 1.
pos_move_newpos(X/Y-left, forward, X1/Y-left) :-   X1 #= X - 1.
pos_move_newpos(X/Y-up, forward, X/Y1-up) :-       Y1 #= Y + 1.
pos_move_newpos(X/Y-Dir, Rot, X/Y-NDir) :-
        rotation(Rot),
        dir_rot_newdir(Dir, Rot, NDir).


% direction_rotation_direction(_, clocwise ; anti-clockwise, ).
dir_rot_newdir(right, right, down).
dir_rot_newdir(right, left, up).
dir_rot_newdir(down, right, left).
dir_rot_newdir(down, left, right).
dir_rot_newdir(left, right, up).
dir_rot_newdir(left, left, down).
dir_rot_newdir(up, right, right).
dir_rot_newdir(up, left, left).

% check if two cells are neighbor or not.
pos_neighbour(X/Y, NX/NY) :-
        (   NX #= X - 1, NY = Y
        ;   NX #= X + 1, NY = Y
        ;   NX = X, NY #= Y - 1
        ;   NX = X, NY #= Y + 1
        ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%OLDS_STUFF%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gridsize(9).
gridindex(V):-
	gridsize(S),
	between(1, S, V).

location(coord(I,J)) :- gridindex(I), gridindex(J).

direction(V) :- member(V,[up,down,left,right]).



















