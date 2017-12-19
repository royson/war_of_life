%Not needed for submission
%:- ensure_loaded('war_of_life.pl').

test_strategy(N, SP1, SP2) :-
  statistics(runtime,[Start, _]),
  test_strategy(N, SP1, SP2, Moves, Results),
  statistics(runtime,[End, _]),
  Time is End - Start,
  AverageTime is Time/N,
  
  sumlist(Moves, TotalMoves),
  AverageMoves is TotalMoves/N,
  delete(Moves, 250, MovesWOExhaustive),
  max_member(LongestMove, MovesWOExhaustive),
  min_member(ShortestMove, Moves),
  numberOfOccurrenceInList(b, Results, BlueWins),
  numberOfOccurrenceInList(r, Results, RedWins),
  Draws is N - BlueWins - RedWins,
  AvgTimeInSeconds is AverageTime/1000,

  write('Number of draws: '), write(Draws),  nl,
  write('Number of wins for player 1 (blue): '), write(BlueWins), nl,
  write('Number of wins for player 2 (red): '), write(RedWins), nl,
  write('Longest (non-exhaustive) game: '), write(LongestMove), nl,
  write('Shortest game: '), write(ShortestMove), nl,
  write('Average game length (including exhaustives): '), write(AverageMoves), nl,
  write('Average game time: '), write(AvgTimeInSeconds), write(' seconds'), nl.

test_strategy(0, _, _, [], []).
test_strategy(N, SP1, SP2, Moves, Results) :-
  play(quiet, SP1, SP2, NumMoves, WinningPlayer),
  NextN is N - 1,
  test_strategy(NextN, SP1, SP2, Mv, Re),
  append(Mv, [NumMoves], Moves),
  append(Re, [WinningPlayer], Results).
  
%%%%%%%%% Strategies %%%%%%%%%

bloodlust(P, [CBlue,CRed], [NBlue,NRed], [R,C,AR,AC]) :-
  opponent(P,Opp),
  possibleMoves(P, [CBlue,CRed], Moves),
  futureBoard([CBlue, CRed], Moves, List),
  piecesToList(Opp, List, ListOfOppPieces),
  min_member(MinPiece, ListOfOppPieces),
  extractMoveFromList(Opp, MinPiece, List, [R,C,AR,AC]),
  executeMove([R,C,AR,AC], [CBlue,CRed], [NBlue,NRed]).

self_preservation(P, [CBlue,CRed], [NBlue,NRed], [R,C,AR,AC]) :-
  possibleMoves(P, [CBlue,CRed], Moves),
  futureBoard([CBlue, CRed], Moves, List),
  piecesToList(P, List, ListOfOwnPieces),
  max_member(MaxPiece, ListOfOwnPieces),
  extractMoveFromList(P, MaxPiece, List, [R,C,AR,AC]),
  executeMove([R,C,AR,AC], [CBlue,CRed], [NBlue,NRed]).

land_grab(P, [CBlue,CRed], [NBlue,NRed], [R,C,AR,AC]) :-
  possibleMoves(P, [CBlue,CRed], Moves),
  futureBoard([CBlue, CRed], Moves, List),
  differenceInPieces(P, List, DListWMoves),
  differencesToList(DListWMoves, DList),
  max_member(MaxDiff, DList),
  extractDifferenceMoveFromList(MaxDiff, DListWMoves, [R,C,AR,AC]),
  executeMove([R,C,AR,AC], [CBlue,CRed], [NBlue,NRed]).

minimax(P, [CBlue,CRed], [NBlue,NRed], [R,C,AR,AC]) :-
  opponent(P,Opp),
  possibleMoves(P, [CBlue, CRed], Moves),
  findMinimums(P, Opp, [CBlue, CRed], Moves, List),
  minimumsToList(List, ListOfMins),
  max_member(MaxOutOfTheMins, ListOfMins),
  extractMinimumMoveFromList(MaxOutOfTheMins, List, [R,C,AR,AC]),
  executeMove([R,C,AR,AC], [CBlue,CRed], [NBlue,NRed]).


%%%%%%%%% Helper Functions %%%%%%%%%

opponent(r,b).
opponent(b,r).

%Finding the minimums based on current possible moves.

findMinimums(P, Opp, [CBlue, CRed], Moves, List) :-
  findall(
    [MinDiff,NMove],
    (
      member(NMove, Moves),
      executeMove(NMove, [CBlue, CRed],TempBoard),
      next_generation(TempBoard,[TBlue, TRed]),
      possibleMoves(Opp, [TBlue, TRed], OppMoves),
      futureBoard([TBlue,TRed], OppMoves, AOppList),
      differenceInPieces(P, AOppList, DListWMoves),
      differencesToList(DListWMoves, DList),
      min_member(MinDiff, DList)
    ),
    List).

%Counts the difference between player and opposing
%player according to move

differenceInPieces(_,[],[]).

differenceInPieces(b,[[BP,RP,Moves]|T],[D,Moves|ST]) :-
  D is BP - RP,
  differenceInPieces(b,T,ST).

differenceInPieces(r,[[BP,RP,Moves]|T],[D,Moves|ST]) :-
  D is RP - BP,
  differenceInPieces(r,T,ST).


%Returns a list of [Number of blue pieces after crank,
%Number of red pieces after crank, move]

futureBoard([CBlue, CRed], Moves, List) :-
  findall(
    [BluePieces,RedPieces,[R1,C1,R2,C2]],
    (
      member([R1,C1,R2,C2], Moves),
      executeMove([R1,C1,R2,C2], [CBlue,CRed], TempBoard),
      next_generation(TempBoard,[AfterCrankBlue, AfterCrankRed]),
      length(AfterCrankBlue, BluePieces),
      length(AfterCrankRed, RedPieces)
    ),
    List).


%Pre: Minimum exists for a move
%Return the move that leads to the minimum

extractMinimumMoveFromList(D, [[D,Move]|_], Move).

extractMinimumMoveFromList(D ,[_|T], M) :-
  extractMinimumMoveFromList(D, T, M).

%Pre: Difference exists for a move
%Return the move that leads to the difference

extractDifferenceMoveFromList(D ,[D,Move|_], Move).

extractDifferenceMoveFromList(D ,[_|T], M) :-
  extractDifferenceMoveFromList(D, T, M).

%Pre: Move exist for a piece count.
%Return the move that leads to the given count.

extractMoveFromList(r, RP ,[[_,RP,Move]|_], Move).
extractMoveFromList(b, BP ,[[BP,_,Move]|_], Move).

extractMoveFromList(P, N ,[_|T], M) :-
  extractMoveFromList(P, N, T, M).

%Extract out number of pieces into a list based on
%the colour (r,b)

piecesToList(_,[],[]).

piecesToList(b,[[BP,_,_]|T], ListOfBluePieces) :-
  append([BP], NewList, ListOfBluePieces),
  piecesToList(b,T,NewList).

piecesToList(r,[[_,RP,_]|T], ListOfRedPieces) :-
  append([RP], NewList, ListOfRedPieces),
  piecesToList(r,T,NewList).

%Extract out the differences into a list

differencesToList([], []).
differencesToList([D,_|T], List) :-
  append([D], NewList, List),
  differencesToList(T,NewList).

%Extract out the minimums into a list (minimax)

minimumsToList([], []).
minimumsToList([[D,_]|T], List) :-
  append([D], NewList, List),
  minimumsToList(T,NewList).

%Returns the number of occurrence of the Term in the List

numberOfOccurrenceInList(Term, List, TCount) :-
  numberOfOccurrenceInList(Term, List, 0, TCount).

numberOfOccurrenceInList(_, [], TC, TC).
numberOfOccurrenceInList(Term, [Term | T], Count , TCount) :-
  NCount is Count+1,
  numberOfOccurrenceInList(Term, T, NCount, TCount).
numberOfOccurrenceInList(Term, [_|T], Count, TCount) :-
  numberOfOccurrenceInList(Term, T, Count, TCount).

% Returns all possible moves of a player from the current board
  
possibleMoves(P,Board,Moves) :-
  findall(
    [R1,C1,R2,C2],
    (
     cell(R1,C1),
     neighbour_position(R1,C1,[R2,C2]),
     what_in_cell(Board,R1,C1,P),
     what_in_cell(Board,R2,C2,' ')
    ),
    Moves).

% Returns the board after moving the piece

executeMove([R1,C1,R2,C2],[CBlue,CRed], [NBlue, NRed]) :-
  (
    member([R1,C1],CBlue) -> 
    delete(CBlue,[R1,C1],B),
    append([[R2,C2]],B,NBlue),
    CRed = NRed
  ;
    member([R1,C1],CRed) ->
    delete(CRed,[R1,C1],R),
    append([[R2,C2]],R,NRed),
    CBlue = NBlue
  ).


