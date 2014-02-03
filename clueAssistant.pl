%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              ASSISTANT FOR BOARD GAME "CLUE"               %
%                       Rodrigo Alves                        %
%                       Kevin Miller                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                            %
%       PUBLIC FUNCTIONS (TO BE USED BY THE PLAYER)          %
%                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialize the application.
init :- canInitialize,
	write('How many players are playing? [3 to 6]'), nl,
	write('Example: Type "4."'), nl,
	read(PlayerCount),
	buildClassic(PlayerCount),
	write('Remember when asked: you are player number 1.'), nl,
	write('The player at your left is player number 2 and so on.'), nl,
	write('You should always type a dot after the number chosen.'), nl,
	write('In case you get an error, type "menu." to come back.'), nl,
	assert(actionMade(0, _, _, _, _)), % keep track of stack of actions.
	assert(initialized),
	menu.
	
% Call the main menu.
menu :- canCallMenu, nl, !,
	nl, nl, write('Choose an action:'), nl,
	write('1 - Someone made a guess.'), nl,
	write('2 - I know someone has a card.'), nl,
	write('3 - What should I guess?'), nl,
	write('4 - Undo action: '), printUndoWhat, nl,
	write('5 - What do we know so far?'), nl,
	write('9 - Close menu.'), nl,
	read(Choice),
	menuChoice(Choice).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                            %
%                       GUESS FUNCTION                       %
%                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Given the cards that a player can have, determine the inverse probability
% (1 - probability) that this player has a specific card.
cardInverseProb(Cards, Card, 1.0) :- not(member(Card, Cards)).
cardInverseProb(Cards, Card, Inv) :-
	member(Card, Cards),
	size(Cards, Size), minCards(Min), Probability is Min / Size,
	Inv is 1 - Probability.
	
envelopeCard(Card, Prob, [P1C, P2C, P3C, P4C, P5C, P6C]) :-
	cardInverseProb(P1C, Card, P1),
	Prob1 is 1.0 * P1,
	cardInverseProb(P2C, Card, P2),
	Prob2 is Prob1 * P2,
	cardInverseProb(P3C, Card, P3),
	Prob3 is Prob2 * P3,
	cardInverseProb(P4C, Card, P4),
	Prob4 is Prob3 * P4,
	cardInverseProb(P5C, Card, P5),
	Prob5 is Prob4 * P5,
	cardInverseProb(P6C, Card, P6),
	Prob is Prob5 * P6.
	
envelopeWeapon(Card, Prob, PCards) :- 
	weapon(Card), not(somePlayerHas(Card)), envelopeCard(Card, Prob, PCards).
envelopeSuspect(Card, Prob, PCards) :- 
	suspect(Card), not(somePlayerHas(Card)), envelopeCard(Card, Prob, PCards).
envelopeRoom(Card, Prob, PCards) :- 
	room(Card), not(somePlayerHas(Card)), envelopeCard(Card, Prob, PCards).

findProbability(_, 1.0, 1.0).
findProbability(AllProbs, CardProb, 0.0) :-
	CardProb < 1.0,
	member(1.0, AllProbs).
findProbability(AllProbs, CardProb, Prob) :-
	CardProb < 1.0,
	not(member(1.0, AllProbs)),
	sum(AllProbs, Sum),
	Prob is CardProb / Sum.

weaponProbability(Weapon, Prob, PCards, Weapons) :-
	envelopeWeapon(Weapon, P2, PCards),
	findProbability(Weapons, P2, Prob).

roomProbability(Room, Prob, PCards, Rooms) :-
	envelopeRoom(Room, P2, PCards),
	findProbability(Rooms, P2, Prob).
	
suspectProbability(Suspect, Prob, PCards, Suspects) :-
	envelopeSuspect(Suspect, P2, PCards),
	findProbability(Suspects, P2, Prob).

possibleEnvelope(Weapon, Room, Suspect, Prob) :-
	possibleCards(1, P1C),
	possibleCards(2, P2C),
	possibleCards(3, P3C),
	possibleCards(4, P4C),
	possibleCards(5, P5C),
	possibleCards(6, P6C),
	PCards = [P1C, P2C, P3C, P4C, P5C, P6C],
	findall(P1, envelopeWeapon(_, P1, PCards), Weapons),
	findall(P2, envelopeRoom(_, P2, PCards), Rooms),
	findall(P3, envelopeSuspect(_, P3, PCards), Suspects),
	weaponProbability(Weapon, WProb, PCards, Weapons),
	suspectProbability(Suspect, SProb, PCards, Suspects),
	roomProbability(Room, RProb, PCards, Rooms),
	WRProb is WProb * RProb,
	Prob is WRProb * SProb.

possibleEnvelopeL([Prob, Weapon, Room, Suspect]) :-
	possibleEnvelope(Weapon, Room, Suspect, Prob).	

guess :-
	write('Computing guesses... Please wait. '), nl,
	findall(Envelope, possibleEnvelopeL(Envelope), Envelopes),
	removeLowProb(Envelopes, DecentEnvelopes),
	size(DecentEnvelopes, L),
	write('There are '), write(L), write(' good guesses to make. '),
	write('Top guesses: '), nl,
	mergesort(DecentEnvelopes, SortedEnvelopes),
	top(40, SortedEnvelopes, TopEnvelopes),
	printAllGuesses(TopEnvelopes).


% Remove all envelopes that have a really low probability:
removeLowProb([], []).
removeLowProb([H|T1], [H|T2]) :- 
	extractValue(H, P), P > 0.002,
	removeLowProb(T1, T2).
removeLowProb([H|T1], List) :- 
	extractValue(H, P), P =< 0.002,
	removeLowProb(T1, List).

printAllGuesses([]).
printAllGuesses([H|T]) :- printGuessL(H), printAllGuesses(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                            %
%                       RELEVANT LOGIC                       %
%                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cannotHaveSuspect(Player, Suspect) :- 
	suspect(Suspect), hasSuspect(Other, Suspect), Other =\= Player.
cannotHaveSuspect(Player, Suspect) :- 
	suspect(Suspect), cantHaveSuspect(Player, Suspect).

cannotHaveWeapon(Player, Weapon) :- 
	weapon(Weapon), hasWeapon(Other, Weapon), Other =\= Player.
cannotHaveWeapon(Player, Weapon) :- 
	weapon(Weapon), cantHaveWeapon(Player, Weapon).

cannotHaveRoom(Player, Room) :- 
	room(Room), hasRoom(Other, Room), Other =\= Player.
cannotHaveRoom(Player, Room) :- 
	room(Room), cantHaveRoom(Player, Room).

cannotHave(Player, _) :- totalPlayers(Players), Player > Players.
cannotHave(Player, Suspect) :- cannotHaveSuspect(Player, Suspect).
cannotHave(Player, Weapon) :- cannotHaveWeapon(Player, Weapon).
cannotHave(Player, Room) :- cannotHaveRoom(Player, Room).


has(Player, Suspect) :- suspect(Suspect), hasSuspect(Player, Suspect).
has(Player, Suspect) :- 
	suspect(Suspect),
	hasOne(Player, Suspect, Weapon, Room),
	cannotHaveWeapon(Player, Weapon),
	cannotHaveRoom(Player, Room).
has(Player, Weapon) :- weapon(Weapon), hasWeapon(Player, Weapon).
has(Player, Weapon) :- 
	weapon(Weapon),
	hasOne(Player, Suspect, Weapon, Room),
	cannotHaveSuspect(Player, Suspect),
	cannotHaveRoom(Player, Room).
has(Player, Room) :- room(Room), hasRoom(Player, Room).
has(Player, Room) :- 
	room(Room),
	hasOne(Player, Suspect, Weapon, Room),
	cannotHaveSuspect(Player, Suspect),
	cannotHaveWeapon(Player, Weapon).

cantHave(Player, Card) :- cannotHave(Player, Card).
cantHave(Player, Card) :- has(Other, Card), Other =\= Player.

cantHave(_, Card) :-
	suspect(Card),
	suspects(Cards),
	remove_element(Card, Cards, OtherCards),
	playersHaveAll(OtherCards).

cantHave(_, Card) :-
	weapon(Card),
	weapons(Cards),
	remove_element(Card, Cards, OtherCards),
	playersHaveAll(OtherCards).

cantHave(_, Card) :-
	room(Card),
	rooms(Cards),
	remove_element(Card, Cards, OtherCards),
	playersHaveAll(OtherCards).

playersHaveAll([]).
playersHaveAll([Card | Deck]) :-
	somePlayerHas(Card), playersHaveAll(Deck).

somePlayerHas(Card) :- has(1, Card).
somePlayerHas(Card) :- has(2, Card).
somePlayerHas(Card) :- has(3, Card).
somePlayerHas(Card) :- player(4), has(4, Card).
somePlayerHas(Card) :- player(5), has(5, Card).
somePlayerHas(Card) :- player(6), has(6, Card).

mightHave(Player, Card) :- 
	totalPlayers(Players), possiblePlayer(Player),
	Player =< Players, card(Card), 
	not(cantHave(Player, Card)).
	
mustNotHave(Player, Card) :- cantHave(Player, Card).
mustNotHave(Player, Card) :- 
	minCards(MinCards),
	findall(C, mightHave(P, C), Cards),
	not(P is Player),
	size(Cards, Length), 
	Length =< MinCards, % We know already all the cards a player can have.
	member(Card, Cards). % That means other players can't have those cards.

canHave(Player, Card) :- card(Card), not(mustNotHave(Player, Card)).
possibleCards(Player, Cards) :- findall(Card, canHave(Player, Card), Cards).

% Player couldn't refute someone's claim:
cantRefute(PlayerNum, Suspect, Weapon, Room) :- 
	validateParams(PlayerNum, Suspect, Weapon, Room),
	storeIfDoesNotExist(1, PlayerNum, Suspect, Weapon, Room).

% The minimum number of cards each player can have:	
minCards(Min) :-
	totalPlayers(Players), 
	Min is 18//Players.

storeIfDoesNotExist(Action, PlayerNum, Suspect, Weapon, Room) :-
	actionMade(Action, PlayerNum, Suspect, Weapon, Room).
storeIfDoesNotExist(Action, PlayerNum, Suspect, Weapon, Room) :-
	not(actionMade(Action, PlayerNum, Suspect, Weapon, Room)),
	store(Action, PlayerNum, Suspect, Weapon, Room).

store(1, PlayerNum, Suspect, Weapon, Room) :-
	assert(cantHaveSuspect(PlayerNum, Suspect)),
	assert(cantHaveWeapon(PlayerNum, Weapon)),
	assert(cantHaveRoom(PlayerNum, Room)),
	logCantRefute(PlayerNum, Suspect, Weapon, Room).
	
store(2, PlayerNum, Suspect, Weapon, Room) :-
	assert(hasOne(PlayerNum, Suspect, Weapon, Room)),
	logHasOne(PlayerNum, Suspect, Weapon, Room).
	
store(3, PlayerNum, Suspect, _, _) :-
	assert(hasSuspect(PlayerNum, Suspect)),
	logHasSuspect(PlayerNum, Suspect).
store(4, PlayerNum, _, Weapon, _) :-
	assert(hasWeapon(PlayerNum, Weapon)),
	logHasWeapon(PlayerNum, Weapon).
store(5, PlayerNum, _, _, Room) :-
	assert(hasRoom(PlayerNum, Room)),
	logHasRoom(PlayerNum, Room).

% Player has a specific card in hands:
hasCard(suspect, PlayerNum, Suspect) :-
	storeIfDoesNotExist(3, PlayerNum, Suspect, 0, 0).
hasCard(weapon, PlayerNum, Weapon) :- 
	storeIfDoesNotExist(4, PlayerNum, 0, Weapon, 0).
hasCard(room, PlayerNum, Room) :-
	storeIfDoesNotExist(5, PlayerNum, 0, 0, Room).

% Return a list of all cards in game:
cards(Cards) :- findall(Card, card(Card), Cards).
suspects(Suspects) :- findall(Suspect, suspect(Suspect), Suspects).
weapons(Weapons) :- findall(Weapon, weapon(Weapon), Weapons).
rooms(Rooms) :- findall(Room, room(Room), Rooms).

guessMade(PlayerWhoGuessed, Suspect, Weapon, Room, true) :-	
	write('Who refuted the guess? '),
	getPlayer(PlayerWhoRefuted),
	guessMadeHelper(PlayerWhoGuessed, PlayerWhoRefuted, Suspect, Weapon, Room).

guessMade(PlayerWhoGuessed, Suspect, Weapon, Room, false) :-
	nextPlayer(PlayerWhoGuessed, NextPlayer),
	guessCircle(NextPlayer, PlayerWhoGuessed, Suspect, Weapon, Room).
	
guessMadeHelper(PlayerWhoGuessed, PlayerWhoRefuted, _, _, _) :-
	samePlayer(PlayerWhoGuessed, PlayerWhoRefuted),
	write('Error: they cannot be the same player!'), nl.

guessMadeHelper(PlayerWhoGuessed, PlayerWhoRefuted, Suspect, Weapon, Room) :-
	not(samePlayer(PlayerWhoGuessed, PlayerWhoRefuted)),	
	nextPlayer(PlayerWhoGuessed, NextPlayer),
	guessCircle(NextPlayer, PlayerWhoRefuted, Suspect, Weapon, Room),
	storeIfDoesNotExist(2, PlayerWhoRefuted, Suspect, Weapon, Room). % has one



guessCircle(Player, Player, _, _, _). % Circle ended
guessCircle(PStart, PEnd, Suspect, Weapon, Room) :-
	PStart =\= PEnd,
	nextPlayer(PStart, NextPlayer),
	guessCircle(NextPlayer, PEnd, Suspect, Weapon, Room),
	cantRefute(PStart, Suspect, Weapon, Room).	

% Get the next player in the circle:
nextPlayer(Player, 1) :- totalPlayers(Total), Total =:= Player.
nextPlayer(P, Next) :- totalPlayers(Total), Total > P, Next is P+1.

samePlayer(P1, P2) :- P1 =:= P2.

% Print in the screen everything we know so far: */
printDatabase :-
	write('Number of players playing: '), 
	totalPlayers(Num), printNum(Num), nl,
	write('All game suspects: '),
	suspects(Suspects), printSuspectList(Suspects),
	write('All game weapons: '),
	weapons(Weapons), printWeaponList(Weapons),
	write('All game rooms: '),
	rooms(Rooms), printRoomList(Rooms),
	nl, write('What do we know about the players?'), nl, nl,
	playerData(1), playerData(2), playerData(3),
	playerData(4), playerData(5), playerData(6).

% Print everything we know about a player:	
playerData(PlayerNum) :- totalPlayers(Total), PlayerNum > Total.
playerData(PlayerNum) :- 
	totalPlayers(Total), PlayerNum =< Total,
	playerSuspects(PlayerNum),
	playerWeapons(PlayerNum),
	playerRooms(PlayerNum),
	findall(PlayerNum, printHasOneWithNl(PlayerNum), _), nl,
	playerCards(PlayerNum), nl.

printHasOneWithNl(PlayerNum) :- printHasOne(PlayerNum), nl.

playerSuspects(PlayerNum) :-
	write('Player '), printNum(PlayerNum), write(' has suspects: '),	
	findall(Card, hasSuspect(PlayerNum, Card), Cards),
	printSuspectList(Cards),
	write('Player '), printNum(PlayerNum), write(' cannot have suspects: '),	
	findall(Suspect, cantHaveSuspect(PlayerNum, Suspect), Suspects),
	printSuspectList(Suspects).
	
playerWeapons(PlayerNum) :-
	write('Player '), printNum(PlayerNum), write(' has weapons: '),	
	findall(Card, hasWeapon(PlayerNum, Card), Cards),
	printWeaponList(Cards),
	write('Player '), printNum(PlayerNum), write(' cannot have weapons: '),	
	findall(Weapon, cantHaveWeapon(PlayerNum, Weapon), Weapons),
	printWeaponList(Weapons).
	
playerRooms(PlayerNum) :-
	write('Player '), printNum(PlayerNum), write(' has rooms: '),	
	findall(Card, hasRoom(PlayerNum, Card), Cards),
	printRoomList(Cards),
	write('Player '), printNum(PlayerNum), write(' cannot have rooms: '),	
	findall(Room, cantHaveRoom(PlayerNum, Room), Rooms),
	printRoomList(Rooms).
	
playerCards(PlayerNum) :- 
	write('A system investigation determined that player '), printNum(PlayerNum), 
	write(' should have at least '), totalPlayers(Players), 
	MinCards is (18//Players),
	printNum(MinCards),
	write(' of those cards: '),
	possibleCards(PlayerNum, Cards),
	printCardList(Cards).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                            %
%                             MENU                           %
%                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Someone made a guess:
menuChoice(1) :- 
	write('Who is making the guess? '),
	getPlayer(PlayerWhoGuessed),	
	getSuspect(Suspect),
	getWeapon(Weapon),
	getRoom(Room),
	write('Did anyone refute the guess? '),
	getBoolean(Refuted),
	guessMade(PlayerWhoGuessed, Suspect, Weapon, Room, Refuted),
	menu.

% I know someone has a card:
menuChoice(2) :-
	write('Who has the card? '),
	getPlayer(PlayerNum),	
	getCardType(CardType),
	getCardBasedOnType(CardType, Card),
	hasCard(CardType, PlayerNum, Card),
	menu.

% What should I guess?
menuChoice(3) :- nl, guess, !, menu.

% Undo last move:
menuChoice(4) :- undoLast, menu.

% What do we know?
menuChoice(5) :- printDatabase, menu.
	
% Quit:
menuChoice(9) :- write('Menu closed. Type "menu." to open it again.'), !.

% Invalid choice: 
menuChoice(X) :- not(isDigit(X)),
	write('Wrong action. You must type a digit.'),
	menu.
menuChoice(X) :- isDigit(X), X =\= 9, X =\= 1, X =\= 2, X =\= 3, X =\= 4, X =\= 5,
	write('Wrong action. Please choose a valid number.'),
	menu.

% Return the Suspect atom chosen by the user.	
getSuspect(Suspect) :-
	write('Suspects available: '), nl,
	write('1 - '), printSuspectName(mustard), nl,
	write('2 - '), printSuspectName(scarlet), nl,
	write('3 - '), printSuspectName(plum), nl,
	write('4 - '), printSuspectName(green), nl,
	write('5 - '), printSuspectName(white), nl,
	write('6 - '), printSuspectName(peacock), nl,
	write('Type the suspect number: '), nl,
	read(SuspectNum),
	getSuspectBasedFromNumber(SuspectNum, Suspect).
	
% Return the Weapon atom chosen by the user.	
getWeapon(Weapon) :-
	write('Weapons available: '), nl,
	write('1 - '), printWeaponName(rope), nl,
	write('2 - '), printWeaponName(pipe), nl,
	write('3 - '), printWeaponName(knife), nl,
	write('4 - '), printWeaponName(wrench), nl,
	write('5 - '), printWeaponName(candlestick), nl,
	write('6 - '), printWeaponName(pistol), nl,
	write('Type the weapon number: '), nl,
	read(WeaponNum),
	getWeaponBasedFromNumber(WeaponNum, Weapon).
	
% Return the Room atom chosen by the user.	
getRoom(Room) :-
	write('Rooms available: '), nl,
	write('1 - '), printRoomName(kitchen), nl,
	write('2 - '), printRoomName(ballroom), nl,
	write('3 - '), printRoomName(conservatory), nl,
	write('4 - '), printRoomName(dining), nl,
	write('5 - '), printRoomName(lounge), nl,
	write('6 - '), printRoomName(hall), nl,
	write('7 - '), printRoomName(study), nl,
	write('8 - '), printRoomName(library), nl,
	write('9 - '), printRoomName(billiard), nl,
	write('Type the room number: '), nl,
	read(RoomNum),
	getRoomBasedFromNumber(RoomNum, Room).

% Return the player number chosen by the user:
getPlayer(Player) :-
	write('Type the player number: '), nl,
	read(PlayerNum),
	getPlayerBasedFromNumber(PlayerNum, Player).

% Return true of false based on what the player chose:
getBoolean(Bool) :-
	write('Type 1 if YES, 2 if NO:'), nl,
	read(BoolNum),
	getBoolBasedFromNumber(BoolNum, Bool).

% Return the type of card chosen by the user:	
getCardType(Type) :-
	write('Card type: '), nl,
	write('1 - Suspect'), nl,
	write('2 - Weapon'), nl,
	write('3 - Room'), nl,	
	write('Type the number of the card type: '), nl,
	read(TypeNum),
	getCardTypeFromNumber(TypeNum, Type).

getCardBasedOnType(suspect, Card) :- getSuspect(Card).
getCardBasedOnType(weapon, Card) :- getWeapon(Card).
getCardBasedOnType(room, Card) :- getRoom(Card).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                            %
%                        LOG AND UNDO                        %
%                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logCantRefute(PlayerNum, Suspect, Weapon, Room) :-
	asserta(actionMade(1, PlayerNum, Suspect, Weapon, Room)),
	printCantRefute(PlayerNum, Suspect, Weapon, Room), nl.

printCantRefute(PlayerNum, Suspect, Weapon, Room) :-
	write('Player '), printNum(PlayerNum), write(' cannot refute "'),
	printSuspectRoomWeapon(Suspect, Room, Weapon), write('"').

logHasOne(PlayerNum, Suspect, Weapon, Room) :-
	asserta(actionMade(2, PlayerNum, Suspect, Weapon, Room)),
	printHasOne(PlayerNum, Suspect, Weapon, Room).

printHasOne(PlayerNum, Suspect, Weapon, Room) :-
	write('Player '), printNum(PlayerNum), write(' has '),
	printSuspectName(Suspect), write(' OR '),
	printRoomName(Room), write(' OR '),
	printWeaponName(Weapon), write('.').
	
logHasSuspect(PlayerNum, Suspect) :-
	asserta(actionMade(3, PlayerNum, Suspect, 0, 0)),
	printHasSuspect(PlayerNum, Suspect), nl.

printHasSuspect(PlayerNum, Suspect) :-
	write('Player '), printNum(PlayerNum), write(' has suspect "'),
	printSuspectName(Suspect), write('".').
	
logHasWeapon(PlayerNum, Weapon) :-
	asserta(actionMade(4, PlayerNum, 0, Weapon, 0)),
	printHasWeapon(PlayerNum, Weapon), nl.

printHasWeapon(PlayerNum, Weapon) :-
	write('Player '), printNum(PlayerNum), write(' has weapon "'),
	printWeaponName(Weapon), write('".').
	
logHasRoom(PlayerNum, Room) :-
	asserta(actionMade(5, PlayerNum, 0, 0, Room)),
	printHasRoom(PlayerNum, Room), nl.
	
printHasRoom(PlayerNum, Room) :-
	write('Player '), printNum(PlayerNum), write(' has room "'),
	printRoomName(Room), write('".').

printUndoWhat :-
	actionMade(Action, Player, Suspect, Weapon, Room), !,
	printUndoWhatHelper(Action, Player, Suspect, Weapon, Room).

printUndoWhatHelper(0, _, _, _, _) :- write('Nothing to undo.').
printUndoWhatHelper(1, PlayerNum, Suspect, Weapon, Room) :- 
	printCantRefute(PlayerNum, Suspect, Weapon, Room).
printUndoWhatHelper(2, PlayerNum, Suspect, Weapon, Room) :- 
	printHasOne(PlayerNum, Suspect, Weapon, Room).
printUndoWhatHelper(3, PlayerNum, Suspect, _, _) :- 
	printHasSuspect(PlayerNum, Suspect).
printUndoWhatHelper(4, PlayerNum, _, Weapon, _) :- 
	printHasWeapon(PlayerNum, Weapon).
printUndoWhatHelper(5, PlayerNum, _, _, Room) :- 
	printHasRoom(PlayerNum, Room).
	
undoLast :-
	write('Removed: '), printUndoWhat, nl,
	actionMade(Action, Player, Suspect, Weapon, Room), !,
	undoAction(Action, Player, Suspect, Weapon, Room).
	
undoAction(0, _, _, _, _).
undoAction(Action, Player, Suspect, Weapon, Room) :-
	Action > 0,
	retract(actionMade(Action, Player, Suspect, Weapon, Room)), 
	undoHelper(Action, Player, Suspect, Weapon, Room), !. 
	
undoHelper(1, PlayerNum, Suspect, Weapon, Room) :-
	retract(cantHaveSuspect(PlayerNum, Suspect)),
	retract(cantHaveWeapon(PlayerNum, Weapon)),
	retract(cantHaveRoom(PlayerNum, Room)).
	
undoHelper(2, PlayerNum, Suspect, Weapon, Room) :-
	retract(hasOne(PlayerNum, Suspect, Weapon, Room)).
	
undoHelper(3, PlayerNum, Suspect, _, _) :-
	retract(hasSuspect(PlayerNum, Suspect)).
	
undoHelper(4, PlayerNum, _, Weapon, _) :-
	retract(hasWeapon(PlayerNum, Weapon)).
	
undoHelper(5, PlayerNum, _, _, Room) :-
	retract(hasRoom(PlayerNum, Room)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                            %
%                      DYNAMIC CLAUSES                       %
%                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic weapon/1.
:- dynamic room/1.
:- dynamic suspect/1.
:- dynamic totalPlayers/1.
:- dynamic cantHaveSuspect/2.
:- dynamic cantHaveWeapon/2.
:- dynamic cantHaveRoom/2.
:- dynamic cantHaveRoom/2.
:- dynamic hasSuspect/2.
:- dynamic hasWeapon/2.
:- dynamic hasRoom/2.
:- dynamic hasOne/4. 
:- dynamic initialized/0.
:- dynamic actionMade/5.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                            %
%                          HELPERS                           %
%                                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_element(_, [], []).
remove_element(X, [X|T1], T1).
remove_element(X, [H1|T1], [H1|T2]) :- X \= H1, remove_element(X, T1, T2).

mergesort([],[]).
mergesort([X],[X]).
mergesort([H1, H2 | T], SortedList) :-
    	split([H1, H2 | T], Half1, Half2),
	mergesort(Half1, SortedHalf1), mergesort(Half2, SortedHalf2), 
	myMerge(SortedHalf1, SortedHalf2, SortedList).

myMerge(L1,[],L1).
myMerge([],L2,L2).
myMerge([H1|T1],[H2|T2],[H2|T3]) :- 
	extractValue(H1, Prob1), extractValue(H2, Prob2), 
	Prob1 < Prob2, !, 
	myMerge([H1|T1],T2,T3).
myMerge([H1|T1],[H2|T2],[H1,H2|T3]) :- 
	extractValue(H1, Prob1), extractValue(H2, Prob2), 
	Prob1 =:= Prob2, !, myMerge(T1,T2,T3).
myMerge([H1|T1],[H2|T2],[H1|T3]) :- 
	extractValue(H1, Prob1), extractValue(H2, Prob2), 
	Prob1 > Prob2, !, myMerge(T1,[H2|T2],T3).
	
% Split a list into 2 halves.
split([],[],[]).
split([X],[X],[]).
split([H1,H2|T],[H1|T1],[H2|T2]) :- split(T,T1,T2).

% Extract the first value of a list:
extractValue([V | _], V).

% Return the first N elements of a list
top(N, L, L) :- size(L, Size), Size =< N.
top(N, L1, L2) :- size(L1, Size), Size > N, topHelper(N, L1, L2).
topHelper(0, _, []).
topHelper(N, [H|T1], [H|T2]) :- N > 0, N1 is N-1, top(N1, T1, T2). 

isDigit(0).
isDigit(1).
isDigit(2).
isDigit(3).
isDigit(4).
isDigit(5).
isDigit(6).
isDigit(7).
isDigit(8).
isDigit(9).

% Compute the sum of a list
sum([], 0.0).
sum([H|T], Sum) :- sum(T, S), Sum is H + S.

size([], 0). % Return the length of the list
size([_|T], N) :- size(T, X), N is X + 1.

% X is a member of list Y
member(X, Y) :- append(_, [X|_], Y).

append([],X,X). % append(Array1, Array2, Result)
append([H1|T1],X,[H1|T2]) :- append(T1,X,T2). 

% Make sure the application wasn't initialized yet:
canInitialize :- 
	initialized,
	write('The application was already initialized. Call "menu" to return.'),
	nl, false.
canInitialize :- not(initialized).

% Make sure the application was already initialized:	
canCallMenu :- 
	not(initialized),
	write('The application must be initialized first. Call "init."'),
	nl, false.
canCallMenu :- initialized.
	

% Print a guess based on the parameters
printGuess(Prob, Suspect, Weapon, Room) :-
	write('[Chance: '), P is Prob * 100, format('~2f', [P]), write('%] '), 
	printSuspectRoomWeapon(Suspect, Room, Weapon), nl.
	
printGuessL([Prob, Weapon, Room, Suspect]) :-
	printGuess(Prob, Suspect, Weapon, Room).

printSuspectRoomWeapon(Suspect, Room, Weapon) :-
	printSuspectName(Suspect), write(' in the '),
	printRoomName(Room), write(' with the '),
	printWeaponName(Weapon), write('.').
	
printSuspectList([]) :- write('[No info]'), nl.
printSuspectList([X]) :- !, printSuspectName(X), write('.'), nl.
printSuspectList([X|T]) :- printSuspectName(X), write(', '), printSuspectList(T).

printRoomList([]) :- write('[No info]'), nl.
printRoomList([X]) :- !, printRoomName(X), write('.'), nl.
printRoomList([X|T]) :- printRoomName(X), write(', '), printRoomList(T).

printWeaponList([]) :- write('[No info]'), nl.
printWeaponList([X]) :- !, printWeaponName(X), write('.'), nl.
printWeaponList([X|T]) :- printWeaponName(X), write(', '), printWeaponList(T).

printCardList([]) :- write('[No cards]'), nl.
printCardList([X]) :- !, printCardName(X), write('.'), nl.
printCardList([X|T]) :- printCardName(X), write(', '), printCardList(T).

printCardName(Card) :- suspect(Card), printSuspectName(Card).
printCardName(Card) :- weapon(Card), printWeaponName(Card).
printCardName(Card) :- room(Card), printRoomName(Card).
	
getSuspectBasedFromNumber(1, mustard).
getSuspectBasedFromNumber(2, scarlet).
getSuspectBasedFromNumber(3, plum).
getSuspectBasedFromNumber(4, green).
getSuspectBasedFromNumber(5, white).
getSuspectBasedFromNumber(6, peacock).
getSuspectBasedFromNumber(X, Suspect) :- isDigit(X),
	X =\= 1, X =\= 2, X =\= 3, X =\= 4, X =\= 5, X =\= 6,
	write('Wrong suspect number! Try again.'), nl, 
	getSuspect(Suspect).
getSuspectBasedFromNumber(X, Suspect) :- not(isDigit(X)),
	write('Input must be a digit! Try again.'), nl,
	getSuspect(Suspect).
	
getWeaponBasedFromNumber(1, rope).
getWeaponBasedFromNumber(2, pipe).
getWeaponBasedFromNumber(3, knife).
getWeaponBasedFromNumber(4, wrench).
getWeaponBasedFromNumber(5, candlestick).
getWeaponBasedFromNumber(6, pistol).
getWeaponBasedFromNumber(X, Weapon) :- isDigit(X),
	X =\= 1, X =\= 2, X =\= 3, X =\= 4, X =\= 5, X =\= 6,
	write('Wrong weapon number! Try again.'), nl, 
	getWeapon(Weapon).
getWeaponBasedFromNumber(X, Weapon) :- not(isDigit(X)),
	write('Input must be a digit! Try again.'), nl,
	getWeapon(Weapon).
	
getRoomBasedFromNumber(1, kitchen).
getRoomBasedFromNumber(2, ballroom).
getRoomBasedFromNumber(3, conservatory).
getRoomBasedFromNumber(4, dining).
getRoomBasedFromNumber(5, lounge).
getRoomBasedFromNumber(6, hall).
getRoomBasedFromNumber(7, study).
getRoomBasedFromNumber(8, library).
getRoomBasedFromNumber(9, billiard).
getRoomBasedFromNumber(X, Room) :- isDigit(X),
	X =\= 1, X =\= 2, X =\= 3, X =\= 4, X =\= 5, X =\= 6, 
	X =\= 7, X =\= 8, X =\= 9,
	write('Wrong room number! Try again.'), nl, 
	getRoom(Room).
getRoomBasedFromNumber(X, Room) :- not(isDigit(X)),
	write('Input must be a digit! Try again.'), nl, 
	getRoom(Room).

getPlayerBasedFromNumber(PlayerNum, PlayerNum) :- player(PlayerNum).
getPlayerBasedFromNumber(PlayerNum, Player) :-	
	not(player(PlayerNum)), 
	totalPlayers(Total),
	write('Wrong player number! The number goes from one to '),
	printNum(Total), write('. Try again.'), nl,
	getPlayer(Player).
	
getBoolBasedFromNumber(1, true).
getBoolBasedFromNumber(2, false).
getBoolBasedFromNumber(X, Bool) :- isDigit(X),
	X =\= 1, X =\= 2,
	write('Wrong input. The number should be 1 or 2. Try again.'), nl, 
	getBoolean(Bool).
getBoolBasedFromNumber(X, Bool) :- not(isDigit(X)),
	write('Input must be a digit! Try again.'), nl,
	getBoolean(Bool).

getCardTypeFromNumber(1, suspect).
getCardTypeFromNumber(2, weapon).
getCardTypeFromNumber(3, room).
getCardTypeFromNumber(X, Type) :- isDigit(X),
	X =\= 1, X =\= 2, X =\= 3,
	write('Wrong input. The number should be 1, 2 or 3. Try again.'), nl, 
	getCardType(Type).
getCardTypeFromNumber(X, Type) :- not(isDigit(X)),
	write('Input must be a digit! Try again.'), nl, 
	getCardType(Type).
	
printSuspectName(mustard) :- write('Colonel Mustard'). 
printSuspectName(scarlet) :- write('Miss Scarlet').
printSuspectName(plum) :- write('Professor Plum').
printSuspectName(green) :- write('Mr. Green').
printSuspectName(white) :- write('Mrs. White').
printSuspectName(peacock) :- write('Mrs. Peacock').

printWeaponName(rope) :- write('Rope'). 
printWeaponName(pipe) :- write('Lead Pipe'). 
printWeaponName(knife) :- write('Knife'). 
printWeaponName(wrench) :- write('Wrench'). 
printWeaponName(candlestick) :- write('Candlestick'). 
printWeaponName(pistol) :- write('Pistol'). 
	
printRoomName(kitchen) :- write('Kitchen'). 
printRoomName(ballroom) :- write('Ballroom'). 
printRoomName(conservatory) :- write('Conservatory'). 
printRoomName(dining) :- write('Dining room'). 
printRoomName(lounge) :- write('Lounge'). 
printRoomName(hall) :- write('Hall'). 
printRoomName(study) :- write('Study'). 
printRoomName(library) :- write('Library'). 
printRoomName(billiard) :- write('Billiard Room'). 	

printNum(1) :- write('one').
printNum(2) :- write('two').
printNum(3) :- write('three').
printNum(4) :- write('four').
printNum(5) :- write('five').
printNum(6) :- write('six').

printHasOne(Player) :-
	hasOne(Player, Suspect, Weapon, Room), 
	write('Player '), printNum(Player), write(' has: '),	
	printSuspectName(Suspect), write(' OR '),
	printWeaponName(Weapon), write(' OR '),
	printRoomName(Room), write('.').
	
% Builds the classic game with 9 rooms, 6 suspects, 6 weapon.
% Parameter PlayerCount is the number of players in game.
buildClassic(PlayerCount) :- 
	validatePlayerCount(PlayerCount),
	setPlayerCount(PlayerCount), 
	addClassicSuspects,
	addClassicRooms, 
	addClassicWeapons.
	
% Make sure the number of players is within limits:
validatePlayerCount(PlayerCount) :-
	not(isDigit(PlayerCount)),
	write('Error: you must type a digit. Initialization aborted.'), 
	nl, false.
validatePlayerCount(PlayerCount) :-
	isDigit(PlayerCount),
	PlayerCount > 6, 
	write('Error: Max of 6 players. Initialization aborted.'), 
	nl, false.
validatePlayerCount(PlayerCount) :-
	isDigit(PlayerCount),
	PlayerCount < 3, 
	write('Error: Min of 3 players. Initialization aborted.'), 
	nl, false.
validatePlayerCount(PlayerCount) :- PlayerCount =< 6, PlayerCount >= 3.
	
% Set how many players are playing. If it was already set, remove and set again:
setPlayerCount(PlayerCount) :- retract(totalPlayers(_)), assert(totalPlayers(PlayerCount)).
setPlayerCount(PlayerCount) :- not(totalPlayers(_)), assert(totalPlayers(PlayerCount)).

addWeapon(Weapon) :- assert(weapon(Weapon)).
addRoom(Room) :- assert(room(Room)).
addSuspect(Suspect) :- assert(suspect(Suspect)).

% Make sure input is valid:
validateParams(PlayerNum, Suspect, Weapon, Room) :-
	player(PlayerNum), suspect(Suspect), weapon(Weapon), room(Room).

% Check if the player number is within the range set.
player(PlayerNum) :- 
	totalPlayers(Total),
	possiblePlayer(PlayerNum),
	PlayerNum =< Total.

possiblePlayer(1).
possiblePlayer(2).
possiblePlayer(3).
possiblePlayer(4).
possiblePlayer(5).
possiblePlayer(6).

card(Card) :- suspect(Card).
card(Card) :- weapon(Card).
card(Card) :- room(Card).

addClassicSuspects :-
	addSuspect(mustard),
	addSuspect(scarlet),
	addSuspect(plum),
	addSuspect(green),
	addSuspect(white),
	addSuspect(peacock).
	
addClassicRooms :-
	addRoom(kitchen),
	addRoom(ballroom),
	addRoom(conservatory),
	addRoom(dining),
	addRoom(lounge),
	addRoom(hall),
	addRoom(study),
	addRoom(library),
	addRoom(billiard).

addClassicWeapons :-
	addWeapon(rope),
	addWeapon(pipe),
	addWeapon(knife),
	addWeapon(wrench),
	addWeapon(candlestick),
	addWeapon(pistol).
