/*

Brandon Holmes, Section 2, 500751878
Nya Samahan, Section 2, 500634913
Rafael Natividad, Section 3, 500705909

*/

/***********************************************
                    KB
***********************************************/


block(b01, red, big, cube).
block(b02, yellow, small, pyramid).
block(b03, green, small, cube).
block(b04, orange, medium, cube).
block(b05, pink, huge, cube).
block(b06, blue, medium, pyramid).
block(b07, blue, huge, wedge).
block(b08, blue, big, cube).
block(b09, green, huge, wedge).
block(b10, blue, medium, cube).
block(b11, pink, small, cube).
block(b12, red, small, pyramid).

isBlock(Block) :- block(Block, _, _, _).

colour(Block, Colour) :- block(Block, Colour, _, _).
size(Block, Size) :- block(Block, _, Size, _).
shape(Block, Shape) :- block(Block, _, _, Shape).

isCube(Block) :- shape(Block, cube).
isPyramid(Block) :- shape(Block, pyramid).
isWedge(Block) :- shape(Block, wedge).


locatedOn(b01, area1, []).
    locatedOn(b02, b01, []).
locatedOn(b03, area2, []).
    locatedOn(b04, b03, []).
    locatedOn(b05, b04, []).
    locatedOn(b06, b05, []).
locatedOn(b07, area4, []).
locatedOn(b08, area5, []).
    locatedOn(b09, b08, []).
locatedOn(b10, area6, []).
    locatedOn(b11, b10, []).
    locatedOn(b12, b11, []).

locatedOn(Block, X, S) :-
    S \= [],
    member(locatedOn(Block, X, S), S).
locatedOn(Block, X, S) :-
    S \= [],
    locatedOn(Block, X, []),
    \+ member(locatedOn(Block, _, S), S),
    \+ member(clear(X, S), S).
locatedOn(Block, table, S) :-
    isArea(Area),
    locatedOn(Block, Area, S).

clear(b02, []).
clear(b06, []).
clear(area3, []).
clear(b07, []).
clear(b09, []).
clear(b12, []).

clear(X, S) :-
    S \= [],
    member(clear(X, S), S).
clear(X, S) :-
    S \= [],
    clear(X, []),
    \+ member(clear(X, S), S).

justLeftOf(area1, area2).
justLeftOf(area2, area3).
justLeftOf(area3, area4).
justLeftOf(area4, area5).
justLeftOf(area5, area6).

isArea(Area) :- justLeftOf(Area, _).
isArea(Area) :- justLeftOf(_, Area).


/********************
  Describe the state of the block world
********************/

adjacent(Area1, Area2) :- justLeftOf(Area1, Area2).
adjacent(Area1, Area2) :- justLeftOf(Area2, Area1).

areaOf(Block, Area, S) :- 
    locatedOn(Block, Area, S),
    isArea(Area).
areaOf(Block, Area, S) :- 
    locatedOn(Block, BlockBelow, S),
    isBlock(BlockBelow),
    areaOf(BlockBelow, Area, S).

above(Block, BlockBelow, S) :- 
    locatedOn(Block, BlockBelow, S),
    isBlock(BlockBelow).
above(Block, BlockBelow, S) :- 
    locatedOn(Block,BlockMid, S),
    isBlock(BlockMid),
    above(BlockMid, BlockBelow, S).

beside(X, Y, S) :-
    areaOf(X, AreaX, S),
    areaOf(Y, AreaY, S),
    adjacent(AreaX, AreaY).

areaLeftOf(Area1, Area2) :- 
    justLeftOf(Area1, Area2).
areaLeftOf(Area1, Area2) :-
    justLeftOf(Area1, AreaMid),
    areaLeftOf(AreaMid, Area2).

leftOf(X, Y, S) :- 
    areaOf(X, AreaX, S),
    areaOf(Y, AreaY, S),
    areaLeftOf(AreaX, AreaY).

rightOf(X, Y, S) :- 
    leftOf(Y, X, S).

/**************************
        Helpers
****************************/

appendList([], L, L).
appendList([H|L1], L2, [H|L3]) :-  appendList(L1, L2, L3).

/***********************************************
                    Lexicon
***********************************************/


article(a).
article(an).
article(the).
article(any).

adjective(red, X) :- colour(X, red).
adjective(green, X) :- colour(X, green).
adjective(yellow, X) :- colour(X, yellow).
adjective(blue, X) :- colour(X, blue).
adjective(orange, X) :- colour(X, orange).
adjective(pink, X) :- colour(X, pink).

adjective(small, X) :- size(X, small).
adjective(medium, X) :- size(X, medium).
adjective(big, X) :- size(X, big).
adjective(huge, X) :- size(X, huge).

common_noun(table, table).
common_noun(area, X) :- isArea(X).
common_noun(block, X) :- isBlock(X).
common_noun(cube, X) :- shape(X, cube).
common_noun(pyramid, X) :- shape(X, pyramid).
common_noun(wedge, X) :- shape(X, wedge).

/********************
    Add prepositons
********************/

preposition(on, X, Y, S) :- locatedOn(X, Y, S).
preposition(above, X, Y, S) :- above(X, Y, S).
preposition(below, X, Y, S) :- above(Y, X, S).
preposition(beside, X, Y, S) :- beside(X, Y, S).

preposition(leftOf, X, Y, S) :- leftOf(X, Y, S).


what(Phrase, B, Actions) :- 
    Actions = [putOn(_, _)|_],
    processActions(Actions, S),
    parse(Phrase, _, _, B, S, true).

what(Phrase, B, State) :-
    State \= [putOn(_, _)|_],
    parse(Phrase, _, _, B, State, true).

whatOld(Phrase, B, Actions) :-
    processActions(Actions, S),
    parse(Phrase, _, _, B, S, false).

parse([], [], _, _, _, _).
parse(Phrase, [preposition(leftOf, OtherObject, Object, S)|ResultRest], Object, Object, S, CheckArticle) :-
    Phrase = [between|PhraseRest],
    preposition(leftOf, OtherObject, Object, S),
writeln(preposition(leftOf, OtherObject, Object)),
    parse(PhraseRest, ResultRest, Object, OtherObject, S, CheckArticle).

parse(Phrase, [preposition(leftOf, PreviousObject, Object, S)|ResultRest], PreviousObject, Object, S, CheckArticle) :-
    Phrase = [and|PhraseRest],
    preposition(leftOf, PreviousObject, OtherObject, S),
 writeln(preposition(leftOf, PreviousObject, OtherObject, S)),
    parse(PhraseRest, ResultRest, PreviousObject, OtherObject, S, CheckArticle).

parse(Phrase, [preposition(Preposition, Object, OtherObject, S)|ResultRest], PreviousObject, Object, S, CheckArticle) :-
    Phrase = [Preposition|PhraseRest],
    preposition(Preposition, Object, OtherObject, S),
    parse(PhraseRest, ResultRest, PreviousObject, OtherObject, S, CheckArticle).

parse(Phrase, [article(Article), adjectiveList(Adjectives), common_noun(Noun,Object)|ResultRest], PreviousObject, Object, S, CheckArticle) :-
    Phrase = [Article|PhraseRest1],
    article(Article),
    getAdjectives(PhraseRest1, [], Adjectives, PhraseRest2, Object),
    PhraseRest2 = [Noun|PhraseRest3],
    common_noun(Noun, Object),
    appendList(FirstPhrase, PhraseRest3, Phrase),
    articleOk(FirstPhrase, CheckArticle),
    parse(PhraseRest3, ResultRest, PreviousObject, Object, S, CheckArticle).

articleOk(_, false).
articleOk([Article|_], true) :- 
    Article \= the.
articleOk(Phrase, true) :- 
    Phrase = [the|_],
    findall(B, parse(Phrase, _, _, B, _, false), List),
    sort(List, Sol),
    length(Sol,1).

getAdjectives(Phrase, Adjectives, Adjectives, Phrase, _) :-
    Phrase = [Adjective|_],
    \+ (adjective(Adjective, _)).

getAdjectives(Phrase, AdjectivesTmp, Adjectives, Rest, Object) :-
    Phrase = [Adjective|RestTmp],
    adjective(Adjective, Object),
    appendList(AdjectivesTmp, [adjective(Adjective, Object)], NewAdjectivesTmp),
    getAdjectives(RestTmp, NewAdjectivesTmp, Adjectives, Rest, Object).


/***********************************************
            Precondition Axioms
***********************************************/


%clear(X, S) :- 
%    \+ currentLocation(_, X, S).

precond(putOn(Block, X), S) :-
    Block \= X,
    tableArea(X, Location, S),
    canPutOn(Location),
    clear(Location, S),
    clear(Block, S),
    \+ locatedOn(Block, Location, S).

tableArea(Location, Location, _) :-
    isBlock(Location).
tableArea(Location, Location, _) :-
    isArea(Location).
tableArea(table, Location, S) :-
    isArea(Location),
    clear(Location, S).

canPutOn(Place) :-
    isArea(Place).
canPutOn(Place) :-
    isCube(Place).

currentLocation(Block, CurrentLocation, []) :-
    locatedOn(Block, CurrentLocation, []).
currentLocation(Block, CurrentLocation, S) :-
    S \= [],
    locatedOn(Block, CurrentLocation, S).
currentLocation(Block, CurrentLocation, S) :-
    S \= [],
    \+ locatedOn(Block, CurrentLocation, S),
    \+ clear(CurrentLocation, S),
    locatedOn(Block, CurrentLocation, []).


/***********************************************
            Successor State Axioms
***********************************************/

succ(putOn(Block, X), S1, S2) :-
    locatedOn(Block, CurrentLocation, S1),
    specificLocation(CurrentLocation),
    delete(S1, locatedOn(Block, CurrentLocation, S1), STemp1),
    delete(STemp1, clear(X, S1), STemp2),
    replaceStatus(STemp2, S1, S2, STemp3),
    appendList(STemp3, [clear(CurrentLocation, S2)], STemp4),
    appendList(STemp4, [locatedOn(Block, X, S2)], S2).

specificLocation(CurrentLocation) :-
    isArea(CurrentLocation).
specificLocation(CurrentLocation) :-
    isBlock(CurrentLocation).

replaceStatus([], _, _, []).
replaceStatus([locatedOn(Block, Location, S1)|StatusRest], S1, S2, [locatedOn(Block, Location, S2)|NewStatusRest]) :-
    replaceStatus(StatusRest, S1, S2, NewStatusRest).
replaceStatus([clear(Location, S1)|StatusRest], S1, S2, [clear(Location, S2)|NewStatusRest]) :-
    replaceStatus(StatusRest, S1, S2, NewStatusRest).


/***********************************************
                    Lexicon
***********************************************/

execute(Action, S1, S2) :-
    precond(Action, S1),
    succ(Action, S1, S2).

processActions(Actions, S) :-
    processActionsAux(Actions, [], S).

processActionsAux([], S, S).

processActionsAux([Action|ActionsRest], S1, S2) :-
    execute(Action, S1, STmp),
    processActionsAux(ActionsRest, STmp, S2).


/********************
    Implement do(List, S1, S2)
********************/

do(Sentence, S1, S2) :-
    appendList([put|NP1], [on|NP2], Sentence),
    what(NP1, B1, S1),
    what(NP2, B2Tmp, S1),
    checkTable(B2Tmp, S1, B2),
    execute(putOn(B1, B2), S1, S2).

checkTable(B, _, B) :-
    B \= table.
checkTable(table, S, Area) :-
    isArea(Area),
    clear(Area, S).
