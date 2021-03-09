/*Examples*/
/*
Use `?-['filename.py'].` to (re)load the db to the program.
Use `?-c?-['myfilea.pl', 'myfileb.pl', 'myfilec.pl'].` to load different files into the program.

*/
% Example of direct recursion
likes(john,X):-likes(X,Y),dog(Y).

% Example of shortenning
/* go:-dog(X),large(X),write(X),
	write(' is a large dog'),nl.*/
chases(X,Y):- dog(X),cat(Y), write(X), write(' chases '), write(Y),nl.
go:-chases(_X,_Y).
/*The variables in chase starts with underscore to supress singleton warning*/
/* chases is a predicate with two arguments*/
/* go is a predicate with no arguments */

/*Biblical*/
father(terach, abraham).
father(terach, nachor).
father(terach, haran).
father(abraham, isaac).
father(haran, lot).
father(haran, milcah).
father(haran, yiscah).
mother(sarah, isaac).
male(terach).
male(abraham).
male(nachor).
male(haran).
male(isaac) .
male(lot).
female(sarah).
female(milcah).
female(yiscah).


/*People*/
woman(mia).
woman(jody).
woman(yolanda).
party.

happy(yolanda).
happy(vincent).
listens2Music(mia).
listens2Music(butch).
listens2Music(yolanda) :- happy(yolanda).

playsAirGuitar(jody).
playsAirGuitar(mia) :- listens2Music(mia).
playsAirGuitar(yolanda) :- listens2Music(yolanda).
playsAirGuitar(vincent):-listens2Music(vincent),happy(vincent).
playsAirGuitar(butch):- happy(butch).
playsAirGuitar(butch):- listens2Music(butch).

loves(vincent,mia).
loves(marsellus, mia).
loves(pumpkin, honey_bunny).
loves(honey_bunny, pumpkin).

jealous(X,Y):- loves(X,Z), loves(Y,Z).


/* Animals Database */
animal(mammal,tiger,carnivore,stripes).
animal(mammal,hyena,carnivore,ugly).
animal(mammal,lion,carnivore,mane).
animal(mammal,zebra,herbivore,stripes).
animal(bird,eagle,carnivore,large).
animal(bird,sparrow,scavenger,small).
animal(reptile,snake,carnivore,long).
animal(reptile,lizard,scavenger,small).


/* Hogwarts Database */
% wizard(ron).
hasWand(harry).
quidditchPlayer(harry).
wizard(X):- hasBroom(X),hasWand(X).
hasBroom(X):- quidditchPlayer(X). 


/*Pet Database*/
alpha.
beta.

dog(misty).
dog(harry).
dog(fido).
large(fido).

cat(jane).
cat(mary).



