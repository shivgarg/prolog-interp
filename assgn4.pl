female(kunti).
female(madri).
female(hidimba).
female(satyavati).
female(ganga).
female(chitranagda).
female(ambika).
female(ambalika).
female(gandhari).
female(dushala).
female(sukhada).
female(draupadi).
female(subhadra).
female(rohini).
female(devaki).
female(uttara).
female(kripi).
female(shikandi).
female(kripi).

male(pandu).
male(subala).
male(shakuni).
male(suryadeva).
male(shantanu).
male(vichitravirya).
male(vyasa).
male(vidur).
male(drupada).
male(dhrishtadyumma).
male(shikandi).
male(yudhisthir).
male(arjun).
male(nakul).
male(bheema).
male(sahdev).
male(karana).
male(ghatotkatch).
male(duryodhan).
male(dushasn).
male(jayadratha).
male(yuyutsu).
male(vikrana).
male(abhimanyu).
male(vasudeva).
male(balram).
male(krishna).
male(virata).
male(parikshit).
male(sharadwan).
male(kripacharya).
male(dronacharya).
male(aswathama).
male(dhritrashtra).

married(kunti,pandu).
married(madri,pandu).
married(kunti,suryadeva).
married(hidimba,bheema).
married(satyavati,shantanu).
married(shantanu,ganga).
married(ambika,vichitravirya).
married(ambalika,vichitravirya).
married(gandhari ,dhritrashtra).
married(dushala,jayadratha).
married(draupadi,arjun).
married(draupadi,bheema).
married(draupadi,yudhisthir).
married(draupadi,nakul).
married(draupadi,sahdev).
married(subhadra,arjun).
married(rohini,vasudeva).
married(devaki,vasudeva).
married(uttara,abhimanyu).
married(kripi,dronacharya).


child(vidur,vyasa).

child(vichitravirya,shantanu).
child(vichitravirya,satyavati).

child(shakuni,subala).
child(gandhari,subala).

child(duryodhan,gandhari).
child(duryodhan,dhritrashtra).
child(dushasn,gandhari).
child(dushasn,dhritrashtra).
child(dushala,gandhari).
child(dushala,dhritrashtra).
child(vikrana,gandhari).
child(vikrana,dhritrashtra).

child(draupadi,drupada).
child(dhrishtadyumma,drupada).
child(shikandi,drupada).

child(yuyutsu,sukhada).
child(yuyutsu,dhritrashtra).


child(dhritrashtra,ambika).
child(dhritrashtra,vyasa).

child(pandu,vyasa).
child(pandu,ambalika).

child(yudhisthir,pandu).
child(yudhisthir,kunti).

child(bheema,pandu).
child(bheema,kunti).

child(arjun,pandu).
child(arjun,kunti).

child(nakul,pandu).
child(nakul,madri).

child(sahdev,pandu).
child(sahdev,madri).

child(karana,suryadeva).
child(karana,kunti).

child(ghatotkatch,bheema).
child(ghatotkatch,hidimba).

child(abhimanyu,subhadra).
child(abhimanyu,arjun).

child(subhadra, vasudeva).
child(subhadra,rohini).

child(uttara,virata).

child(parikshit,uttara).
child(parikshit,abhimanyu).

child(aswathama,dronacharya).
child(aswathama,kripi).

child(kripi,sharadwan).
child(kripacharya,sharadwan).


couple(X,Y) :- married(X,Y).
couple(X,Y) :- married(Y,X).

father(X,Y) :- child(Y,X),male(X).
mother(X,Y) :- child(Y,X),female(X).

parent(X,Y) :- mother(X,Y).
parent(X,Y) :- father(X,Y).


same(X,X).

brother(X,Y) :- male(X),father(Z,X),father(Z,Y),mother(W,X),mother(W,Y),\+same(X,Y).

sister(X,Y) :- female(X),father(Z,X),father(Z,Y),mother(W,X),mother(W,Y),\+same(X,Y).

fullsibling(X,Y) :- father(Z,X),father(Z,Y),mother(W,X),mother(W,Y),\+same(X,Y).
halfsibling(X,Y) :- father(Z,X),father(Z,Y),mother(W,X),\+mother(W,Y).
halfsibling(X,Y) :- mother(Z,X),mother(Z,Y),father(W,X),\+father(W,Y).

sibling(X,Y) :- fullsibling(X,Y).
sibling(X,Y) :- halfsibling(X,Y).

halfsister(X,Y) :- female(X),father(Z,X),father(Z,Y),mother(W,X),\+mother(W,Y).
halfsister(X,Y) :- female(X),mother(Z,X),mother(Z,Y),father(W,X),\+father(W,Y).

halfbrother(X,Y) :- male(X),father(Z,X),father(Z,Y),mother(W,X),\+mother(W,Y).
halfbrother(X,Y) :- male(X),mother(Z,X),mother(Z,Y),father(W,X),\+father(W,Y).

stepfather(X,Y) :- mother(Z,Y),couple(X,Z),\+child(Y,X).
stepmother(X,Y) :- father(Z,Y),couple(X,Z),\+child(Y,X).

brotherinlaw(X,Y) :- male(X),sibling(Z,Y),couple(Z,X).
brotherinlaw(X,Y) :- male(X),couple(Y,Z),sibling(X,Z).

sisterinlaw(X,Y) :- female(X),sibling(Z,Y),couple(Z,X).
sisterinlaw(X,Y) :- female(X),sibling(X,Z),couple(Y,Z).

uncle(X,Y) :- father(Z,Y),brother(X,Z).
uncle(X,Y) :- father(Z,Y),brotherinlaw(X,Z).
uncle(X,Y) :- mother(Z,Y),brother(X,Z).
uncle(X,Y) :- mother(Z,Y),brotherinlaw(X,Z).

aunt(X,Y) :- father(Z,Y),sister(X,Z).
aunt(X,Y) :- father(Z,Y),sisterinlaw(X,Z).
aunt(X,Y) :- mother(Z,Y),sister(X,Z).
aunt(X,Y) :- mother(Z,Y),sisterinlaw(X,Z).

fatherinlaw(X,Y) :- couple(Y,Z),father(X,Z).
motherinlaw(X,Y) :- couple(Y,Z),mother(X,Z).

descendant(X,Y) :- child(X,Y).
descendant(X,Y) :- child(X,Z),descendant(Z,Y).

ancestor(X,Y) :- child(Y,X).
ancestor(X,Y) :- child(Y,Z),ancestor(X,Z).

cousin(X,Y) :- child(X,Z),sibling(Z,W),child(Y,W).

nthcousin(X,Y) :- cousin(X,Y).
nthcousin(X,Y) :- child(X,Z),child(Y,W),nthcousin(Z,W),\+same(X,Y),\+sibling(X,Y).

grandparent(X,Y) :- child(Y,Z),child(Z,X).

stepchild(X,Y) :- stepfather(Y,X).
stepchild(X,Y) :- stepmother(Y,X).

niece(X,Y) :- sibling(Z,Y),child(X,Z),female(X).
niece(X,Y) :- cousin(Z,Y),child(X,Z),female(X).

nephew(X,Y) :- sibling(Z,Y),child(X,Z),male(X).
nephew(X,Y) :- cousin(Z,Y),child(X,Z),male(X).
