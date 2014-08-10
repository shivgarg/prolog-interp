/*vdmklfsdjgskeljgvees
ge
ve
te
tbe
vg
ertve
vt*/

a.
male(bharata).
male(hasti).
male(kuru).
male(shantanu).
male(bheeshma).
male(vichitravirya).
male(chitrangada).
male(vasishta).
male(shakti).
male(parashara).
male(vyasa).
male(pandu).
male(dhriturashtra).
male(vidura).
male(suryadevta).
male(yadu).
male(surasena).
male(kuntibhoja).
male(nandlala).
male(vasudeva).
male(duryodhana).
male(balarama).
male(krishna).
male(pradyumna).
male(aniruddha).
male(karna).
male(yudhistira).
male(bheema).
male(arjuna).
male(nakula).
male(sahadeva).
male(drupada).
male(ghatotkacha).
male(prativindhya).
male(sutasoma).
male(shatanika).
male(shrutakarma).
male(shrutakirti).
male(abhimanyu).
male(parikshit).
male(janmejaya).
female(satyavati).
female(ambika).
female(ambalika).
female(dasi).
female(kunti).
female(madri).
female(gandhari).
female(uttara).
female(dusheela).
female(devaki).
female(yashoda).
female(subhadra).
female(rukmini).
female(draupadi).
female(hidimbi).
female(ganga).
child(balarama,nandlala).
child(balarama,yashoda).
child(krishna,vasudeva).
child(krishna,devaki).
child(krishna,nandlala).
child(krishna,yashoda).
child(subhadra,nandlala).
child(subhadra,yashoda).
child(pandu,ambika).
child(aniruddha,pradyumna).
child(karna,suryadevta).
child(yudhistira,kunti).
child(yudhistira,pandu).
child(bheema,kunti).
child(bheema,pandu).
child(arjuna,kunti).
child(arjuna,pandu).
child(ghatotkacha,hidimbi).
child(ghatotkacha,bheema).
child(prativindhya,draupadi).
child(prativindhya,yudhistira).
child(sutasoma,draupadi).
child(sutasoma,bheema).
child(shatanika,draupadi).
child(shatanika,nakula).
child(shrutakarma,draupadi).
child(shrutakarma,sahadeva).
child(shrutakirti,draupadi).
child(shrutakirti,arjuna).
child(abhimanyu,subhadra).
child(abhimanyu,arjuna).
child(parikshit,uttara).
child(parikshit,abhimanyu).
child(janmejaya,parikshit).
child(nakula,madri).
child(nakula,pandu).
child(sahadeva,madri).
child(sahadeva,pandu).
child(draupadi,drupada).
child(pandu,vyasa).
child(dhriturashtra,ambalika).
child(dhriturashtra,vyasa).
child(vidura,dasi).
child(vidura,vyasa).
child(surasena,yadu).
child(kuntibhoja,yadu).
child(nandlala,surasena).
child(kunti,surasena).
child(kunti,kuntibhoja).
child(duryodhana,gandhari).
child(duryodhana,dhriturashtra).
child(dusheela,gandhari).
child(dusheela,dhriturashtra).
child(pradyumna,rukmini).
child(pradyumna,krishna).
child(hasti,bharata).
child(kuru,hasti).
child(shantanu,kuru).
child(bheeshma,ganga).
child(bheeshma,shantanu).
child(vichitravirya,satyavati).
child(vichitravirya,shantanu).
child(chitrangada,satyavati).
child(chitrangada,shantanu).
child(shakti,vasishta).
child(parashara,shakti).
child(vyasa,parashar).
child(vyasa,satyavati).
child(karna,kunti).
married(uttara,abhimanyu).
married(satyavati,shantanu).
married(devaki,vasudeva).
married(yashoda,nandlala).
married(subhadra,arjuna).
married(rukmini,krishna).
married(hidimbi,bheema).
married(ganga,shantanu).
married(shantanu,ganga).
married(ambika,vichitravirya).
married(ambalika,vichitravirya).
married(kunti,pandu).
married(madri,pandu).
married(gandhari,dhriturashtra).
married(draupadi,yudhistira).
married(draupadi,bheema).
married(draupadi,arjuna).
married(draupadi,nakula).
married(draupadi,sahadeva).
married(ambika,vyasa).
married(ambalika,vyasa).
couple(X,Y) :- married(X,Y).
couple(X,Y) :- married(Y,X).
father(X,Y) :- male(X), child(Y,X).
mother(X,Y) :- female(X), child(Y,X).
stepfather(X,Y) :- male(X), child(Y,Z), couple(Z,X), \+child(Y,X).
stepmother(X,Y) :- female(X), child(Y,Z), couple(Z,X),\+child(Y,X).
same(X,X).
fullsiblings(X,Y) :- child(X,Z), child(Y,Z),female(Z),child(X,W),child(Y,W),male(W),\+same(X,Y).
halfbrother(X,Y) :- male(X), father(Z,X)  ,father(Z,Y), \+fullsiblings(X,Y),\+same(X,Y).
halfbrother(X,Y) :- male(X) ,mother(Z,X), mother(Z,Y), \+fullsiblings(X,Y),\+same(X,Y).
halfsister(X,Y) :- mother(Z,X), female(X) ,mother(Z,Y), \+fullsiblings(X,Y),\+same(X,Y).
halfsister(X,Y) :- father(Z,X), female(X) ,father(Z,Y), \+fullsiblings(X,Y),\+same(X,Y).
brother(X,Y) :- fullsiblings(X,Y) , male(X).
sister(X,Y) :- fullsiblings(X,Y) , female(X).
siblings(X,Y) :- fullsiblings(X,Y), \+same(X,Y).
siblings(X,Y) :- halfsister(X,Y),\+same(X,Y).
siblings(X,Y) :- halfbrother(X,Y),\+same(X,Y).
sisterinlaw(X,Y) :- female(X), siblings(Y,W),male(W), couple(X,W).
sisterinlaw(X,Y) :- female(X),couple(Y,Z),siblings(X,Z).
brotherinlaw(X,Y) :- male(X), siblings(Y,W),female(W), couple(X,W).
brotherinlaw(X,Y) :- male(X),couple(Y,Z),siblings(X,Z).
fatherinlaw(X,Y) :- male(X),child(Z,X), couple(Y,Z).
motherinlaw(X,Y) :- female(X),child(Z,X), couple(Y,Z).
cousin(X,Y) :- child(X,W), child(Y,Z), siblings(W,Z),\+siblings(X,Y),\+same(Y,X).
nthcousin(X,Y) :- cousin(X,Y).
nthcousin(X,Y) :- child(X,Z) , child(Y,W), nthcousin(Z,W).
niece(X,Y) :- nthcousin(Y,Z), female(X) ,child(X,Z).
niece(X,Y) :- siblings(Y,Z), child(X,Z),female(X).
nephew(X,Y) :- nthcousin(Y,Z), male(X) ,child(X,Z).
nephew(X,Y) :- siblings(Y,Z), child(X,Z),male(X).
uncle(X,Y) :- child(Y,Z), siblings(Z,X),\+child(Y,X),male(X).
uncle(X,Y) :- child(Y,Z), siblings(Z,W),male(X),couple(W,X),\+child(Y,X).
aunt(X,Y) :- child(Y,Z), siblings(Z,X),\+child(Y,X),female(X).
aunt(X,Y) :- child(Y,Z), siblings(Z,W),female(X),couple(W,X),\+child(Y,X).
ancestor(X,Y) :- child(Y,X).
ancestor(X,Y) :- child(Y,Z), ancestor(X,Z).
descendant(X,Y) :- ancestor(Y,X).
grandparent(X,Y) :- child(Z,X),child(Y,Z).
stepchild(X,Y) :- stepfather(Y,X).
stepchild(X,Y) :- stepmother(Y,X).
