/*Phrase vocabulary*/

   /*Adjectives*/
adj(old).
adj(teenage).
adj(good).
adj(sprightly).
adj(long).
adj(social).
adj(young).
adj(avid).
adj(racing).

   /*Determines*/

det(a).
det(an).

   /*Nouns*/

noun(father).
noun(book).
noun(boy).
noun(horses).
noun(grandfather).
noun(walk).
noun(person).
noun(chat).
noun(student).
noun(guitar).
noun(petrolhead).
noun(car).

   /*Verbs*/

verb(likes).
verb(loves).

/*Outputs - Recommendations*/

recommend(likes, book, "'He joins the book club.'").
recommend(loves, horses, "'They join a riding club'").
recommend(loves, walk, "'He joins a rambling club'").
recommend(likes, chat, "'They join a social club'").
recommend(likes, guitar, "'They should join a band'").
recommend(loves, car, "'They should go to the races'").


/*Noun Phrase and Verb Phrase Splitter*/

sentence(Sentence,sentence(np(Noun_Phrase),vp(Verb_Phrase))):-
      np(Sentence,Noun_Phrase,Rem), vp(Rem,Verb_Phrase).

/*Tree*/
add(Value, Condition) :-
       Condition, Value.

add(_, Condition) :-
        \+Condition.

recursecheck([], _).

recursecheck([Firsthalf|Secondhalf], N) :-
      creategraph(Firsthalf, N),
      add(nl, Secondhalf \== []),
      recursecheck(Secondhalf, N).

creategraph(Sentence, N) :- Sentence =.. [Firsthalf|Secondhalf],
      length(Secondhalf, Length),
      add(tab(N), Length >= 0),
      write(Firsthalf),
      add(write('|-- '), Length >= 1),
      add(nl, Length >= 1), Nx is N + 5,
      recursecheck(Secondhalf, Nx).

creategraph(E) :- creategraph(E, 0).

/*Recommend Verb and Noun Catch*/

catchwords([Firsthalf|Secondhalf], Noun, Verb) :-
      verbtrue([Firsthalf|Secondhalf], Verb),
      nountrue([Firsthalf|Secondhalf], Noun).

verbtrue([Firsthalf|_], Verb) :-
      verb(Firsthalf),
      Verb = Firsthalf.

verbtrue([_|Secondhalf], Verb) :-
      verbtrue(Secondhalf, Verb).

nountrue([Firsthalf|_], Noun) :-
      noun(Firsthalf),
      Noun = Firsthalf.

nountrue([_|Secondhalf], Noun) :-
      nountrue(Secondhalf, Noun).


/*input and Output*/

agent:-
      perceive(Percepts),
      action(Percepts).

perceive(Input):-
      nl,
      write('Enter Sentence: '),
      read(Input),
      nl.

action(Input) :-
      sentence(Input, X),
      write(X),
      nl,
      nl,
      creategraph(X),
      np(Input,_, Secondphrase),
      catchwords(Secondphrase, Noun, Verb),
      recommend(Verb, Noun, Recommendation),
      nl,
      nl,
      write('Response:'),
      write(Recommendation),
      nl,
      nl,
      write('Graphical Tree:'),
      nl,
      nl,
      write('|'), tab(8), write('|- '), write(Input),
      nl,
      write('|'), tab(8), write('|- '), write('Produces the recommendation that'),
      nl,
      tab(9), write('|'), tab(8), write('|- '), write(Recommendation).

/*sentence breaker*/

   /*Noun Phrases*/

np([X|T],np(det(X),NP2),Rem):-
      det(X),
      np2(T,NP2,Rem).

np(Sentence,Parse,Rem):-
      np2(Sentence,Parse,Rem).

np(Sentence,np(NP,PP),Rem):-
      np(Sentence,NP,Rem1),
      pp(Rem1,PP,Rem).

pp([H|T],pp(det(H),Parse),Rem):-
      det(H),
      np(T,Parse,Rem).

np2([H|T],np2(noun(H)),T):-
      noun(H).  /* ok cute H, so you are a noun */

np2([H|T],np2(adj(H),Rest),Rem):-
      adj(H),np2(T,Rest,Rem).

/* shove the adj(H) into the to be retured answer and then recurse on the rest of the phrase
to return the parse and the remainder of the sentence */

   /*Verb Phrases*/

vp([H|[]],verb(H)):-
      verb(H).

vp([H|Rest],vp(verb(H),RestParsed)):-
      verb(H),
      np(Rest, RestParsed, _).

vp([H|Rest],vp(verb(H),RestParsed)):-
      pp(Rest, RestParsed, _).


