/* ESERCIZI SULLO SCHEMA MOVIE RATINGS */

--------------------------INTERROGAZIONI CON ORDINAMENTO --------------------------

/*
Ottieni i titoli dei film diretti da Steven Spielberg in ordine alfabetico.
Riporta nella risposta i titoli dei film in ordine alfabetico, separati da virgole.
*/

SELECT movie.title
FROM movies.movie
where director = 'Steven Spielberg'
order by movie.title

-- E.T.,Raiders of the Lost Ark

/*
Ottieni, in ordine crescente, gli anni distinti in cui sono stati prodotti film che hanno
ricevuto una valutazione maggiore o uguale a 4. Qual è il terzo anno nel risultato?
*/

SELECT distinct year
FROM movies.movie
	join movies.rating on movie.mid = rating.mid
where rating.stars >= 4
order by year

-- 1981


/*
Ottieni, in ordine alfabetico, i titoli dei film per i quali non è specificato il regista.
Riporta nella risposta i titoli, separati da virgole:
*/

SELECT movie.title
FROM movies.movie
where movie.director is null
order by movie.title

-- Snow White

/*
Ottieni i nomi dei critici che hanno assegnato una valutazione inferiore a 4 al film Avatar,
in ordine alfabetico. Riporta nella risposta i nomi separati da virgole.
*/

SELECT reviewer.name
FROM movies.movie
	join movies.rating on movie.mid = rating.mid
	join movies.reviewer on rating.rid = reviewer.rid
where movie.title = 'Avatar'
	and rating.stars < 4
order by reviewer.name

-- Elizabeth Thomas

/*
Ottieni i nomi dei critici, i titoli dei film, la corrispondente valutazione e la data della valutazione, ordinati come segue:
 - nome del critico (in ordine alfabetico);
 - titolo del film (in ordine alfabetico)
 - valutazione (dalla più alta alla più bassa).
Qual è la data nella quinta riga del risultato?
*/

SELECT reviewer.name, movie.title, rating.stars, rating.ratingdate
FROM movies.movie
	join movies.rating on movie.mid = rating.mid
	join movies.reviewer on rating.rid = reviewer.rid
order by  reviewer.name asc, movie.title asc, rating.stars desc

-- 2011-01-22

/*
Ottieni, in ordine alfabetico, i nomi dei critici che hanno valutato almeno due volte lo stesso film
assegnando un voto maggiore nella valutazione più recente tra le due. Qual è il primo nome nel risultato?
*/

SELECT distinct reviewer.name
FROM movies.reviewer
	join movies.rating on reviewer.rid = rating.rid
WHERE exists (	select *
		from movies.rating as tab2
		where tab2.rid = rating.rid
			and tab2.mid = rating.mid
			and tab2.ratingdate > rating.ratingdate
			and tab2.stars > rating.stars )
order by reviewer.name

select distinct RE.name
    from movies.Reviewer RE, movies.Rating RA1, movies.Rating RA2
    where RE.rID = RA1.rID
      and RE.rID = RA2.rID
      and RA1.mid = RA2.mid
      and RA1.ratingDate > RA2.ratingDate
      and RA1.stars > RA2.stars
    order by RE.name;

-- Sarah Martinez




--------------------------INTERROGAZIONI CON RAGGRUPPAMENTO --------------------------

-------------------------- Domanda 1
--Quanti film nella base di dati sono stati prodotti tra il 1977 e il 1985 inclusi?

select count(*)
from movies.movie
where year >= '1977' and year < '1986'


-------------------------- Domanda 2
-- Qual è la valutazione media dei film di James Cameron?

select avg(stars)
from movies.movie
	join movies.rating on movie.mid = rating.mid
where director = 'James Cameron'

-------------------------- Domanda 3
-- Qual è la data della critica più recente a un film di Victor Fleming?

select max(ratingdate)
from movies.movie
	join movies.rating on movie.mid = rating.mid
where director = 'Victor Fleming'


-------------------------- Domanda 4
-- Ottieni il titolo e la valutazione massima per ogni film che è stato valutato
-- almeno una volta, in ordine decrescente rispetto al voto 

select movie.title, max(stars)
from movies.movie
	join movies.rating on movie.mid = rating.mid
group by movie.title, movie.mid
order by max(stars) desc

-- Attenzione al group by, è meglio inserire anche la chiave primaria per evitare
-- clash sui titoli dei film


-------------------------- Domanda 5
-- Ottieni il titolo e la differenza tra voto massimo e voto minimo per ogni film
-- che abbia ricevuto almeno due valutazioni. Ordina il risultato alfabeticamente rispetto al titolo.

select movie.title, max(rating.stars)-min(rating.stars)
from movies.movie
	join movies.rating on movie.mid = rating.mid
group by movie.title, movie.mid
having count(*) >= 2
order by movie.title


-------------------------- Domanda 6
-- Ottieni, in ordine alfabetico, i nomi dei registi dei film che hanno ricevuto una valutazione media
-- di tutti i loro film superiore a 3 (presta attenzione al fatto che il nome di un regista può non essere noto)

select movie.director, avg(stars)
from movies.movie
	join movies.rating on movie.mid = rating.mid
where movie.director is not null
group by movie.director
having avg(stars) > 3
order by movie.director


-------------------------- Domanda 7
-- Ottieni, per ciascun critico, il nome del critico e il numero di film da costui recensiti,
-- ordinando il risultato rispetto al nome. 

select reviewer.name, count(distinct rating.mid)
from movies.reviewer 
	join movies.rating on reviewer.rid = rating.rid
group by reviewer.name, reviewer.rid
order by reviewer.name

-- Attenzione al distinct... 
-- Non viene richiesto il numero di recensioni... ma il numero di film...

	

-------------------------- Domanda 8
-- Ripeti l'interrogazione precedente, ma includi nel risultato (in ordine alfabetico)
-- soltanto i critici che hanno recensito uno e un solo un film.

select reviewer.name, count(distinct rating.mid)
from movies.reviewer 
	join movies.rating on reviewer.rid = rating.rid
group by reviewer.name, reviewer.rid
having count(distinct rating.mid) = 1
order by reviewer.name


-------------------------- Domanda 9
-- Ottieni l'elenco degli studenti che hanno almeno 3 amici, ordinato in modo decrescente
-- rispetto al numero di amici e, a parità di amici, in ordine alfabetico rispetto al nome dello studente. 

select student.name, count(*)
from social_network.student
	join social_network.friend on student.id = friend.id1
group by student.name, student.id
having count(*) >= 3
order by count(*) desc, student.name asc

-- Attenzione al group by, sono presenti delle omonimie!


-------------------------- Domanda 10
-- Ottieni i nomi delle coppie di studenti che hanno almeno 2 amici in comune.
-- Uno studente C è un amico in comune ad A e B se C è amico sia di A sia di B
-- (A e B non sono necessariamente amici). Le coppie devono comparire una sola
-- volta nel risultato (cioè, se (A,B) sta nel risultato allora (B,A) non è presente)
-- e devono essere ordinate rispetto al primo nome e, a parità del primo nome, rispetto al secondo.

select s1.name, s2.name, count(*)
from social_network.student as s1 
	join social_network.friend as f1 on s1.id = f1.id1
	join social_network.friend as f2 on f1.id2 = f2.id2
	join social_network.student as s2 on s2.id = f2.id1
where s1.id < s2.id
group by S1.id, S2.id, s1.name, s2.name
having count(*) >= 2
order by s1.name, s2.name

-- Attenzione al group by, sono presenti delle omonimie!



--------------------------INTERROGAZIONI CON ANNIDAMENTO --------------------------


-------------------------- Domanda 1
-- Tra i film che sono stati recensiti almeno una volta, trova (in ordine alfabetico)
-- i titoli di quelli che hanno ricevuto soltanto valutazioni maggiori o uguali a 4.

select movie.title
from movies.movie
where 	not exists (	select *
			from movies.rating
			where rating.mid = movie.mid
				and rating.stars < 4	)
	and exists (	select *
			from movies.rating
			where rating.mid = movie.mid	)
order by movie.title

-- oppure...

select distinct movie.title
from movies.movie
	join movies.rating on rating.mid = movie.mid
where 	not exists (	select *
			from movies.rating
			where rating.mid = movie.mid
				and rating.stars < 4	)
order by movie.title


-------------------------- Domanda 2
-- Quanti registi hanno diretto film più vecchi del film con mID = 107?

select count(distinct director)
from movies.movie
where movie.year < (	select year
			from movies.movie as tab2
			where tab2.mid = 107	)



-------------------------- Domanda 3
-- Trova le coppie di nomi di critici cinematografici che non hanno mai espresso valutazioni
-- uguali per i film che hanno recensito entrambi. Considera solo le coppie di critici tali che
-- esiste almeno un film che hanno recensito entrambi. Produci il risultato ordinato rispetto
-- al nome del primo critico e poi rispetto al nome del secondo.

select rev1.name, rev2.name
from movies.reviewer as rev1
	join movies.reviewer as rev2 on rev1.rid < rev2.rid
where exists (	select *
		from movies.rating as rat1
			join movies.rating as rat2 on rat1.mid = rat2.mid
		where rat1.rid = rev1.rid
			and rat2.rid = rev2.rid ) -- esiste un film che hanno recensito entrambi
	and not exists (select *
			from movies.rating as rat1
				join movies.rating as rat2 on rat1.mid = rat2.mid
			where rat1.rid = rev1.rid
				and rat2.rid = rev2.rid
				and rat1.stars = rat2.stars) -- mai una stessa valutazione
order by rev1.name, rev2.name
	


-------------------------- Domanda 4
-- Qual è il critico che ha recensito tutti i film di Spielberg?

select name
from movies.reviewer
where not exists (	select *
			from movies.movie
			where movie.director = 'Steven Spielberg'	
				and not exists (	select *
							from movies.rating
							where rating.mid = movie.mid 
								and rating.rid = reviewer.rid )
		 )
-- estrae tutti i rencensori tali che non esiste alcun film di Spielberg che non hanno recensito


-------------------------- Domanda 5
-- Elenca in ordine alfabetico, separati da virgole (senza spazi),
-- i nomi degli studenti che hanno soltanto amici maggiorenni (>= 18 anni).

select name
from social_network.student as tab1
where not exists (	select *
			from social_network.friend
				join social_network.student as tab2 on friend.id2 = tab2.id
			where friend.id1 = tab1.id
				and tab2.age < 18	)
order by name



-------------------------- Domanda 6
-- Elenca in ordine alfabetico, separati da virgole e senza spazi,
-- i nomi degli studenti che hanno più amici dello studente con id = 1709.

select student.id, student.name, count(*)
from social_network.student
	join social_network.friend on student.id = friend.id1
group by student.id, student.name
having count(*) > (	select count(*)
			from social_network.friend as f1
			where f1.id1 = 1709 )
order by student.name

-- Group by con anche student.id per evitare possibili clash di nome


-------------------------- Domanda 7
-- Quante sono le coppie di studenti che non hanno amici in comune?
-- (Attenzione a non considerare la stessa coppia due volte).

select count(*)
from social_network.student as s1
	join social_network.student as s2 on s1.id < s2.id
where not exists (	select *
			from social_network.friend as f1
				join social_network.friend as f2 on f1.id2 = f2.id2
			where f1.id1 = s1.id
				and f2.id1 = s2.id	)