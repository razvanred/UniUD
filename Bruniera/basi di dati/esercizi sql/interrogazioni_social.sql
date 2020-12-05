/* ESERCIZI SULLO SCHEMA SOCIAL NETWORK */

/*
Ottieni, in ordine alfabetico, i nomi degli studenti che sono amici di uno studente
di nome Gabriel. Qual è il quarto nome nel risultato?
*/

select distinct student.name
from social_network.student
	join social_network.friend  on student.id = friend.id1
	join social_network.student as tab2 on tab2.id = friend.id2
where tab2.name = 'Gabriel'
order by student.name

-- Jessica

/*
Ottieni in ordine alfabetico il nome e l'età di ogni studente cui piacciono studenti
di almeno due anni più giovani. Riporta nella risposta il risultato nella forma seguente:
*/

select student.name, student.age
from social_network.student
where exists (	select *
		from social_network.likes
			join social_network.student as tab2 on likes.id2 = tab2.id
		where tab2.age < (student.age - 1)
			and likes.id1 = student.id )
order by student.name

select distinct student.name, student.age
from social_network.student
	join social_network.likes on student.id = likes.id1
	join social_network.student as tab2 on likes.id2 = tab2.id
where tab2.age < (student.age - 1)
	and likes.id1 = student.id 
order by student.name

-- John, 19

/*
Ottieni i nomi e le età delle coppie di studenti che si piacciono mutuamente.
Ogni coppia deve comparire nel risultato una sola volta, con il primo nome che precede
il secondo nell'ordinamento alfabetico. Produci il risultato ordinato alfabeticamente rispetto ai nomi. 
*/

select student_likes.name, student_likes.age, student_liked.name, student_liked.age
from social_network.student as student_likes
	join social_network.likes on student_likes.id = likes.id1
	join social_network.student as student_liked on likes.id2 = student_liked.id
where exists (	select *
		from social_network.likes as mutual_likes
		where mutual_likes.id2 = likes.id1
			and mutual_likes.id1 = likes.id2)
	and student_likes.name  < student_liked.name
order by student_likes.name, student_liked.name

select student_likes.name, student_likes.age, student_liked.name, student_liked.age
from social_network.student as student_likes
	join social_network.likes on student_likes.id = likes.id1
	join social_network.student as student_liked on likes.id2 = student_liked.id
	join social_network.likes as mutual_likes on (student_likes.id = mutual_likes.id2
							and student_liked.id = mutual_likes.id1)
where student_likes.name  < student_liked.name
order by student_likes.name, student_liked.name

-- Jessica;19;Kyle;20

/*
Ottieni, in ordine decrescente rispetto all'età e, a parità d'età, in ordine alfabetico rispetto ai nomi,
i nomi e le età degli studenti che non compaiono nella tabella Likes.
*/

select student.name, student.age
from social_network.student 
where not exists (	select *
			from social_network.likes 
			where likes.id1 = student.id 
				or likes.id2 = student.id )
order by student.age desc, student.name

select student.name, student.age
from social_network.student 
except
select student.name, student.age
from social_network.student 
	join social_network.likes on ( likes.id1 = student.id
					or likes.id2 = student.id)
order by age desc, name

-- Jordan;16

/*
Ottieni le coppie distinte di nomi degli studenti A e B tali che ad A piace B,
ma a B non piace nessuno. Il risultato dev'essere in ordine alfabetico rispetto ad A.
*/

select distinct student_likes.name, student_liked.name
from social_network.student as student_likes
	join social_network.likes on student_likes.id = likes.id1
	join social_network.student as student_liked on likes.id2 = student_liked.id
where not exists (	select *
			from social_network.likes as liked_student_likes
			where liked_student_likes.id1 = student_liked.id )
order by student_likes.name

select distinct student_likes.name, student_liked.name
from social_network.student as student_likes
	join social_network.likes on student_likes.id = likes.id1
	join social_network.student as student_liked on likes.id2 = student_liked.id
	left outer join social_network.likes as liked_student_likes on student_liked.id = liked_student_likes.id1
where liked_student_likes.id2 is null
order by student_likes.name

-- John;Haley

/*
Ottieni le coppie di nomi di studenti A e B tali che ad A piace B, ma A e B non sono amici.
Il risultato dev'essere in ordine alfabetico rispetto ad A.
Riporta la coppia nella quarta riga del risultato nella forma:
*/

select student_likes.name, student_liked.name
from social_network.student as student_likes
	join social_network.likes on student_likes.id = likes.id1
	join social_network.student as student_liked on likes.id2 = student_liked.id
where not exists (	select *
			from social_network.friend
			where (friend.id1 = student_likes.id 
				and friend.id2 = student_liked.id)
				or
			      (friend.id2 = student_likes.id 
				and friend.id1 = student_liked.id)
		 )
order by student_likes.name, student_liked.name desc

select student_likes.name, student_liked.name
from social_network.student as student_likes
	join social_network.likes on student_likes.id = likes.id1
	join social_network.student as student_liked on likes.id2 = student_liked.id
	left outer join social_network.friend on (	(friend.id1 = student_likes.id 
							and friend.id2 = student_liked.id)
							or
							(friend.id2 = student_likes.id 
							and friend.id1 = student_liked.id)    )
where friend.id1 is null
order by student_likes.name, student_liked.name desc


-- John;Haley

/*
Ottieni i nomi e le età degli studenti che sono apprezzati da almeno due studenti
(cioè, compaiono almeno due volte nel campo id2 della tabella Likes).
Il risultato dev'essere in ordine alfabetico rispetto ai nomi.
*/

select student.name, student.age
from social_network.student
where ( select count(*)
	from social_network.likes
	where likes.id2 = student.id ) > 1
order by student.name

select student.name, student.age
from social_network.student
where exists (	select *
		from social_network.likes
		where likes.id2 = student.id
			and exists (	select *
					from social_network.likes as tab2
					where tab2.id2 = student.id
						and tab2.id1 <> likes.id1 )
	     )
order by student.name

select distinct student.name, student.age
from social_network.student
	join social_network.likes as likes1 on likes1.id2 = student.id
	join social_network.likes as likes2 on (likes2.id2 = student.id
						 and likes2.id1 <> likes1.id1)
order by student.name

-- Kyle,20







