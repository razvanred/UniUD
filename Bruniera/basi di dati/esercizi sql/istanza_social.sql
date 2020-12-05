DROP SCHEMA IF EXISTS social_network CASCADE;


-------------------------- creazione dello schema
CREATE SCHEMA social_network;

SET search_path TO social_network;


-------------------------- creazione delle tabelle e dei vincoli intrarelazionali
CREATE TABLE student(	  
	id integer NOT NULL
,	name varchar(20) NOT NULL
,	age integer NOT NULL
,	CONSTRAINT pk_student PRIMARY KEY (id)
);

CREATE TABLE friend(	  
	id1 integer
,	id2 integer
,	CONSTRAINT pk_friend PRIMARY KEY (id1, id2)
-- si osservi che id1 ed id2 vengono comunque impostati come NOT NULL (vincolo d'integrità dell'entità)
);

COMMENT ON TABLE friend IS 'Symmetric relationship';

CREATE TABLE likes(	  
	id1 integer
,	id2 integer
,	CONSTRAINT pk_likes PRIMARY KEY (id1, id2)
);

COMMENT ON TABLE friend IS 'Non-symmetric relationship';


-------------------------- creazione dei vincoli interrelazionali
ALTER TABLE friend
ADD CONSTRAINT fk_friend1_student FOREIGN KEY (id1)
REFERENCES student (id) 
ON DELETE CASCADE ON UPDATE RESTRICT;

ALTER TABLE friend
ADD CONSTRAINT fk_friend2_student FOREIGN KEY (id2)
REFERENCES student (id) 
ON DELETE CASCADE ON UPDATE RESTRICT;

ALTER TABLE likes
ADD CONSTRAINT fk_likes1_student FOREIGN KEY (id1)
REFERENCES student (id) 
ON DELETE CASCADE ON UPDATE RESTRICT;

ALTER TABLE likes
ADD CONSTRAINT fk_likes2_student FOREIGN KEY (id2)
REFERENCES student (id) 
ON DELETE CASCADE ON UPDATE RESTRICT;

-------------------------- Inserimento dei dati 
insert into Student(id, name, age) values (1510, 'Jordan', 16);
insert into Student(id, name, age) values (1689, 'Gabriel', 16);
insert into Student(id, name, age) values (1381, 'Tiffany', 16);
insert into Student(id, name, age) values (1709, 'Cassandra', 16);
insert into Student(id, name, age) values (1101, 'Haley', 17);
insert into Student(id, name, age) values (1782, 'Andrew', 17);
insert into Student(id, name, age) values (1468, 'Kris', 17);
insert into Student(id, name, age) values (1641, 'Brittany', 17);
insert into Student(id, name, age) values (1247, 'Alexis', 19);
insert into Student(id, name, age) values (1316, 'Austin', 19);
insert into Student(id, name, age) values (1911, 'Gabriel', 19);
insert into Student(id, name, age) values (1501, 'Jessica', 19);
insert into Student(id, name, age) values (1304, 'Jordan', 20);
insert into Student(id, name, age) values (1025, 'John', 20);
insert into Student(id, name, age) values (1934, 'Kyle', 20);
insert into Student(id, name, age) values (1661, 'Logan', 20);

insert into Friend(id1, id2) values (1510, 1381);
insert into Friend(id1, id2) values (1510, 1689);
insert into Friend(id1, id2) values (1689, 1709);
insert into Friend(id1, id2) values (1381, 1247);
insert into Friend(id1, id2) values (1709, 1247);
insert into Friend(id1, id2) values (1689, 1782);
insert into Friend(id1, id2) values (1782, 1468);
insert into Friend(id1, id2) values (1782, 1316);
insert into Friend(id1, id2) values (1782, 1304);
insert into Friend(id1, id2) values (1468, 1101);
insert into Friend(id1, id2) values (1468, 1641);
insert into Friend(id1, id2) values (1101, 1641);
insert into Friend(id1, id2) values (1247, 1911);
insert into Friend(id1, id2) values (1247, 1501);
insert into Friend(id1, id2) values (1911, 1501);
insert into Friend(id1, id2) values (1501, 1934);
insert into Friend(id1, id2) values (1316, 1934);
insert into Friend(id1, id2) values (1934, 1304);
insert into Friend(id1, id2) values (1304, 1661);
insert into Friend(id1, id2) values (1661, 1025);
insert into Friend(id1, id2) select id2, id1 from Friend;

insert into Likes(id1, id2) values(1689, 1709);
insert into Likes(id1, id2) values(1709, 1689);
insert into Likes(id1, id2) values(1782, 1709);
insert into Likes(id1, id2) values(1911, 1247);
insert into Likes(id1, id2) values(1247, 1468);
insert into Likes(id1, id2) values(1641, 1468);
insert into Likes(id1, id2) values(1316, 1304);
insert into Likes(id1, id2) values(1501, 1934);
insert into Likes(id1, id2) values(1934, 1501);
insert into Likes(id1, id2) values(1025, 1101);

