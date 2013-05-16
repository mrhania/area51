
CREATE TABLE client(
	id integer primary key autoincrement,
	name text, -- nazwisko
	phone integer, -- numer telefonu
	email text, -- adres e-mail
	adress text -- adres domowy
);

CREATE TABLE car(
	id integer primary key autoincrement,
	brand text, -- marka
	capacity integer, -- pojemność silnika
	type text, -- typ silnika
	maxp integer -- maksymalna ilość osób
);

CREATE TABLE booking(
	client_id integer,
	car_id integer,
	date_bg integer, -- data rozpoczęcia
	date_ed integer -- data zwrotu
);

CREATE TABLE hiring(
	client_id integer,
	car_id integer,
	date_bg integer,
	date_ed integer
	notes text
);

INSERT INTO
	client (name, phone, email, adress)
VALUES
	("Spammer", 692127752, "spam@example.com", "---")
;
INSERT INTO
	client (name, phone, email, adress)
VALUES
	("Abacki",     129319321, "abacki@abc.com",   "---")
;
INSERT INTO
	client (name, phone, email, adress)
VALUES
	("Popek",      997997997, "popek@firma.jp",   "---")
;

INSERT INTO
	car (brand, capacity, type, maxp)
VALUES
	("Batmobil", 100, "???", 1)
;
