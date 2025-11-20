drop table `forhandler`;
CREATE TABLE forhandler (
    forhandler_id INTEGER PRIMARY KEY AUTO_INCREMENT,
    adresse varchar(200),
    cvr varchar(200),
    navn VARCHAR(100)
);

INSERT INTO forhandler (adresse, cvr, navn)
SELECT distinct adresse, cvr , forhandler
FROM autocamper;