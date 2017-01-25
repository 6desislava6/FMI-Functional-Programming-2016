BEGIN TRANSACTION;
CREATE TABLE Questions
(
ID INTEGER PRIMARY KEY AUTOINCREMENT,
question TEXT NOT NULL
);
INSERT INTO `Questions` VALUES (1,'');
INSERT INTO `Questions` VALUES (2,'Does it bite?');
INSERT INTO `Questions` VALUES (3,'Would you find it on a farm?');
INSERT INTO `Questions` VALUES (4,'Can you pet it?');
INSERT INTO `Questions` VALUES (5,'Is it round?');
INSERT INTO `Questions` VALUES (6,'Is it fury?');
INSERT INTO `Questions` VALUES (7,'Does it have seeds?');
INSERT INTO `Questions` VALUES (8,'Is it bigger than a duck?');
INSERT INTO `Questions` VALUES (9,'Is it found in salad bars?');
INSERT INTO `Questions` VALUES (10,'Can you eat it?');
INSERT INTO `Questions` VALUES (11,'Is it loyal?');
CREATE TABLE Answers (
    ID INTEGER PRIMARY KEY AUTOINCREMENT,
    answer INTEGER,
    animal_id INTEGER,
    question_id INTEGER,
    FOREIGN KEY(animal_id) REFERENCES Animals(ID),
    FOREIGN KEY(question_id) REFERENCES Questions(ID)
);
INSERT INTO `Answers` VALUES (1,1,1,2);
INSERT INTO `Answers` VALUES (2,1,2,2);
INSERT INTO `Answers` VALUES (3,1,3,2);
INSERT INTO `Answers` VALUES (4,0,4,2);
INSERT INTO `Answers` VALUES (5,1,5,2);
INSERT INTO `Answers` VALUES (6,0,6,2);
INSERT INTO `Answers` VALUES (7,0,7,2);
INSERT INTO `Answers` VALUES (8,1,8,2);
INSERT INTO `Answers` VALUES (9,0,9,2);
INSERT INTO `Answers` VALUES (10,0,1,3);
INSERT INTO `Answers` VALUES (11,0,2,3);
INSERT INTO `Answers` VALUES (12,1,3,3);
INSERT INTO `Answers` VALUES (13,0,4,3);
INSERT INTO `Answers` VALUES (14,1,5,3);
INSERT INTO `Answers` VALUES (15,1,6,3);
INSERT INTO `Answers` VALUES (16,0,7,3);
INSERT INTO `Answers` VALUES (17,0,8,3);
INSERT INTO `Answers` VALUES (18,0,9,3);
INSERT INTO `Answers` VALUES (19,0,1,4);
INSERT INTO `Answers` VALUES (20,0,2,4);
INSERT INTO `Answers` VALUES (21,1,3,4);
INSERT INTO `Answers` VALUES (22,0,4,4);
INSERT INTO `Answers` VALUES (23,1,5,4);
INSERT INTO `Answers` VALUES (24,1,6,4);
INSERT INTO `Answers` VALUES (25,0,7,4);
INSERT INTO `Answers` VALUES (26,0,8,4);
INSERT INTO `Answers` VALUES (27,0,9,4);
CREATE TABLE Animals
(
ID INTEGER PRIMARY KEY AUTOINCREMENT,
name varchar(255) NOT NULL
);
INSERT INTO `Animals` VALUES (1,'tiger');
INSERT INTO `Animals` VALUES (2,'hawaiian lion');
INSERT INTO `Animals` VALUES (3,'dog');
INSERT INTO `Animals` VALUES (4,'owl');
INSERT INTO `Animals` VALUES (5,'cat');
INSERT INTO `Animals` VALUES (6,'chicken');
INSERT INTO `Animals` VALUES (7,'whale');
INSERT INTO `Animals` VALUES (8,'lion');
INSERT INTO `Animals` VALUES (9,'eagle');
COMMIT;
